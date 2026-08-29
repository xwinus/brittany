{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.ConstructorComments
  ( normalizeConstructorComments
  ) where

import qualified Data.Char                               as Char
import           Data.Data                                ( Data )
import qualified Data.Generics                           as SYB
import           Data.Kind                                ( Type )
import qualified Data.Map                                as Map
import           GHC                                      ( GenLocated(L)
                                                          , getLoc
                                                          , unLoc
                                                          )
import           GHC.Hs                                   ( DataDefnCons(..)
                                                          , HsDataDefn(..)
                                                          , LHsDecl
                                                          , TyClDecl(..)
                                                          )
import           GHC.Parser.Annotation                    ( getLocA )
import qualified GHC.Types.SrcLoc                        as SrcLoc
import           Language.Haskell.Brittany.Internal.ExactPrintCompat
import           Language.Haskell.Brittany.Internal.Prelude

type BoundaryNode :: Type
data BoundaryNode = BoundaryNode
  { boundaryKey :: AnnKey
  , boundaryEnd :: (Int, Int)
  }

normalizeConstructorComments :: [LHsDecl GhcPs] -> Anns -> Anns
normalizeConstructorComments declarations annotations =
  foldl moveGroup annotations $ constructorBoundaryGroups declarations
 where
  moveGroup currentAnnotations nodes =
    foldl movePostDocs currentAnnotations $ zip nodes $ drop 1 nodes

movePostDocs :: Anns -> (BoundaryNode, BoundaryNode) -> Anns
movePostDocs annotations (previousNode, currentNode) =
  case Map.lookup (boundaryKey currentNode) annotations of
    Nothing -> annotations
    Just currentAnnotation ->
      let (postDocs, remainingPriors) =
            takePostDocRun $ annPriorComments currentAnnotation
      in
        if null postDocs
          then annotations
          else
            Map.insert
                (boundaryKey previousNode)
                ( appendFollowingComments (boundaryEnd previousNode) postDocs
                $ Map.findWithDefault
                    emptyAnnotation
                    (boundaryKey previousNode)
                    annotations
                )
              $ Map.insert
                  (boundaryKey currentNode)
                  currentAnnotation { annPriorComments = remainingPriors }
                  annotations

emptyAnnotation :: Annotation
emptyAnnotation =
  Ann
    { annCapturedSpan      = Nothing
    , annSortKey           = Nothing
    , annsDP               = []
    , annFollowingComments = []
    , annPriorComments     = []
    , annEntryDelta        = DP (0, 0)
    }

appendFollowingComments
  :: (Int, Int) -> [(Comment, DeltaPos)] -> Annotation -> Annotation
appendFollowingComments nodeEnd postDocs annotation =
  annotation
    { annFollowingComments =
        rebaseComments nodeEnd $ annFollowingComments annotation ++ postDocs
    }

takePostDocRun
  :: [(Comment, DeltaPos)] -> ([(Comment, DeltaPos)], [(Comment, DeltaPos)])
takePostDocRun commentEntries = case commentEntries of
  firstEntry@(sourceComment, _) : rest | isHaddockPostDoc sourceComment ->
    let (continuations, remaining) = takeContinuations sourceComment rest
    in  (firstEntry : continuations, remaining)
  _ -> ([], commentEntries)

takeContinuations
  :: Comment
  -> [(Comment, DeltaPos)]
  -> ([(Comment, DeltaPos)], [(Comment, DeltaPos)])
takeContinuations previous commentEntries = case commentEntries of
  current@(sourceComment, _) : rest
    | not (isLeadingHaddock sourceComment)
    , commentsAreAdjacent previous sourceComment
    -> let (continuations, remaining) = takeContinuations sourceComment rest
       in  (current : continuations, remaining)
  _ -> ([], commentEntries)

commentsAreAdjacent :: Comment -> Comment -> Bool
commentsAreAdjacent previous current =
  case
      ( srcSpanToRealSpan $ commentIdentifier previous
      , srcSpanToRealSpan $ commentIdentifier current
      )
    of
      (Just previousSpan, Just currentSpan) ->
        SrcLoc.srcSpanStartLine currentSpan
          <= SrcLoc.srcSpanEndLine previousSpan
          +  1
      _ -> False

isHaddockPostDoc :: Comment -> Bool
isHaddockPostDoc = hasHaddockMarker '^'

isLeadingHaddock :: Comment -> Bool
isLeadingHaddock = hasHaddockMarker '|'

hasHaddockMarker :: Char -> Comment -> Bool
hasHaddockMarker marker sourceComment =
  case dropWhile Char.isSpace $ commentContents sourceComment of
    '-' : '-' : rest -> startsWithMarker rest
    '{' : '-' : rest -> startsWithMarker rest
    _                -> False
 where
  startsWithMarker = (== Just marker) . listToMaybe . dropWhile Char.isSpace

rebaseComments :: (Int, Int) -> [(Comment, DeltaPos)] -> [(Comment, DeltaPos)]
rebaseComments initialPosition commentEntries = snd
  $ mapAccumL rebase initialPosition commentEntries
 where
  rebase previousPosition (sourceComment, _) =
    case srcSpanToRealSpan $ commentIdentifier sourceComment of
      Nothing -> (previousPosition, (sourceComment, DP (0, 0)))
      Just span' ->
        ( spanEnd span'
        , (sourceComment, positionDelta previousPosition $ spanStart span')
        )

positionDelta :: (Int, Int) -> (Int, Int) -> DeltaPos
positionDelta (previousLine, previousColumn) (currentLine, currentColumn)
  | currentLine == previousLine = DP (0, currentColumn - previousColumn)
  | otherwise = DP (currentLine - previousLine, currentColumn - 1)

constructorBoundaryGroups :: [LHsDecl GhcPs] -> [[BoundaryNode]]
constructorBoundaryGroups = SYB.everything (++) query
 where
  query :: SYB.GenericQ [[BoundaryNode]]
  query = const [] `SYB.extQ` fromDataDeclaration
  fromDataDeclaration :: TyClDecl GhcPs -> [[BoundaryNode]]
  fromDataDeclaration = \case
    DataDecl
      _
      _
      _
      _
      HsDataDefn { dd_cons = constructors, dd_derivs = derivings } ->
        [ fmap constructorNode (constructorList constructors)
            ++ fmap derivingNode derivings
        ]
    _ -> []
  constructorList = \case
    NewTypeCon constructor      -> [constructor]
    DataTypeCons _ constructors -> constructors
  constructorNode constructor =
    locatedNode (L (getLocA constructor) $ unLoc constructor)
  derivingNode derivingClause =
    locatedNode (L (getLocA derivingClause) $ unLoc derivingClause)

locatedNode :: Data a => GenLocated SrcLoc.SrcSpan a -> BoundaryNode
locatedNode node =
  BoundaryNode
    { boundaryKey = mkAnnKey node
    , boundaryEnd = maybe (0, 0) spanEnd $ srcSpanToRealSpan $ getLoc node
    }

spanStart :: SrcLoc.RealSrcSpan -> (Int, Int)
spanStart span' = (SrcLoc.srcSpanStartLine span', SrcLoc.srcSpanStartCol span')

spanEnd :: SrcLoc.RealSrcSpan -> (Int, Int)
spanEnd span' = (SrcLoc.srcSpanEndLine span', SrcLoc.srcSpanEndCol span')
