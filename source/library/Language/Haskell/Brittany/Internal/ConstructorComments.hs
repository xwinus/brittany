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
import qualified Data.List                               as List
import qualified Data.Map                                as Map
import qualified Data.Maybe                              as Maybe
import qualified Data.Set                                as Set
import           GHC                                      ( GenLocated(L)
                                                          , getLoc
                                                          , unLoc
                                                          )
import           GHC.Hs                                   ( DataDefnCons(..)
                                                          , HsDecl(..)
                                                          , HsDataDefn(..)
                                                          , LHsDecl
                                                          , TyClDecl(..)
                                                          )
import           GHC.Parser.Annotation                    ( getLocA )
import qualified GHC.Types.SrcLoc                        as SrcLoc
import           Language.Haskell.Brittany.Internal.CommentBoundary.Trailing
                                                          ( plainLineCommentText
                                                          , takeTrailingContinuationRun
                                                          )
import           Language.Haskell.Brittany.Internal.ExactPrintCompat
import           Language.Haskell.Brittany.Internal.Prelude
import           Language.Haskell.Brittany.Internal.SourceComment.Types
                                                          ( SourceCommentKey(..) )

type BoundaryNode :: Type
data BoundaryNode = BoundaryNode
  { boundaryKey :: AnnKey
  , boundaryEnd :: (Int, Int)
  }

normalizeConstructorComments :: [LHsDecl GhcPs] -> Anns -> Anns
normalizeConstructorComments declarations annotations =
  foldl moveOrdinaryGroup postDocAnnotations
    $ terminalConstructorBoundaryGroups declarations True
 where
  postDocAnnotations = foldl moveGroup annotations
    $ constructorBoundaryGroups declarations
    ++ terminalConstructorBoundaryGroups declarations False
  moveGroup currentAnnotations nodes =
    foldl movePostDocs currentAnnotations $ zip nodes $ drop 1 nodes
  moveOrdinaryGroup currentAnnotations nodes =
    foldl moveOrdinaryRun currentAnnotations
      $ zip nodes $ (Just <$> drop 1 nodes) ++ [Nothing]

moveOrdinaryRun :: Anns -> (BoundaryNode, Maybe BoundaryNode) -> Anns
moveOrdinaryRun annotations (constructor, nextNode) =
  fromMaybe annotations $ do
    let key@(AnnKey _ constructorName) = boundaryKey constructor
    guard $ unConName constructorName `elem` ["ConDeclH98", "ConDeclGADT"]
    constructorSpan <- annKeyRealSpan key
    let candidates = List.sortOn (spanStart . snd)
          [ (comment, span')
          | annotation <- Map.elems annotations
          , comment <- annotationComments annotation
          , Just span' <- [srcSpanToRealSpan $ commentIdentifier comment]
          , SrcLoc.srcSpanFile span' == SrcLoc.srcSpanFile constructorSpan
          , spanStart span' >= spanEnd constructorSpan
          , maybe True (spanStart span' <)
              (nextNode >>= annKeyRealSpan . boundaryKey >>= pure . spanStart)
          ]
        (sameLine, following) = List.partition
          ((== SrcLoc.srcSpanEndLine constructorSpan) . SrcLoc.srcSpanStartLine . snd)
          candidates
        seeds = filter (\(comment, span') ->
          plainLineCommentText (commentContents comment)
            && SrcLoc.srcSpanStartLine span' == SrcLoc.srcSpanEndLine span') sameLine
        (continuations, _) = takeTrailingContinuationRun constructorSpan
          (fst <$> seeds) (fst <$> following)
        moved = (fst <$> seeds) ++ continuations
        movedKeys = Set.fromList $ commentKey <$> moved
        occurrences = Map.fromListWith (+)
          [(commentKey comment, 1 :: Int) | (comment, _) <- candidates]
    guard $ not $ null continuations
    -- A repeated occurrence must remain visible to comment-plan validation.
    guard $ all (\comment -> Map.lookup (commentKey comment) occurrences == Just 1) moved
    let removed = Map.mapWithKey (removeRun movedKeys) annotations
        previous = Map.findWithDefault emptyAnnotation key removed
        merged = List.sortOn (commentKey . fst)
          $ annFollowingComments previous ++ [(comment, DP (0, 0)) | comment <- moved]
    pure $ Map.insert key
      previous { annFollowingComments = rebaseComments (spanEnd constructorSpan) merged }
      removed
 where
  commentKey = SourceCommentKey . commentIdentifier
  annotationComments annotation =
    (fst <$> annPriorComments annotation)
      ++ (fst <$> annFollowingComments annotation)
      ++ [comment | (AnnComment comment, _) <- annsDP annotation]
  removeRun keys key annotation =
    let keep = (`Set.notMember` keys) . commentKey
        priors = filter (keep . fst) $ annPriorComments annotation
        nextDeclaration = maybe False ((== key) . boundaryKey) nextNode
          && case key of
            AnnKey _ name -> unConName name `elem` ["ValD", "SigD", "TyClD"]
    in annotation
      { annPriorComments = priors
      , annFollowingComments = filter (keep . fst) $ annFollowingComments annotation
      , annsDP = filter (\case
          (AnnComment comment, _) -> keep comment
          _ -> True) $ annsDP annotation
      , annEntryDelta = if nextDeclaration && null priors
            && not (null $ annPriorComments annotation)
          then DP (0, 0)
          else annEntryDelta annotation
      }

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

terminalConstructorBoundaryGroups :: [LHsDecl GhcPs] -> Bool -> [[BoundaryNode]]
terminalConstructorBoundaryGroups declarations includeFinal = Maybe.mapMaybe boundaryGroup
  $ zip declarations $ (Just <$> drop 1 declarations) ++ [Nothing | includeFinal]
 where
  boundaryGroup :: (LHsDecl GhcPs, Maybe (LHsDecl GhcPs)) -> Maybe [BoundaryNode]
  boundaryGroup (declaration, nextDeclaration) = case unLoc declaration of
    TyClD _ DataDecl
      { tcdDataDefn = HsDataDefn
          { dd_cons = constructors
          , dd_derivs = derivings
          }
      } -> Just
        $ fmap constructorNode (constructorList constructors)
        ++ fmap derivingNode derivings
        ++ maybe [] (\next -> [locatedNode $ L (getLocA next) $ unLoc next]) nextDeclaration
    _ -> Nothing
  constructorList = \case
    NewTypeCon constructor -> [constructor]
    DataTypeCons _ constructors -> constructors
  constructorNode constructor =
    locatedNode $ L (getLocA constructor) $ unLoc constructor
  derivingNode derivingClause =
    locatedNode $ L (getLocA derivingClause) $ unLoc derivingClause

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
