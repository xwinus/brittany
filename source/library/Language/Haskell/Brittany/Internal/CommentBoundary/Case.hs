{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.CommentBoundary.Case
  ( caseAlternativeBoundary
  , materializeCaseComments
  ) where

import qualified Data.Generics                           as Generics
import           Data.Kind                               ( Type )
import qualified Data.List                               as List
import qualified Data.Map                                as Map
import qualified Data.Set                                as Set
import           GHC                                      ( GenLocated(L)
                                                          , HsModule
                                                          )
import           GHC.Hs                                   ( EpAnnHsCase(..)
                                                          , HsExpr(..)
                                                          , MatchGroup(..)
                                                          )
import           GHC.Parser.Annotation                    ( getEpTokenSrcSpan
                                                          , getLocA
                                                          )
import qualified GHC.Types.SrcLoc                        as SrcLoc
import           Language.Haskell.Brittany.Internal.ExactPrintCompat
import           Language.Haskell.Brittany.Internal.Prelude
import           Language.Haskell.Brittany.Internal.SourceComment.Types

type CaseRegion :: Type
data CaseRegion = CaseRegion
  { regionMatchGroupOwner :: AnnKey
  , regionOfSpan          :: SrcLoc.RealSrcSpan
  , regionFirstMatchSpan  :: SrcLoc.RealSrcSpan
  }

materializeCaseComments :: HsModule GhcPs -> Anns -> Anns
materializeCaseComments module' annotations =
  foldl relocateComments annotations $ caseRegions module'

caseAlternativeBoundary
  :: HsModule GhcPs -> SrcLoc.RealSrcSpan -> Maybe CommentBoundaryId
caseAlternativeBoundary module' commentSpan = do
  (index, _) <-
    List.find (commentWithinRegion commentSpan . snd) $ zip [0 ..] $ caseRegions
      module'
  pure $ CommentBoundaryId (CaseAlternativeBoundaryPath index) BeforeBoundary

caseRegions :: HsModule GhcPs -> [CaseRegion]
caseRegions =
  List.sortOn regionPosition . Generics.everything (++) caseRegionQuery

caseRegionQuery :: Generics.GenericQ [CaseRegion]
caseRegionQuery = Generics.mkQ [] caseRegion

caseRegion :: HsExpr GhcPs -> [CaseRegion]
caseRegion = \case
  HsCase annotations _ (MG _ matches@(L _ (firstMatch : _))) ->
    maybeToList $ do
      ofSpan <- srcSpanToRealSpan $ getEpTokenSrcSpan $ hsCaseAnnOf annotations
      firstMatchSpan <- srcSpanToRealSpan $ getLocA firstMatch
      pure
        CaseRegion
          { regionMatchGroupOwner = mkNamedAnnKey "MatchGroup" (getLocA matches)
          , regionOfSpan          = ofSpan
          , regionFirstMatchSpan  = firstMatchSpan
          }
  _ -> []

relocateComments :: Anns -> CaseRegion -> Anns
relocateComments annotations region
  | null moved
  = annotations
  | otherwise
  = Map.alter
      (Just . addPriorComments region moved . fromMaybe emptyAnnotation)
      (regionMatchGroupOwner region)
    $ removeComments moved annotations
 where
  moved = commentsInRegion region annotations

addPriorComments :: CaseRegion -> [Comment] -> Annotation -> Annotation
addPriorComments region moved annotation =
  annotation
    { annPriorComments =
        rebaseComments region moved ++ annPriorComments annotation
    }

commentsInRegion :: CaseRegion -> Anns -> [Comment]
commentsInRegion region annotations =
  distinctComments
    $ List.sortOn commentPosition
    $ filter (commentWithinRegion' region)
    $ List.concatMap annotationComments
    $ Map.elems annotations

annotationComments :: Annotation -> [Comment]
annotationComments annotation =
  fmap fst (annPriorComments annotation)
    ++ fmap fst (annFollowingComments annotation)
    ++ [ comment | (AnnComment comment, _) <- annsDP annotation ]

removeComments :: [Comment] -> Anns -> Anns
removeComments comments = Map.map remove
 where
  keys = Set.fromList $ SourceCommentKey . commentIdentifier <$> comments
  keep comment =
    Set.notMember (SourceCommentKey $ commentIdentifier comment) keys
  remove annotation =
    annotation
      { annPriorComments     = filter (keep . fst) $ annPriorComments annotation
      , annFollowingComments =
          filter (keep . fst) $ annFollowingComments annotation
      , annsDP               = filter keepKeyword $ annsDP annotation
      }
  keepKeyword = \case
    (AnnComment comment, _) -> keep comment
    _ -> True

rebaseComments :: CaseRegion -> [Comment] -> [(Comment, DeltaPos)]
rebaseComments region = snd . mapAccumL rebase (spanEnd $ regionOfSpan region)
 where
  rebase previous comment =
    ( commentEnd comment
    , (comment, positionDelta previous $ commentStart comment)
    )

distinctComments :: [Comment] -> [Comment]
distinctComments = go Set.empty
 where
  go _ [] = []
  go seen (comment : remaining)
    | Set.member key seen = go seen remaining
    | otherwise           = comment : go (Set.insert key seen) remaining
   where
    key = SourceCommentKey $ commentIdentifier comment

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

commentWithinRegion' :: CaseRegion -> Comment -> Bool
commentWithinRegion' region comment = fromMaybe False $ do
  commentSpan <- srcSpanToRealSpan $ commentIdentifier comment
  pure $ commentWithinRegion commentSpan region

commentWithinRegion :: SrcLoc.RealSrcSpan -> CaseRegion -> Bool
commentWithinRegion commentSpan region =
  spanStart commentSpan
    >= spanEnd (regionOfSpan region)
    && spanEnd commentSpan
    <= spanStart (regionFirstMatchSpan region)

commentPosition :: Comment -> (String, Int, Int, Int, Int)
commentPosition comment = case srcSpanToRealSpan $ commentIdentifier comment of
  Nothing -> (show $ commentIdentifier comment, 0, 0, 0, 0)
  Just span' ->
    ( show $ SrcLoc.srcSpanFile span'
    , SrcLoc.srcSpanStartLine span'
    , SrcLoc.srcSpanStartCol span'
    , SrcLoc.srcSpanEndLine span'
    , SrcLoc.srcSpanEndCol span'
    )

regionPosition :: CaseRegion -> ((Int, Int), (Int, Int))
regionPosition region =
  (spanStart $ regionOfSpan region, spanStart $ regionFirstMatchSpan region)

commentStart :: Comment -> (Int, Int)
commentStart comment =
  maybe (0, 0) spanStart $ srcSpanToRealSpan $ commentIdentifier comment

commentEnd :: Comment -> (Int, Int)
commentEnd comment =
  maybe (0, 0) spanEnd $ srcSpanToRealSpan $ commentIdentifier comment

positionDelta :: (Int, Int) -> (Int, Int) -> DeltaPos
positionDelta (previousLine, previousColumn) (currentLine, currentColumn)
  | currentLine == previousLine = DP (0, currentColumn - previousColumn)
  | otherwise = DP (currentLine - previousLine, currentColumn - 1)

spanStart :: SrcLoc.RealSrcSpan -> (Int, Int)
spanStart span' = (SrcLoc.srcSpanStartLine span', SrcLoc.srcSpanStartCol span')

spanEnd :: SrcLoc.RealSrcSpan -> (Int, Int)
spanEnd span' = (SrcLoc.srcSpanEndLine span', SrcLoc.srcSpanEndCol span')
