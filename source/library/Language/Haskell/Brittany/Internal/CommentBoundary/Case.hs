{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.CommentBoundary.Case
  ( CaseBoundaryIndex
  , buildCaseBoundaryIndex
  , caseAlternativeBoundary
  , caseAlternativeBoundaryFromIndex
  , caseAlternativeBoundaryWithPlacement
  , materializeCaseComments
  ) where

import qualified Data.Char                               as Char
import qualified Data.Generics                           as Generics
import           Data.Kind                               ( Type )
import qualified Data.List                               as List
import qualified Data.Map                                as Map
import qualified Data.Set                                as Set
import           GHC                                      ( GenLocated(L)
                                                          , HsModule
                                                          , unLoc
                                                          )
import           GHC.Hs                                   ( EpAnnHsCase(..)
                                                          , HsExpr(..)
                                                          , LHsExpr
                                                          , LMatch
                                                          , MatchGroup(..)
                                                          )
import           GHC.Parser.Annotation                    ( getEpTokenSrcSpan
                                                          , getLocA
                                                          )
import qualified GHC.Types.SrcLoc                        as SrcLoc
import           Language.Haskell.Brittany.Internal.ExactPrintCompat
import           Language.Haskell.Brittany.Internal.CommentBoundary.Trailing
                                                          ( plainLineCommentText )
import           Language.Haskell.Brittany.Internal.Prelude
import           Language.Haskell.Brittany.Internal.SourceComment.Types

type CaseRegion :: Type
data CaseRegion = CaseRegion
  { regionMatchGroupOwner :: AnnKey
  , regionOfSpan          :: SrcLoc.RealSrcSpan
  , regionFirstMatchSpan  :: SrcLoc.RealSrcSpan
  , regionFollowsMatch    :: Bool
  }

type CaseBoundaryIndex :: Type
newtype CaseBoundaryIndex = CaseBoundaryIndex [(Int, CaseRegion)]

buildCaseBoundaryIndex :: HsModule GhcPs -> CaseBoundaryIndex
buildCaseBoundaryIndex = CaseBoundaryIndex . zip [0 ..] . caseRegions

materializeCaseComments :: HsModule GhcPs -> Anns -> Anns
materializeCaseComments module' annotations =
  foldl relocateComments annotations $ caseRegions module'

caseAlternativeBoundary
  :: HsModule GhcPs -> SrcLoc.RealSrcSpan -> Maybe CommentBoundaryId
caseAlternativeBoundary module' =
  caseAlternativeBoundaryFromIndex $ buildCaseBoundaryIndex module'

caseAlternativeBoundaryFromIndex
  :: CaseBoundaryIndex -> SrcLoc.RealSrcSpan -> Maybe CommentBoundaryId
caseAlternativeBoundaryFromIndex index = caseBoundaryForPlacement index Nothing

caseAlternativeBoundaryWithPlacement
  :: CaseBoundaryIndex
  -> CommentPlacement
  -> SrcLoc.RealSrcSpan
  -> Maybe CommentBoundaryId
caseAlternativeBoundaryWithPlacement index placement =
  caseBoundaryForPlacement index $ Just placement

caseBoundaryForPlacement
  :: CaseBoundaryIndex
  -> Maybe CommentPlacement
  -> SrcLoc.RealSrcSpan
  -> Maybe CommentBoundaryId
caseBoundaryForPlacement (CaseBoundaryIndex indexedRegions) placement commentSpan = do
  (index, _) <-
    List.find (matchesRegion . snd) indexedRegions
  pure $ CommentBoundaryId (CaseAlternativeBoundaryPath index) BeforeBoundary
 where
  matchesRegion region = commentWithinRegion commentSpan region
    && (not (regionFollowsMatch region) || maybe False (ownsComment region) placement)
  ownsComment region current =
    placementOwner current == NodeId (regionMatchGroupOwner region)
      && placementAnchor current == BeforeNode
      && placementRole current == LeadingOrdinary
      && placementLineRelation current == CommentOwnLine

caseRegions :: HsModule GhcPs -> [CaseRegion]
caseRegions =
  List.sortOn regionPosition . Generics.everything (++) caseRegionQuery

caseRegionQuery :: Generics.GenericQ [CaseRegion]
caseRegionQuery = Generics.mkQ [] caseRegion

caseRegion :: HsExpr GhcPs -> [CaseRegion]
caseRegion = \case
  HsCase annotations _ (MG _ matches@(L _ matchList@(firstMatch : _))) ->
    maybeToList (do
      ofSpan <- srcSpanToRealSpan $ getEpTokenSrcSpan $ hsCaseAnnOf annotations
      firstMatchSpan <- srcSpanToRealSpan $ getLocA firstMatch
      pure
        CaseRegion
          { regionMatchGroupOwner = mkNamedAnnKey "MatchGroup" (getLocA matches)
          , regionOfSpan          = ofSpan
          , regionFirstMatchSpan  = firstMatchSpan
          , regionFollowsMatch    = False
          })
      ++ List.concatMap (maybeToList . betweenMatches)
        (zip matchList $ drop 1 matchList)
  _ -> []
 where
  betweenMatches
    :: (LMatch GhcPs (LHsExpr GhcPs), LMatch GhcPs (LHsExpr GhcPs))
    -> Maybe CaseRegion
  betweenMatches (previous, current) = do
    previousSpan <- srcSpanToRealSpan $ getLocA previous
    currentSpan <- srcSpanToRealSpan $ getLocA current
    pure CaseRegion
      { regionMatchGroupOwner = mkAnnKey $ L (getLocA current) $ unLoc current
      , regionOfSpan = previousSpan
      , regionFirstMatchSpan = currentSpan
      , regionFollowsMatch = True
      }

relocateComments :: Anns -> CaseRegion -> Anns
relocateComments annotations region
  | null moved
  = annotations
  | otherwise
  = Map.alter
      (Just . addPriorComments region regionComments moved . fromMaybe emptyAnnotation)
      (regionMatchGroupOwner region)
    $ removeComments moved annotations
 where
  regionComments = commentsInRegion region annotations
  moved = commentsToRelocate region regionComments

addPriorComments :: CaseRegion -> [Comment] -> [Comment] -> Annotation -> Annotation
addPriorComments region regionComments moved annotation =
  annotation
    { annPriorComments =
        if regionFollowsMatch region
          then rebaseFrom precedingPosition priorComments
          else rebaseComments region moved ++ annPriorComments annotation
    }
 where
  priorComments = List.sortOn commentPosition
    $ moved ++ fmap fst (annPriorComments annotation)
  precedingPosition = case priorComments of
    [] -> spanEnd $ regionOfSpan region
    first : _ -> foldl max (spanEnd $ regionOfSpan region)
      [ commentEnd earlier
      | earlier <- regionComments
      , commentEnd earlier <= commentStart first
      ]

commentsInRegion :: CaseRegion -> Anns -> [Comment]
commentsInRegion region annotations = distinctComments
    $ List.sortOn commentPosition
    $ filter (commentWithinRegion' region)
    $ List.concatMap annotationComments
    $ Map.elems annotations

commentsToRelocate :: CaseRegion -> [Comment] -> [Comment]
commentsToRelocate region comments
  | not $ regionFollowsMatch region = comments
  -- Earlier prose must not cross a protected run that keeps its original owner.
  | otherwise = List.concat $ reverse
      $ takeWhile ordinaryStandaloneRun $ reverse $ commentRuns comments
 where
  ordinaryStandaloneRun comments = not (null comments)
    && all ordinaryComment comments
    && all ((> SrcLoc.srcSpanEndLine (regionOfSpan region)) . fst . commentStart)
      comments
    && all ((== snd (commentStart $ head comments)) . snd . commentStart) comments

-- Keep a run intact when it starts inline, contains documentation or a diagram,
-- or uses deliberately different source indentation.
commentRuns :: [Comment] -> [[Comment]]
commentRuns [] = []
commentRuns (first : remaining) = collect [first] first remaining
 where
  collect run _ [] = [reverse run]
  collect run previous rest@(current : following)
    | fst (commentStart current) <= fst (commentEnd previous) + 1 =
        collect (current : run) current following
    | otherwise = reverse run : commentRuns rest

ordinaryComment :: Comment -> Bool
ordinaryComment comment = plainLineCommentText text
  && fst (commentStart comment) == fst (commentEnd comment)
  && not (any (`List.isInfixOf` text) ["->", "<-", "=>", "::", "$"])
  && case drop 2 $ dropWhile Char.isSpace text of
    ' ' : content -> prose content
    content -> prose content
 where
  text = commentContents comment
  prose [] = True
  prose (first : _) = Char.isAlphaNum first || first == '('

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
rebaseComments region = rebaseFrom $ spanEnd $ regionOfSpan region

rebaseFrom :: (Int, Int) -> [Comment] -> [(Comment, DeltaPos)]
rebaseFrom previousPosition = snd . mapAccumL rebase previousPosition
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
