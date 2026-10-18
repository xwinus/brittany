{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Decl.GuardComments
  ( attachGuardLeadingComments
  , hasGuardLeadingComments
  , withGuardLeadingComments
  ) where

import qualified Data.List as List
import qualified Data.Map as Map
import GHC.Hs (LGRHS, LHsExpr)
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.CommentIR (planSourceCommentWithDelta)
import Language.Haskell.Brittany.Internal.ExactPrintCompat
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.IE (toL)
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Types

-- Reserve these comments before generic annotations can attach them to a head
-- pattern or a preceding RHS. The guard renderer emits them before its marker.
attachGuardLeadingComments
  :: LGRHS GhcPs (LHsExpr GhcPs)
  -> [BriDocNumbered]
  -> ToBriDocM [BriDocNumbered]
attachGuardLeadingComments _ [] = pure []
attachGuardLeadingComments clause (first : remaining) = do
  plan <- mAsk
  annotations <- (mAsk :: ToBriDocM Anns)
  let owner = NodeId $ mkAnnKey $ toL clause
      sources = List.sortOn (position . sourceCommentSpan)
        [ source
        | (key, source) <- Map.toList $ commentPlanSources plan
        , Just placement <- [Map.lookup key $ commentPlanPlacements plan]
        , placementOwner placement == owner
        , Just boundary <- [Map.lookup key $ commentPlanBoundaries plan]
        , GuardBoundaryPath{} <- [commentBoundaryPath boundary]
        ]
      priorGaps = Map.fromList
        [ (SourceCommentKey $ commentIdentifier comment, lineDelta)
        | annotation <- maybeToList $ Map.lookup (mkAnnKey $ toL clause) annotations
        , (comment, DP (lineDelta, _)) <- annPriorComments annotation
        ]
      firstGap = case sources of
        source : _ -> Map.findWithDefault 1 (sourceCommentKey source) priorGaps
        [] -> 1
      gaps = zip sources $ firstGap : zipWith
        (\left right -> startLine right - endLine left) sources (drop 1 sources)
      layoutRun run = do
        let column = minimum $ startColumn . fst <$> run
        forM run $ \(source, gap) -> case
            planSourceCommentWithDelta plan source gap 0 of
          Left commentError -> do
            mTell [ErrorCommentPlan $ show commentError]
            docEmpty
          Right planned -> allocateNode $ BDFComment planned
            { plannedCommentIndentPolicy = OwnerRelativeIndent
            , plannedCommentColumnDelta = startColumn source - column
            }
  case sources of
    [] -> pure $ first : remaining
    _ -> do
      comments <- List.concat <$> mapM layoutRun (commentRuns gaps)
      annotated <- docSeq $ pure <$> comments ++ [first]
      pure $ annotated : remaining
 where
  position span' = (SrcLoc.srcSpanStartLine span', SrcLoc.srcSpanStartCol span')
  startLine = SrcLoc.srcSpanStartLine . sourceCommentSpan
  endLine = SrcLoc.srcSpanEndLine . sourceCommentSpan
  startColumn = SrcLoc.srcSpanStartCol . sourceCommentSpan
  commentRuns [] = []
  commentRuns (first : rest) =
    let (continuing, later) = break ((> 1) . snd) rest
    in (first : continuing) : commentRuns later

-- Keep the prefix outside the clause alternatives so comments cannot force a
-- fitting guard and its RHS onto separate lines.
hasGuardLeadingComments :: BriDocNumbered -> Bool
hasGuardLeadingComments (_, BDFSeq (first : _)) = isGuardComment first
hasGuardLeadingComments _ = False

withGuardLeadingComments
  :: BriDocNumbered
  -> (BriDocNumbered -> ToBriDocM BriDocNumbered)
  -> ToBriDocM BriDocNumbered
withGuardLeadingComments document build = case document of
  (_, BDFSeq children)
    | (comments@(_ : _), [content]) <- List.span isGuardComment children ->
        docSeq $ (pure <$> comments) ++ [build content]
  _ -> build document

isGuardComment :: BriDocNumbered -> Bool
isGuardComment (_, BDFComment planned) = case
    commentBoundaryPath $ plannedCommentBoundary planned of
  GuardBoundaryPath{} -> True
  _ -> False
isGuardComment _ = False
