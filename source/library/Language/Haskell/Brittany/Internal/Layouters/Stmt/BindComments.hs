{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Stmt.BindComments
  ( layoutBindArrowRhs ) where

import Data.Semigroup (Last(..))
import qualified Data.List as List
import qualified Data.Map as Map
import GHC.Hs (Stmt, LHsExpr)
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.CommentBoundary.BindArrow
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.CommentIR (planSourceCommentWithDelta)
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Types

-- Explicit comments reserve their keys before generic annotation lowering, so
-- even a coarse preceding-pattern annotation cannot emit them before the arrow.
layoutBindArrowRhs
  :: Stmt GhcPs (LHsExpr GhcPs)
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM (Maybe BriDocNumbered)
layoutBindArrowRhs statement rhs = do
  plan <- mAsk
  case bindArrowRegion statement of
    Nothing -> pure Nothing
    Just region -> do
      let select predicate = List.sortOn (position . sourceCommentSpan)
            $ filter (predicate region . sourceCommentSpan)
            $ Map.elems $ commentPlanSources plan
          before = select preArrowComment
          after = select postArrowComment
          arrow = case before of
            [] -> docLitS "<-"
            _ -> docSetBaseY $ gapBody False plan (bindPatternSpan region)
              (bindArrowSpan region) before $ docLitS "<-"
      columns <- mAsk <&> _conf_layout .> _lconfig_cols .> confUnpack
      case after of
        [] | null before -> pure Nothing
           | otherwise -> Just <$> docAlt
               [ docColumnsLimit columns $ docSeq [appSep arrow, rhs]
               , docParIndented BrIndentRegular arrow rhs
               ]
        first : _ -> do
          let inline = startLine first == SrcLoc.srcSpanEndLine (bindArrowSpan region)
              body forceOwn = gapBody forceOwn plan (bindArrowSpan region) (bindRhsSpan region) after rhs
              attached = docSeq [appSep arrow, docSetBaseY $ body False]
              broken = docParIndented BrIndentRegular arrow $ docSetBaseY $ body inline
          Just <$> if inline
            -- Check hanging continuations at the actual arrow column rather
            -- than accepting their paragraph width at the enclosing do base.
            then docAlt [docColumnsLimit columns attached, broken]
            else broken

 where
  position span' = (SrcLoc.srcSpanStartLine span', SrcLoc.srcSpanStartCol span')
  startLine = SrcLoc.srcSpanStartLine . sourceCommentSpan
  endLine = SrcLoc.srcSpanEndLine . sourceCommentSpan
  column = SrcLoc.srcSpanStartCol . sourceCommentSpan
  gapBody forceOwn plan previous following comments next =
    let gaps = zip comments $ case comments of
          [] -> []
          first : _ -> max (if forceOwn then 1 else 0) (startLine first - SrcLoc.srcSpanEndLine previous)
            : zipWith (\left right -> startLine right - endLine left) comments (drop 1 comments)
        layoutRun run = case run of
          [] -> []
          _ ->
            -- A continuation originally below a long binder may start left of
            -- its inline seed. Rebase that run at the RHS, retaining its offsets.
            let baseColumn = minimum $ column . fst <$> run
            in map (\entry@(source, gap) ->
                 let first = Just (sourceCommentKey source) == (sourceCommentKey <$> listToMaybe comments)
                     comment = layoutComment plan baseColumn (forceOwn && first) entry
                 in if gap == 0 && not first
                   then docSeq [docSeparator, comment]
                   else comment) run
        commentDoc = docSeq $ List.concatMap layoutRun $ runs gaps
    in case reverse comments of
      [] -> next
      lastComment : _ ->
        let nextGap = SrcLoc.srcSpanStartLine following - endLine lastComment
        in if nextGap == 0
          then docSeq [commentDoc, docSeparator, next]
          else docPar commentDoc $ docLines $
            replicate (max 0 $ nextGap - 1) docBlankLine ++ [next]
  runs [] = []
  runs (first : rest) =
    let (continuing, later) = break ((> 1) . snd) rest
    in (first : continuing) : runs later
  layoutComment plan baseColumn forceFirst (source, lineDelta) =
    case planSourceCommentWithDelta plan source lineDelta 1 of
      Left commentError -> do
        mTell [ErrorCommentPlan $ show commentError]
        docEmpty
      Right planned -> allocateNode $ BDFComment planned
        { plannedCommentPlacement = (plannedCommentPlacement planned)
            { placementAnchor = BeforeNode
            , placementLineRelation = if lineDelta == 0 then InlineComment else CommentOwnLine
            }
        , plannedCommentIndentPolicy = if lineDelta == 0 then RenderedAnchorIndent else OwnerRelativeIndent
        , plannedCommentLineDelta = lineDelta
        , plannedCommentColumnDelta = if lineDelta == 0 || forceFirst then 0 else column source - baseColumn
        }
