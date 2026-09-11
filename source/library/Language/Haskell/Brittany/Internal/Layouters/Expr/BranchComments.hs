{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Expr.BranchComments
  ( reserveBranchSuffixWidth
  ) where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Semigroup (Last(..))
import qualified Data.Text as Text
import GHC.Hs (HsExpr)
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.Config.Types
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as ExactPrintCompat
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Types

-- The enclosing owner emits these comments after the final else expression.
-- Reserve their physical suffix width without moving or duplicating them.
reserveBranchSuffixWidth
  :: SrcLoc.Located (HsExpr GhcPs)
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
reserveBranchSuffixWidth expression document = case
    ExactPrintCompat.srcSpanToRealSpan $ SrcLoc.getLoc expression of
  Nothing -> document
  Just expressionSpan -> do
    plan <- mAsk
    columns <- mAsk <&> _conf_layout .> _lconfig_cols .> confUnpack
    let comments = List.sortOn (SrcLoc.srcSpanStartCol . sourceCommentSpan)
          $ filter (externalInlineSuffix plan expressionSpan)
          $ Map.elems $ commentPlanSources plan
        suffixWidth = sum $ snd $ List.mapAccumL commentWidth
          (SrcLoc.srcSpanEndCol expressionSpan) comments
    if suffixWidth == 0
      then document
      else docColumnsLimit (max 1 $ columns - suffixWidth) document

externalInlineSuffix :: CommentPlan -> SrcLoc.RealSrcSpan -> SourceComment -> Bool
externalInlineSuffix plan expressionSpan source = fromMaybe False $ do
  placement <- Map.lookup (sourceCommentKey source) $ commentPlanPlacements plan
  let NodeId owner = placementOwner placement
  ownerSpan <- ExactPrintCompat.annKeyRealSpan owner
  let sourceSpan = sourceCommentSpan source
  pure $ placementAnchor placement == AfterNode
    && placementLineRelation placement == InlineComment
    && SrcLoc.srcSpanFile ownerSpan == SrcLoc.srcSpanFile expressionSpan
    && SrcLoc.srcSpanFile sourceSpan == SrcLoc.srcSpanFile expressionSpan
    && spanStart ownerSpan <= spanStart expressionSpan
    && spanEnd ownerSpan == spanEnd expressionSpan
    && SrcLoc.srcSpanStartLine sourceSpan == SrcLoc.srcSpanEndLine expressionSpan
    && SrcLoc.srcSpanStartCol sourceSpan >= SrcLoc.srcSpanEndCol expressionSpan

commentWidth :: Int -> SourceComment -> (Int, Int)
commentWidth previousColumn source =
  let sourceSpan = sourceCommentSpan source
      gap = max 1 $ SrcLoc.srcSpanStartCol sourceSpan - previousColumn
      firstLine = Text.takeWhile (/= '\n') $ sourceCommentText source
  in (SrcLoc.srcSpanEndCol sourceSpan, gap + Text.length firstLine)

spanStart :: SrcLoc.RealSrcSpan -> (Int, Int)
spanStart sourceSpan =
  (SrcLoc.srcSpanStartLine sourceSpan, SrcLoc.srcSpanStartCol sourceSpan)

spanEnd :: SrcLoc.RealSrcSpan -> (Int, Int)
spanEnd sourceSpan =
  (SrcLoc.srcSpanEndLine sourceSpan, SrcLoc.srcSpanEndCol sourceSpan)
