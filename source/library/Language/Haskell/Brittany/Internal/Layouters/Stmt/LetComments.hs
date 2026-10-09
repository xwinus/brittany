{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Stmt.LetComments
  ( layoutLetComments ) where

import qualified Data.List as List
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.CommentIR (planSourceCommentWithDelta)
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Types

-- Relocated do-let comments keep canonical ownership and raw block text.
-- Marker offsets are relative to each run, rather than the original code column.
layoutLetComments :: [SourceComment] -> ToBriDocM BriDocNumbered
layoutLetComments comments = docSeq $ List.concatMap layoutRun $ runs withGaps
 where
  ordered = List.sortOn (\comment -> (startLine comment, column comment)) comments
  startLine = SrcLoc.srcSpanStartLine . sourceCommentSpan
  endLine = SrcLoc.srcSpanEndLine . sourceCommentSpan
  column = SrcLoc.srcSpanStartCol . sourceCommentSpan
  withGaps = zip ordered $ 1 : zipWith
    (\left right -> startLine right - endLine left) ordered (drop 1 ordered)
  runs [] = []
  runs (firstComment : rest) =
    let (continuing, later) = break ((> 1) . snd) rest
    in (firstComment : continuing) : runs later
  layoutRun [] = []
  layoutRun run =
    let baseColumn = minimum $ column . fst <$> run
    in layoutComment baseColumn <$> run
  layoutComment baseColumn (source, lineDelta) = do
    commentPlan <- mAsk
    case planSourceCommentWithDelta commentPlan source lineDelta 0 of
      Left commentError -> do
        mTell [ErrorCommentPlan $ show commentError]
        docEmpty
      Right planned -> allocateNode $ BDFComment planned
        { plannedCommentPlacement = (plannedCommentPlacement planned)
            { placementLineRelation = CommentOwnLine }
        , plannedCommentIndentPolicy = OwnerRelativeIndent
        , plannedCommentLineDelta = lineDelta
        , plannedCommentColumnDelta = column source - baseColumn
        }
