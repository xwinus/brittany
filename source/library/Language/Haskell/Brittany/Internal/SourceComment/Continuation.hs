{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.SourceComment.Continuation
  ( TrailingCommentRun
  , continueTrailingCommentRun
  , startTrailingCommentRun
  , trailingCommentColumn
  ) where

import Data.Kind (Type)
import qualified Data.Text as Text
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.CommentBoundary.Trailing
  ( plainLineCommentText )
import Language.Haskell.Brittany.Internal.ExactPrintCompat
  ( AnnKey(..), annKeyRealSpan, unConName )
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types

type TrailingCommentRun :: Type
data TrailingCommentRun = TrailingCommentRun
  { trailingCommentSeed :: SourceComment
  , trailingCommentPrevious :: SourceComment
  , trailingCommentColumn :: Int
  , trailingCommentSourceColumn :: Maybe Int
  }

startTrailingCommentRun :: PlannedComment -> Int -> Maybe TrailingCommentRun
startTrailingCommentRun planned column = do
  let source = plannedCommentSource planned
      placement = plannedCommentPlacement planned
  guard $ ordinarySource source
  guard $ placementLineRelation placement == InlineComment
  guard $ case commentBoundaryPath $ plannedCommentBoundary planned of
    ConstructorBoundaryPath{} -> False
    _ -> True
  pure $ TrailingCommentRun source source column Nothing

continueTrailingCommentRun
  :: PlannedComment -> TrailingCommentRun -> Maybe TrailingCommentRun
continueTrailingCommentRun planned run = do
  let source = plannedCommentSource planned
      placement = plannedCommentPlacement planned
      currentSpan = sourceCommentSpan source
      previousSpan = sourceCommentSpan $ trailingCommentPrevious run
      seedSpan = sourceCommentSpan $ trailingCommentSeed run
      column = SrcLoc.srcSpanStartCol currentSpan
      NodeId owner@(AnnKey _ constructor) = placementOwner placement
  guard $ placementAnchor placement == AfterNode
  guard $ unConName constructor == "ValD"
  guard $ ordinarySource source
  ownerSpan <- annKeyRealSpan owner
  guard $ SrcLoc.srcSpanFile currentSpan == SrcLoc.srcSpanFile previousSpan
    && SrcLoc.srcSpanFile currentSpan == SrcLoc.srcSpanFile ownerSpan
  guard $ SrcLoc.srcSpanEndLine seedSpan == SrcLoc.srcSpanEndLine ownerSpan
    && SrcLoc.srcSpanStartCol seedSpan >= SrcLoc.srcSpanEndCol ownerSpan
  guard $ SrcLoc.srcSpanStartLine currentSpan
    == SrcLoc.srcSpanEndLine previousSpan + 1
  guard $ column > SrcLoc.srcSpanStartCol ownerSpan
    && maybe True (== column) (trailingCommentSourceColumn run)
  pure run
    { trailingCommentPrevious = source
    , trailingCommentSourceColumn = Just column
    }

ordinarySource :: SourceComment -> Bool
ordinarySource source = sourceCommentSyntax source == LineComment
  && SrcLoc.srcSpanStartLine span' == SrcLoc.srcSpanEndLine span'
  && plainLineCommentText (Text.unpack $ sourceCommentText source)
 where
  span' = sourceCommentSpan source
