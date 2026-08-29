{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Pattern.Comments
  ( layoutPatternSourceComment
  , patternSourceCommentPrecedesNode
  , patternSourceCommentWithinNodeSpan
  , patternSourceCommentsWithinNode
  , ownedTrailingPatternComments
  ) where

import qualified Data.Map                                as Map
import qualified Data.Set                                as Set
import           GHC                                      ( GenLocated(L) )
import           GHC.Hs                                   ( Pat )
import qualified GHC.OldList                             as List
import           GHC.Types.SrcLoc                         ( Located
                                                          , getLoc
                                                          , srcSpanEndCol
                                                          , srcSpanEndLine
                                                          , srcSpanStartCol
                                                          , srcSpanStartLine
                                                          )
import           Language.Haskell.Brittany.Internal.ExactPrintCompat
                                                          ( realSpanToSrcSpan
                                                          , srcSpanToRealSpan
                                                          )
import           Language.Haskell.Brittany.Internal.ExactPrintUtils
                                                          ( foldedAnnKeys )
import           Language.Haskell.Brittany.Internal.ExactSource
                                                          ( sourceCommentFragment
                                                          )
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Prelude
import           Language.Haskell.Brittany.Internal.SourceComment.Types
import           Language.Haskell.Brittany.Internal.Types

ownedTrailingPatternComments
  :: Located (Pat GhcPs) -> ToBriDocM [SourceComment]
ownedTrailingPatternComments node = do
  commentPlan <- mAsk
  let ownerKeys = foldedAnnKeys node
      nodeEnd = do
        nodeSpan <- srcSpanToRealSpan $ getLoc node
        pure (srcSpanEndLine nodeSpan, srcSpanEndCol nodeSpan)
      isTrailing sourceComment = case nodeEnd of
        Nothing -> False
        Just endPosition -> patternSourceCommentStart sourceComment >= endPosition
  pure
    [ sourceComment
    | (key, placement) <- List.sortOn
        (placementRelativeOrder . snd)
        $ Map.toList $ commentPlanPlacements commentPlan
    , NodeId ownerKey <- [placementOwner placement]
    , Set.member ownerKey ownerKeys
    , Just sourceComment <- [Map.lookup key $ commentPlanSources commentPlan]
    , isTrailing sourceComment
    ]

layoutPatternSourceComment :: SourceComment -> ToBriDocM BriDocNumbered
layoutPatternSourceComment sourceComment = briDocBySourceFragmentNoComment
  (L (realSpanToSrcSpan $ sourceCommentSpan sourceComment) sourceComment)
  (sourceCommentFragment sourceComment)

patternSourceCommentStart :: SourceComment -> (Int, Int)
patternSourceCommentStart sourceComment =
  ( srcSpanStartLine $ sourceCommentSpan sourceComment
  , srcSpanStartCol $ sourceCommentSpan sourceComment
  )

patternSourceCommentPrecedesNode :: Located ast -> SourceComment -> Bool
patternSourceCommentPrecedesNode node sourceComment = case
    srcSpanToRealSpan $ getLoc node of
  Just nodeSpan ->
    ( srcSpanEndLine $ sourceCommentSpan sourceComment
    , srcSpanEndCol $ sourceCommentSpan sourceComment
    )
      <= (srcSpanStartLine nodeSpan, srcSpanStartCol nodeSpan)
  Nothing -> False

patternSourceCommentWithinNodeSpan :: Located ast -> SourceComment -> Bool
patternSourceCommentWithinNodeSpan node sourceComment = case
    srcSpanToRealSpan $ getLoc node of
  Just nodeSpan ->
    (srcSpanStartLine nodeSpan, srcSpanStartCol nodeSpan)
      <= patternSourceCommentStart sourceComment
      && ( srcSpanEndLine $ sourceCommentSpan sourceComment
         , srcSpanEndCol $ sourceCommentSpan sourceComment
         )
        <= (srcSpanEndLine nodeSpan, srcSpanEndCol nodeSpan)
  Nothing -> False

patternSourceCommentsWithinNode
  :: Located ast -> ToBriDocM [SourceComment]
patternSourceCommentsWithinNode node = do
  commentPlan <- mAsk
  pure $ List.sortOn patternSourceCommentStart
    $ filter (patternSourceCommentWithinNodeSpan node)
    $ Map.elems
    $ commentPlanSources commentPlan
