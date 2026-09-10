{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.SourceComment.Continuation
  ( TrailingCommentRun
  , continueTrailingCommentRun
  , continueTrailingCommentFragment
  , startTrailingCommentRun
  , trailingCommentColumn
  ) where

import Data.Kind (Type)
import qualified Data.Map as Map
import qualified Data.Set as Set
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
  { trailingCommentSeed :: PlannedComment
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
  pure $ TrailingCommentRun planned source column Nothing

continueTrailingCommentRun
  :: PlannedComment -> TrailingCommentRun -> Maybe TrailingCommentRun
continueTrailingCommentRun planned run = do
  let source = plannedCommentSource planned
      placement = plannedCommentPlacement planned
      currentSpan = sourceCommentSpan source
      previousSpan = sourceCommentSpan $ trailingCommentPrevious run
      seed = trailingCommentSeed run
      column = SrcLoc.srcSpanStartCol currentSpan
  ownerSpan <- declarationOwnerSpan planned seed <|> adjacentOwnerSpan planned seed
  guard $ ordinarySource source
  guard $ placementLineRelation placement == CommentOwnLine
  guard $ SrcLoc.srcSpanFile currentSpan == SrcLoc.srcSpanFile previousSpan
    && SrcLoc.srcSpanFile currentSpan == SrcLoc.srcSpanFile ownerSpan
  guard $ SrcLoc.srcSpanStartLine currentSpan
    == SrcLoc.srcSpanEndLine previousSpan + 1
  let normalizedDeclaration = case declarationOwnerSpan planned seed of
        Just _ -> True
        Nothing -> False
  guard $ maybe True
    (if normalizedDeclaration then (== column) else (<= column))
    (trailingCommentSourceColumn run)
  pure run
    { trailingCommentPrevious = source
    , trailingCommentColumn = trailingCommentColumn run
        + case trailingCommentSourceColumn run of
          Nothing -> 0
          Just _ -> column - SrcLoc.srcSpanStartCol previousSpan
    , trailingCommentSourceColumn = trailingCommentSourceColumn run <|> Just column
    }

declarationOwnerSpan :: PlannedComment -> PlannedComment -> Maybe SrcLoc.RealSrcSpan
declarationOwnerSpan planned seed = do
  let placement = plannedCommentPlacement planned
      seedSpan = sourceCommentSpan $ plannedCommentSource seed
      NodeId owner@(AnnKey _ constructor) = placementOwner placement
      NodeId seedOwner@(AnnKey _ seedConstructor) =
        placementOwner $ plannedCommentPlacement seed
  ownerSpan <- case commentBoundaryPath $ plannedCommentBoundary seed of
    ConstructorBoundaryPath{} -> do
      guard $ plannedCommentBoundary planned == plannedCommentBoundary seed
      guard $ unConName seedConstructor `elem` ["ConDeclH98", "ConDeclGADT"]
      guard $ placementLineRelation placement == CommentOwnLine
      annKeyRealSpan seedOwner
    _ -> do
      guard $ placementAnchor placement == AfterNode
      guard $ unConName constructor == "ValD"
      annKeyRealSpan owner
  guard $ SrcLoc.srcSpanEndLine seedSpan == SrcLoc.srcSpanEndLine ownerSpan
    && SrcLoc.srcSpanStartCol seedSpan >= SrcLoc.srcSpanEndCol ownerSpan
  guard $ SrcLoc.srcSpanStartCol (sourceCommentSpan $ plannedCommentSource planned)
    > SrcLoc.srcSpanStartCol ownerSpan
  pure ownerSpan

adjacentOwnerSpan :: PlannedComment -> PlannedComment -> Maybe SrcLoc.RealSrcSpan
adjacentOwnerSpan planned seed = do
  let placement = plannedCommentPlacement planned
      seedPlacement = plannedCommentPlacement seed
      NodeId owner = placementOwner placement
      NodeId seedOwner = placementOwner seedPlacement
      seedSpan = sourceCommentSpan $ plannedCommentSource seed
      column = SrcLoc.srcSpanStartCol $ sourceCommentSpan $ plannedCommentSource planned
  guard $ plannedCommentBoundary planned == plannedCommentBoundary seed
  -- A substantially dedented marker belongs to the following syntax boundary.
  guard $ column >= SrcLoc.srcSpanStartCol seedSpan - 1
  ownerSpan <- annKeyRealSpan owner
  seedOwnerSpan <- annKeyRealSpan seedOwner
  case placementAnchor seedPlacement of
    BeforeNode -> guard $ placementOwner placement == placementOwner seedPlacement
      && column >= SrcLoc.srcSpanStartCol seedSpan
      && SrcLoc.srcSpanEndLine seedSpan < SrcLoc.srcSpanStartLine seedOwnerSpan
    _ -> do
      -- A leading comment at the next node's indentation starts a separate group.
      guard $ column > SrcLoc.srcSpanStartCol ownerSpan
        && column > SrcLoc.srcSpanStartCol seedOwnerSpan
      guard $ SrcLoc.srcSpanEndLine seedSpan == SrcLoc.srcSpanEndLine seedOwnerSpan
        && SrcLoc.srcSpanStartCol seedSpan >= SrcLoc.srcSpanEndCol seedOwnerSpan
  pure seedOwnerSpan

continueTrailingCommentFragment
  :: CommentPlan -> ExactSourceFragment -> TrailingCommentRun -> Maybe TrailingCommentRun
continueTrailingCommentFragment plan fragment run = do
  guard $ fragmentAbsoluteColumn fragment == Nothing
  key <- case Set.toList $ fragmentCommentKeys fragment of
    [key] -> Just key
    _ -> Nothing
  source <- Map.lookup key $ commentPlanSources plan
  guard $ fragmentText fragment == sourceCommentText source
  placement <- Map.lookup key $ commentPlanPlacements plan
  boundary <- Map.lookup key $ commentPlanBoundaries plan
  continueTrailingCommentRun
    (PlannedComment source placement boundary SourceColumnIndent 1 0) run

ordinarySource :: SourceComment -> Bool
ordinarySource source = sourceCommentSyntax source == LineComment
  && SrcLoc.srcSpanStartLine span' == SrcLoc.srcSpanEndLine span'
  && plainLineCommentText (Text.unpack $ sourceCommentText source)
 where
  span' = sourceCommentSpan source
