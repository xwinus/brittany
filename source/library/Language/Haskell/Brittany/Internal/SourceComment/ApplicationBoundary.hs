{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.SourceComment.ApplicationBoundary
  ( classifyApplicationComment
  , isFollowingApplicationComment
  ) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.CommentBoundary.Trailing
  ( plainLineCommentText )
import Language.Haskell.Brittany.Internal.ExactPrintCompat
  ( AnnKey(..), annKeyRealSpan, unConName )
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types

-- Classify the whole run before layout transforms can change its base. An
-- inline seed, documentation or pragma protects all adjacent continuations.
-- A blank line starts a new run, which can independently follow the application.
classifyApplicationComment :: CommentPlan -> PlannedComment -> PlannedComment
classifyApplicationComment plan planned = fromMaybe planned $ do
  guard $ applicationSuffixCandidate planned
  run <- List.find (any ((== target) . sourceCommentKey . plannedCommentSource))
    $ commentRuns candidates
  guard $ all applicationSuffixCandidate run
  pure planned
    { plannedCommentIndentPolicy = ApplicationSuffixIndent
    , plannedCommentColumnDelta = sourceColumn (plannedCommentSource planned)
        - minimum (sourceColumn . plannedCommentSource <$> run)
    }
 where
  target = sourceCommentKey $ plannedCommentSource planned
  owner = placementOwner $ plannedCommentPlacement planned
  boundary = plannedCommentBoundary planned
  candidates = List.sortOn (sourcePosition . plannedCommentSource)
    [ PlannedComment source placement currentBoundary ContainerRelativeIndent 1 0
    | (key, source) <- Map.toList $ commentPlanSources plan
    , Just placement <- [Map.lookup key $ commentPlanPlacements plan]
    , placementOwner placement == owner
    , Just currentBoundary <- [Map.lookup key $ commentPlanBoundaries plan]
    , currentBoundary == boundary
    ]

isFollowingApplicationComment :: PlannedComment -> Bool
isFollowingApplicationComment planned =
  plannedCommentIndentPolicy planned == ApplicationSuffixIndent

-- A completed application does not own its container's continuation indent.
-- Its ordinary suffix comments stay anchored to that container's content row.
applicationSuffixCandidate :: PlannedComment -> Bool
applicationSuffixCandidate planned =
  plannedCommentIndentPolicy planned == ContainerRelativeIndent
    && placementLineRelation placement == CommentOwnLine
    && placementAnchor placement == AfterNode
    && ordinaryRole (placementRole placement)
    && ordinarySource source
    && followsOwner
 where
  source = plannedCommentSource planned
  placement = plannedCommentPlacement planned
  NodeId owner@(AnnKey _ constructor) = placementOwner placement
  followsOwner = unConName constructor == "HsApp"
    && fromMaybe False (do
      ownerSpan <- annKeyRealSpan owner
      let commentSpan = sourceCommentSpan source
      pure $ SrcLoc.srcSpanFile ownerSpan == SrcLoc.srcSpanFile commentSpan
        && SrcLoc.realSrcSpanEnd ownerSpan < SrcLoc.realSrcSpanStart commentSpan)

commentRuns :: [PlannedComment] -> [[PlannedComment]]
commentRuns [] = []
commentRuns (initial : remaining) = collect [initial] initial remaining
 where
  collect run _ [] = [reverse run]
  collect run previous rest@(current : following)
    | adjacent previous current = collect (current : run) current following
    | otherwise = reverse run : commentRuns rest
  adjacent previous current =
    let previousSpan = sourceCommentSpan $ plannedCommentSource previous
        currentSpan = sourceCommentSpan $ plannedCommentSource current
    in SrcLoc.srcSpanFile previousSpan == SrcLoc.srcSpanFile currentSpan
      && SrcLoc.srcSpanStartLine currentSpan <= SrcLoc.srcSpanEndLine previousSpan + 1

ordinaryRole :: CommentRole -> Bool
ordinaryRole role = case role of
  LeadingOrdinary -> True
  TrailingSameLine -> True
  Unattached -> True
  BetweenChildren ListChildren -> True
  _ -> False

ordinarySource :: SourceComment -> Bool
ordinarySource source = case sourceCommentSyntax source of
  LineComment -> plainLineCommentText text
  BlockComment -> case dropWhile Char.isSpace text of
    '{' : '-' : rest -> case dropWhile Char.isSpace rest of
      marker : _ | marker `elem` ['|', '^', '*', '$', '#'] -> False
      content -> not $ any (`List.isPrefixOf` content) ["BRITTANY", "brittany"]
    _ -> False
 where
  text = Text.unpack $ sourceCommentText source

sourceColumn :: SourceComment -> Int
sourceColumn = SrcLoc.srcSpanStartCol . sourceCommentSpan

sourcePosition :: SourceComment -> (Int, Int)
sourcePosition source =
  (SrcLoc.srcSpanStartLine $ sourceCommentSpan source, sourceColumn source)
