{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Alignment
  ( AlignmentCandidate(..)
  , AlignmentRow(..)
  , AlignmentCost(..)
  , AlignmentLayout(..)
  , AlignmentGroup(..)
  , AlignmentRejection(..)
  , AlignmentRejectionReason(..)
  , AlignmentPlan(..)
  , AlignmentPlanError(..)
  , alignmentPaddingLimit
  , alignmentPlanBreaks
  , planAlignment
  , planAlignmentWithin
  ) where

import qualified Data.Data
import qualified Data.List as List
import Language.Haskell.Brittany.Internal.Prelude

-- Structural affinity controls legal partitions. It never requires visual
-- padding. OptionalAlignment only describes a visual preference.
data AlignmentCandidate group
  = StructuralAffinity group
  | OptionalAlignment group
  | ProhibitedAlignment group
  | UnalignedLayout
  deriving (Eq, Ord, Data.Data.Data, Show)

data AlignmentRow group = AlignmentRow
  { alignmentRowIdentity :: Int
  , alignmentRowCandidates :: [AlignmentCandidate group]
  , alignmentRowWidth :: Int
  , alignmentRowContentWidth :: Int
  , alignmentRowBreakCost :: Int
  }
  deriving (Eq, Show)

data AlignmentCost = AlignmentCost
  { alignmentTargetWidth :: Int
  , alignmentMaximumPadding :: Int
  , alignmentTotalPadding :: Int
  , alignmentMaximumOverflow :: Int
  , alignmentTotalOverflow :: Int
  , alignmentAffectedRows :: Int
  , alignmentRowCount :: Int
  }
  deriving (Eq, Show)

data AlignmentLayout
  = AlignmentAligned
  | AlignmentUnaligned
  deriving (Eq, Show)

data AlignmentGroup group = AlignmentGroup
  { alignmentGroupLayout :: AlignmentLayout
  , alignmentGroupRows :: [AlignmentRow group]
  , alignmentGroupCost :: AlignmentCost
  }
  deriving (Eq, Show)

data AlignmentRejectionReason
  = OptionalPaddingLimitExceeded
  | ConfiguredWidthOverflow
  | ExplicitUnalignedLayout
  | HigherCostPartitionSelected
  deriving (Eq, Show)

data AlignmentRejection group = AlignmentRejection
  { alignmentRejectedRows :: [AlignmentRow group]
  , alignmentRejectedCost :: AlignmentCost
  , alignmentRejectionReason :: AlignmentRejectionReason
  }
  deriving (Eq, Show)

data AlignmentPlan group = AlignmentPlan
  { alignmentOptionalPaddingLimit :: Int
  , alignmentConfiguredWidth :: Maybe Int
  , alignmentPlanBoundaryCost :: Int
  , alignmentPlanGroups :: [AlignmentGroup group]
  , alignmentPlanRejections :: [AlignmentRejection group]
  }
  deriving (Eq, Show)

data AlignmentPlanError
  = EmptyAlignmentPlan
  | InvalidAlignmentLimit Int
  | InvalidConfiguredWidth Int
  | MissingAlignmentCandidates Int
  | NegativeAlignmentWidth Int Int
  | InvalidAlignmentContentWidth Int Int Int
  | NegativeAlignmentBreakCost Int Int
  | DuplicateAlignmentIdentity Int
  | ContradictoryAlignmentConstraints Int
  | ImpossibleAlignmentPartition Int Int
  deriving (Eq, Show)

alignmentPaddingLimit :: Int -> Int
alignmentPaddingLimit alignmentLimit = max 0 alignmentLimit `div` 2

planAlignment
  :: Eq group
  => Int
  -> [AlignmentRow group]
  -> Either AlignmentPlanError (AlignmentPlan group)
planAlignment configuredLimit = planAlignmentWith configuredLimit Nothing

planAlignmentWithin
  :: Eq group
  => Int
  -> Int
  -> [AlignmentRow group]
  -> Either AlignmentPlanError (AlignmentPlan group)
planAlignmentWithin configuredLimit configuredWidth =
  planAlignmentWith configuredLimit $ Just configuredWidth

planAlignmentWith
  :: Eq group
  => Int
  -> Maybe Int
  -> [AlignmentRow group]
  -> Either AlignmentPlanError (AlignmentPlan group)
planAlignmentWith configuredLimit configuredWidth rows = do
  validateRows configuredLimit configuredWidth rows
  let paddingLimit = alignmentPaddingLimit configuredLimit
      units = structuralUnits rows
      groups = optionalGroups configuredWidth paddingLimit units
  validatePartitions groups
  pure AlignmentPlan
    { alignmentOptionalPaddingLimit = paddingLimit
    , alignmentConfiguredWidth = configuredWidth
    , alignmentPlanBoundaryCost = boundaryCost groups
    , alignmentPlanGroups = makeGroup configuredWidth paddingLimit <$> groups
    , alignmentPlanRejections = rejectedGroups
        configuredWidth paddingLimit units groups
    }

alignmentPlanBreaks :: AlignmentPlan group -> [Int]
alignmentPlanBreaks plan = List.sort
  $ groupBoundaryBreaks ++ unalignedBreaks
 where
  groups = alignmentPlanGroups plan
  groupBoundaryBreaks =
    [ alignmentRowIdentity firstRow
    | group <- drop 1 groups
    , firstRow : _ <- [alignmentGroupRows group]
    ]
  unalignedBreaks =
    [ alignmentRowIdentity row
    | group <- groups
    , alignmentGroupLayout group == AlignmentUnaligned
    , row <- drop 1 $ alignmentGroupRows group
    ]

validateRows
  :: Eq group
  => Int
  -> Maybe Int
  -> [AlignmentRow group]
  -> Either AlignmentPlanError ()
validateRows configuredLimit configuredWidth rows
  | configuredLimit < 0 = Left $ InvalidAlignmentLimit configuredLimit
  | Just width <- configuredWidth, width < 0 =
      Left $ InvalidConfiguredWidth width
  | null rows = Left EmptyAlignmentPlan
  | Just row <- List.find (null . alignmentRowCandidates) rows =
      Left $ MissingAlignmentCandidates $ alignmentRowIdentity row
  | Just row <- List.find ((< 0) . alignmentRowWidth) rows = Left
      $ NegativeAlignmentWidth
        (alignmentRowIdentity row)
        (alignmentRowWidth row)
  | Just row <- List.find invalidContentWidth rows = Left
      $ InvalidAlignmentContentWidth
        (alignmentRowIdentity row)
        (alignmentRowWidth row)
        (alignmentRowContentWidth row)
  | Just row <- List.find ((< 0) . alignmentRowBreakCost) rows = Left
      $ NegativeAlignmentBreakCost
        (alignmentRowIdentity row)
        (alignmentRowBreakCost row)
  | duplicate : _ <- duplicateIdentities = Left
      $ DuplicateAlignmentIdentity duplicate
  | Just row <- List.find contradictory rows = Left
      $ ContradictoryAlignmentConstraints $ alignmentRowIdentity row
  | otherwise = Right ()
 where
  invalidContentWidth row = alignmentRowContentWidth row
    < alignmentRowWidth row
  identities = alignmentRowIdentity <$> rows
  duplicateIdentities = identities List.\\ List.nub identities
  contradictory row = not $ null
    $ (structuralKeys row ++ optionalKeys row)
    `List.intersect` prohibitedKeys row

validatePartitions
  :: [[[AlignmentRow group]]]
  -> Either AlignmentPlanError ()
validatePartitions groups = case List.concatMap List.concat groups of
  [] -> Left $ ImpossibleAlignmentPartition 0 0
  firstRow : remainingRows ->
    let firstIdentity = alignmentRowIdentity firstRow
        lastIdentity = foldl'
          (\_ row -> alignmentRowIdentity row)
          firstIdentity
          remainingRows
        plannedRowCount = 1 + length remainingRows
    in if plannedRowCount == lastIdentity - firstIdentity + 1
      then Right ()
      else Left $ ImpossibleAlignmentPartition firstIdentity lastIdentity

structuralUnits :: Eq group => [AlignmentRow group] -> [[AlignmentRow group]]
structuralUnits = foldr addRow []
 where
  addRow row (unit@(next : _) : remaining)
    | structurallyConnected row next = (row : unit) : remaining
  addRow row units = [row] : units

  structurallyConnected left right = not $ null
    $ structuralKeys left `List.intersect` structuralKeys right

optionalGroups
  :: Eq group
  => Maybe Int
  -> Int
  -> [[AlignmentRow group]]
  -> [[[AlignmentRow group]]]
optionalGroups configuredWidth paddingLimit units
  | singleGroupIsOptimal = [units]
  | otherwise = case bestPlans of
      plan : _ -> scoredPlanGroups plan
      [] -> []
 where
  unitCount = length units
  wholeCost = costOfUnits configuredWidth units
  -- Overflow is the primary non-negative score and group count is secondary,
  -- so a valid zero-overflow single group cannot be beaten.
  singleGroupIsOptimal = unitCount == 1
    || ( unitsConnected units
      && not (candidateForcesUnaligned units)
      && alignmentMaximumPadding wholeCost <= paddingLimit
      && alignmentTotalOverflow wholeCost == 0
       )
  bestPlans = [bestFrom index | index <- [0 .. unitCount]]

  bestFrom index
    | index == unitCount = ScoredPlan [] emptyPlanScore
    | otherwise = selectBest
        [ prependCandidate candidate candidateCost
            $ planAt (index + candidateLength)
        | candidateLength <- [1 .. unitCount - index]
        , let candidate = take candidateLength $ drop index units
        , let candidateCost = costOfUnits configuredWidth candidate
        , candidateLength == 1
            || ( unitsConnected candidate
              && candidateWithinLimit candidate candidateCost
               )
        ]

  planAt index = case drop index bestPlans of
    plan : _ -> plan
    [] -> ScoredPlan [] emptyPlanScore

  selectBest (candidate : candidates) = foldl' chooseBetter candidate candidates
  selectBest [] = ScoredPlan [] emptyPlanScore

  chooseBetter left right
    | compare (scoredPlanScore left) (scoredPlanScore right) == GT = right
    | otherwise = left

  candidateWithinLimit candidate candidateCost =
    not (candidateForcesUnaligned candidate)
      && alignmentMaximumPadding candidateCost
        <= paddingLimit

  unitsConnected candidate = and
    $ zipWith visuallyConnectedUnits candidate (drop 1 candidate)

  prependCandidate candidate candidateCost tailPlan = ScoredPlan
    { scoredPlanGroups = candidate : tailGroups
    , scoredPlanScore = PlanScore
        { scoreTotalOverflow = alignmentTotalOverflow candidateCost
            + scoreTotalOverflow tailScore
        , scoreGroupCount = 1 + scoreGroupCount tailScore
        , scoreMaximumPadding = max
            (alignmentMaximumPadding candidateCost)
            (scoreMaximumPadding tailScore)
        , scoreTotalPadding = alignmentTotalPadding candidateCost
            + scoreTotalPadding tailScore
        , scoreBoundaryCost = nextBoundaryCost
            + scoreBoundaryCost tailScore
        , scoreBoundaryIdentities = nextBoundaryIdentity
            ++ scoreBoundaryIdentities tailScore
        }
    }
   where
    tailGroups = scoredPlanGroups tailPlan
    tailScore = scoredPlanScore tailPlan
    nextRows = case tailGroups of
      nextGroup : _ -> List.concat nextGroup
      [] -> []
    nextBoundaryCost = case nextRows of
      nextRow : _ -> alignmentRowBreakCost nextRow
      [] -> 0
    nextBoundaryIdentity = case nextRows of
      nextRow : _ -> [alignmentRowIdentity nextRow]
      [] -> []

data PlanScore = PlanScore
  { scoreTotalOverflow :: Int
  , scoreGroupCount :: Int
  , scoreMaximumPadding :: Int
  , scoreTotalPadding :: Int
  , scoreBoundaryCost :: Int
  , scoreBoundaryIdentities :: [Int]
  }
  deriving (Eq, Ord)

data ScoredPlan group = ScoredPlan
  { scoredPlanGroups :: [[[AlignmentRow group]]]
  , scoredPlanScore :: PlanScore
  }

emptyPlanScore :: PlanScore
emptyPlanScore = PlanScore 0 0 0 0 0 []

boundaryCost :: [[[AlignmentRow group]]] -> Int
boundaryCost groups = sum
  [ alignmentRowBreakCost row
  | group <- drop 1 groups
  , row : _ <- [List.concat group]
  ]

rejectedGroups
  :: Eq group
  => Maybe Int
  -> Int
  -> [[AlignmentRow group]]
  -> [[[AlignmentRow group]]]
  -> [AlignmentRejection group]
rejectedGroups configuredWidth paddingLimit units chosenGroups =
  [ AlignmentRejection
      { alignmentRejectedRows = List.concat candidate
      , alignmentRejectedCost = candidateCost
      , alignmentRejectionReason =
          if candidateForcesUnaligned candidate
            then ExplicitUnalignedLayout
            else if alignmentMaximumPadding candidateCost > paddingLimit
            then OptionalPaddingLimitExceeded
            else if alignmentTotalOverflow candidateCost > chosenOverflow
              then ConfiguredWidthOverflow
              else HigherCostPartitionSelected
      }
  | start <- [0 .. length units - 1]
  , candidateLength <- [2 .. length units - start]
  , let candidate = take candidateLength $ drop start units
  , unitsConnected candidate
  , candidateIdentities candidate `notElem` chosenIdentities
  , let candidateCost = costOfUnits configuredWidth candidate
  ]
 where
  chosenIdentities = candidateIdentities <$> chosenGroups
  chosenOverflow = sum
    $ alignmentTotalOverflow . costOfUnits configuredWidth <$> chosenGroups
  candidateIdentities = fmap alignmentRowIdentity . List.concat
  unitsConnected candidate = and
    $ zipWith visuallyConnectedUnits candidate (drop 1 candidate)

makeGroup
  :: Maybe Int
  -> Int
  -> [[AlignmentRow group]]
  -> AlignmentGroup group
makeGroup configuredWidth paddingLimit units = AlignmentGroup
  { alignmentGroupLayout = if withinLimit
      then AlignmentAligned
      else AlignmentUnaligned
  , alignmentGroupRows = rows
  , alignmentGroupCost = if withinLimit
      then cost
      else unalignedCost configuredWidth rows
  }
 where
  rows = List.concat units
  cost = costOfUnits configuredWidth units
  withinLimit = not (candidateForcesUnaligned units)
    && alignmentMaximumPadding cost <= paddingLimit

candidateForcesUnaligned :: [[AlignmentRow group]] -> Bool
candidateForcesUnaligned = any
  $ any
  $ any isUnaligned . alignmentRowCandidates
 where
  isUnaligned UnalignedLayout = True
  isUnaligned _ = False

costOfUnits :: Maybe Int -> [[AlignmentRow group]] -> AlignmentCost
costOfUnits configuredWidth units = AlignmentCost
  { alignmentTargetWidth = targetWidth
  , alignmentMaximumPadding = maximum $ 0 : rowPaddings
  , alignmentTotalPadding = sum rowPaddings
  , alignmentMaximumOverflow = maximum $ 0 : overflows
  , alignmentTotalOverflow = sum overflows
  , alignmentAffectedRows = length $ filter (> 0) rowPaddings
  , alignmentRowCount = length rows
  }
 where
  rows = List.concat units
  unitWidths = maximum . (0 :) . fmap alignmentRowWidth <$> units
  targetWidth = maximum $ 0 : unitWidths
  rowPaddings = (targetWidth -) . alignmentRowWidth <$> rows
  overflows = case configuredWidth of
    Nothing -> replicate (length rows) 0
    Just width ->
      [ max 0 $ alignmentRowContentWidth row + padding - width
      | (row, padding) <- zip rows rowPaddings
      ]

unalignedCost :: Maybe Int -> [AlignmentRow group] -> AlignmentCost
unalignedCost configuredWidth rows = AlignmentCost
  { alignmentTargetWidth = maximum $ 0 : (alignmentRowWidth <$> rows)
  , alignmentMaximumPadding = 0
  , alignmentTotalPadding = 0
  , alignmentMaximumOverflow = maximum $ 0 : overflows
  , alignmentTotalOverflow = sum overflows
  , alignmentAffectedRows = 0
  , alignmentRowCount = length rows
  }
 where
  overflows = case configuredWidth of
    Nothing -> replicate (length rows) 0
    Just width -> max 0 . subtract width . alignmentRowContentWidth <$> rows

structuralKeys :: AlignmentRow group -> [group]
structuralKeys row =
  [group | StructuralAffinity group <- alignmentRowCandidates row]

optionalKeys :: AlignmentRow group -> [group]
optionalKeys row =
  [group | OptionalAlignment group <- alignmentRowCandidates row]

prohibitedKeys :: AlignmentRow group -> [group]
prohibitedKeys row =
  [group | ProhibitedAlignment group <- alignmentRowCandidates row]

visuallyConnectedUnits
  :: Eq group => [AlignmentRow group] -> [AlignmentRow group] -> Bool
visuallyConnectedUnits left right = case (reverse left, right) of
  (leftRow : _, rightRow : _) -> not $ null
    $ optionalKeys leftRow `List.intersect` optionalKeys rightRow
  _ -> False
