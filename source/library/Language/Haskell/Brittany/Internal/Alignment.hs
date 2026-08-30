{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Alignment
  ( AlignmentCandidate(..)
  , AlignmentStrength(..)
  , AlignmentRow(..)
  , AlignmentCost(..)
  , AlignmentGroup(..)
  , AlignmentRejection(..)
  , AlignmentRejectionReason(..)
  , AlignmentPlan(..)
  , AlignmentPlanError(..)
  , alignmentPaddingLimit
  , alignmentPlanBreaks
  , planAlignment
  ) where

import qualified Data.Data
import qualified Data.List as List
import Language.Haskell.Brittany.Internal.Prelude

data AlignmentCandidate group
  = RequiredAlignment group
  | OptionalAlignment group
  deriving (Eq, Ord, Data.Data.Data, Show)

data AlignmentStrength
  = AlignmentRequired
  | AlignmentOptional
  deriving (Eq, Show)

data AlignmentRow group = AlignmentRow
  { alignmentRowIdentity :: Int
  , alignmentRowCandidates :: [AlignmentCandidate group]
  , alignmentRowWidth :: Int
  }
  deriving (Eq, Show)

data AlignmentCost = AlignmentCost
  { alignmentTargetWidth :: Int
  , alignmentMaximumPadding :: Int
  , alignmentTotalPadding :: Int
  , alignmentAffectedRows :: Int
  , alignmentRowCount :: Int
  }
  deriving (Eq, Show)

data AlignmentGroup group = AlignmentGroup
  { alignmentGroupStrength :: AlignmentStrength
  , alignmentGroupRows :: [AlignmentRow group]
  , alignmentGroupCost :: AlignmentCost
  }
  deriving (Eq, Show)

data AlignmentRejectionReason
  = OptionalPaddingLimitExceeded
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
  , alignmentPlanGroups :: [AlignmentGroup group]
  , alignmentPlanRejections :: [AlignmentRejection group]
  }
  deriving (Eq, Show)

data AlignmentPlanError
  = EmptyAlignmentPlan
  | MissingAlignmentCandidates Int
  | NegativeAlignmentWidth Int Int
  deriving (Eq, Show)

-- Optional visual alignment is intentionally stricter than the existing
-- column-width tolerance. Required grammar relationships are never bounded.
alignmentPaddingLimit :: Int -> Int
alignmentPaddingLimit alignmentLimit = max 0 alignmentLimit `div` 2

planAlignment
  :: Eq group
  => Int
  -> [AlignmentRow group]
  -> Either AlignmentPlanError (AlignmentPlan group)
planAlignment configuredLimit rows = do
  validateRows rows
  let paddingLimit = alignmentPaddingLimit configuredLimit
      units = requiredUnits rows
      groups = optionalGroups paddingLimit units
  pure AlignmentPlan
    { alignmentOptionalPaddingLimit = paddingLimit
    , alignmentPlanGroups = makeGroup <$> groups
    , alignmentPlanRejections = rejectedGroups paddingLimit units groups
    }

alignmentPlanBreaks :: AlignmentPlan group -> [Int]
alignmentPlanBreaks plan =
  [ alignmentRowIdentity firstRow
  | group <- drop 1 $ alignmentPlanGroups plan
  , firstRow : _ <- [alignmentGroupRows group]
  ]

validateRows :: [AlignmentRow group] -> Either AlignmentPlanError ()
validateRows [] = Left EmptyAlignmentPlan
validateRows rows = case List.find (null . alignmentRowCandidates) rows of
  Just row -> Left $ MissingAlignmentCandidates $ alignmentRowIdentity row
  Nothing -> case List.find ((< 0) . alignmentRowWidth) rows of
    Just row -> Left
      $ NegativeAlignmentWidth
        (alignmentRowIdentity row)
        (alignmentRowWidth row)
    Nothing -> Right ()

requiredUnits :: Eq group => [AlignmentRow group] -> [[AlignmentRow group]]
requiredUnits = foldr addRow []
 where
  addRow row (unit@(next : _) : remaining)
    | requiredTogether row next = (row : unit) : remaining
  addRow row units = [row] : units

  requiredTogether left right = not $ null
    $ requiredGroups left `List.intersect` requiredGroups right

optionalGroups
  :: Eq group => Int -> [[AlignmentRow group]] -> [[[AlignmentRow group]]]
optionalGroups paddingLimit units = case bestPlans of
  plan : _ -> plan
  [] -> []
 where
  unitCount = length units
  bestPlans = [bestFrom index | index <- [0 .. unitCount]]

  bestFrom index
    | index == unitCount = []
    | otherwise = selectBest
        [ candidate : planAt (index + candidateLength)
        | candidateLength <- [1 .. unitCount - index]
        , let candidate = take candidateLength $ drop index units
        , candidateLength == 1
            || (unitsConnected candidate && candidateWithinLimit candidate)
        ]

  planAt index = case drop index bestPlans of
    plan : _ -> plan
    [] -> []

  selectBest (candidate : candidates) =
    foldl' chooseBetter candidate candidates
  selectBest [] = []

  chooseBetter left right
    | comparePlans left right == GT = right
    | otherwise = left

  candidateWithinLimit candidate =
    alignmentMaximumPadding (costOfUnits candidate) <= paddingLimit

  unitsConnected candidate = and
    $ zipWith optionalUnitsConnected candidate (drop 1 candidate)

  comparePlans left right = compare (planScore left) (planScore right)

  planScore groups =
    ( length groups
    , sum $ alignmentTotalPadding . costOfUnits <$> groups
    , maximum $ 0 : (alignmentMaximumPadding . costOfUnits <$> groups)
    , sum $ alignmentAffectedRows . costOfUnits <$> groups
    , [ alignmentRowIdentity row
      | group <- drop 1 groups
      , row : _ <- [List.concat group]
      ]
    )

rejectedGroups
  :: Eq group
  => Int
  -> [[AlignmentRow group]]
  -> [[[AlignmentRow group]]]
  -> [AlignmentRejection group]
rejectedGroups paddingLimit units chosenGroups =
  [ AlignmentRejection
      { alignmentRejectedRows = List.concat candidate
      , alignmentRejectedCost = candidateCost
      , alignmentRejectionReason =
          if alignmentMaximumPadding candidateCost > paddingLimit
            then OptionalPaddingLimitExceeded
            else HigherCostPartitionSelected
      }
  | start <- [0 .. length units - 1]
  , candidateLength <- [2 .. length units - start]
  , let candidate = take candidateLength $ drop start units
  , unitsConnected candidate
  , candidateIdentities candidate `notElem` chosenIdentities
  , let candidateCost = costOfUnits candidate
  ]
 where
  chosenIdentities = candidateIdentities <$> chosenGroups

  candidateIdentities = fmap alignmentRowIdentity . List.concat

  unitsConnected candidate = and
    $ zipWith optionalUnitsConnected candidate (drop 1 candidate)

makeGroup :: [[AlignmentRow group]] -> AlignmentGroup group
makeGroup units = AlignmentGroup
  { alignmentGroupStrength = case units of
      [_] -> case List.concat units of
        row : _ | not (null $ requiredGroups row) -> AlignmentRequired
        [] -> AlignmentOptional
        _ -> AlignmentOptional
      _ -> AlignmentOptional
  , alignmentGroupRows = rows
  , alignmentGroupCost = costOfUnits units
  }
 where
  rows = List.concat units

costOfUnits :: [[AlignmentRow group]] -> AlignmentCost
costOfUnits units = AlignmentCost
  { alignmentTargetWidth = targetWidth
  , alignmentMaximumPadding = maximum (0 : paddings)
  , alignmentTotalPadding = sum
      $ zipWith (*) paddings (length <$> units)
  , alignmentAffectedRows = sum
      [ length unit
      | (unit, padding) <- zip units paddings
      , padding > 0
      ]
  , alignmentRowCount = length rows
  }
 where
  rows = List.concat units
  unitWidths = maximum . (0 :) . fmap alignmentRowWidth <$> units
  targetWidth = maximum $ 0 : unitWidths
  paddings = (targetWidth -) <$> unitWidths

requiredGroups :: AlignmentRow group -> [group]
requiredGroups row =
  [ group
  | RequiredAlignment group <- alignmentRowCandidates row
  ]

optionalGroupsFor :: AlignmentRow group -> [group]
optionalGroupsFor row =
  [ group
  | OptionalAlignment group <- alignmentRowCandidates row
  ]

optionalUnitsConnected
  :: Eq group => [AlignmentRow group] -> [AlignmentRow group] -> Bool
optionalUnitsConnected left right = case (reverse left, right) of
  (leftRow : _, rightRow : _) -> not $ null
    $ optionalGroupsFor leftRow `List.intersect` optionalGroupsFor rightRow
  _ -> False
