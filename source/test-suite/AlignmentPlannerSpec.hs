module AlignmentPlannerSpec (spec) where

import Language.Haskell.Brittany.Internal.Alignment
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "alignment planner" $ do
  Hspec.it "partitions optional groups around a width outlier" $ do
    let rows =
          [ optionalRow 0 10
          , optionalRow 1 12
          , optionalRow 2 40
          , optionalRow 3 11
          , optionalRow 4 13
          ]
    case planAlignment 30 rows of
      Right plan -> do
        alignmentPlanBreaks plan `Hspec.shouldBe` [2, 3]
        alignmentGroupRows <$> alignmentPlanGroups plan
          `Hspec.shouldBe` [take 2 rows, take 1 $ drop 2 rows, drop 3 rows]
      Left plannerError -> Hspec.expectationFailure $ show plannerError

  Hspec.it "never partitions consecutive rows in one required group" $ do
    let rows = [requiredRow 0 "same" 10, requiredRow 1 "same" 40]
    case planAlignment 30 rows of
      Right plan -> do
        alignmentPlanBreaks plan `Hspec.shouldBe` []
        alignmentGroupStrength <$> alignmentPlanGroups plan
          `Hspec.shouldBe` [AlignmentRequired]
      Left plannerError -> Hspec.expectationFailure $ show plannerError
    alignmentPlanBreaks
      <$> planAlignment 30 (rows ++ [optionalRow 2 40])
      `Hspec.shouldBe` Right []

  Hspec.it "uses an inclusive deterministic padding boundary" $ do
    alignmentPlanBreaks <$> planAlignment 30 [optionalRow 0 0, optionalRow 1 15]
      `Hspec.shouldBe` Right []
    alignmentPlanBreaks <$> planAlignment 30 [optionalRow 0 0, optionalRow 1 16]
      `Hspec.shouldBe` Right [1]

  Hspec.it "reports the padding cost for an accepted optional group" $ do
    case planAlignment 30 [optionalRow 0 10, optionalRow 1 12] of
      Right plan -> do
        alignmentGroupCost <$> alignmentPlanGroups plan
          `Hspec.shouldBe` [AlignmentCost 12 2 2 1 2]
        alignmentPlanRejections plan `Hspec.shouldBe` []
      Left plannerError -> Hspec.expectationFailure $ show plannerError

  Hspec.it "scores the complete run before choosing an outlier partition" $ do
    let rows = [optionalRow 0 0, optionalRow 1 15, optionalRow 2 16]
    case planAlignment 30 rows of
      Right plan -> do
        alignmentPlanBreaks plan `Hspec.shouldBe` [1]
        fmap rejectionSummary (alignmentPlanRejections plan)
          `Hspec.shouldContain`
            [ ([0, 1, 2]
              , AlignmentCost 16 16 17 2 3
              , OptionalPaddingLimitExceeded
              )
            ]
      Left plannerError -> Hspec.expectationFailure $ show plannerError

  Hspec.it "rejects empty and negative-width plans" $ do
    planAlignment 30 ([] :: [AlignmentRow String])
      `Hspec.shouldBe` Left EmptyAlignmentPlan
    planAlignment 30 ([AlignmentRow 6 [] 1] :: [AlignmentRow String])
      `Hspec.shouldBe` Left (MissingAlignmentCandidates 6)
    planAlignment 30 [optionalRow 7 (-1)]
      `Hspec.shouldBe` Left (NegativeAlignmentWidth 7 (-1))

optionalRow :: Int -> Int -> AlignmentRow String
optionalRow identity width =
  AlignmentRow identity [OptionalAlignment "siblings"] width

requiredRow :: Int -> String -> Int -> AlignmentRow String
requiredRow identity group width =
  AlignmentRow
    identity
    [RequiredAlignment group, OptionalAlignment "siblings"]
    width

rejectionSummary
  :: AlignmentRejection group
  -> ([Int], AlignmentCost, AlignmentRejectionReason)
rejectionSummary rejection =
  ( alignmentRowIdentity <$> alignmentRejectedRows rejection
  , alignmentRejectedCost rejection
  , alignmentRejectionReason rejection
  )
