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

  Hspec.it "keeps expensive structural units atomic but unaligned" $ do
    let rows = [structuralRow 0 "same" 10, structuralRow 1 "same" 40]
    case planAlignment 30 rows of
      Right plan -> do
        alignmentGroupRows <$> alignmentPlanGroups plan
          `Hspec.shouldBe` [rows]
        alignmentGroupLayout <$> alignmentPlanGroups plan
          `Hspec.shouldBe` [AlignmentUnaligned]
        alignmentPlanBreaks plan `Hspec.shouldBe` [1]
      Left plannerError -> Hspec.expectationFailure $ show plannerError

  Hspec.it "honors an explicit unaligned structural alternative" $ do
    let baseFirst = structuralRow 0 "same" 10
        first = baseFirst
          { alignmentRowCandidates =
              UnalignedLayout : alignmentRowCandidates baseFirst
          }
        rows = [first, structuralRow 1 "same" 12]
    case planAlignment 30 rows of
      Right plan -> do
        alignmentGroupLayout <$> alignmentPlanGroups plan
          `Hspec.shouldBe` [AlignmentUnaligned]
        alignmentPlanBreaks plan `Hspec.shouldBe` [1]
      Left plannerError -> Hspec.expectationFailure $ show plannerError

  Hspec.it "uses an inclusive deterministic padding boundary" $ do
    alignmentPlanBreaks <$> planAlignment 30 [optionalRow 0 0, optionalRow 1 15]
      `Hspec.shouldBe` Right []
    alignmentPlanBreaks <$> planAlignment 30 [optionalRow 0 0, optionalRow 1 16]
      `Hspec.shouldBe` Right [1]

  Hspec.it "reports all padding and overflow cost dimensions" $ do
    case planAlignmentWithin 30 20 [contentRow 0 10 18, contentRow 1 12 19] of
      Right plan -> do
        alignmentGroupCost <$> alignmentPlanGroups plan
          `Hspec.shouldBe` [AlignmentCost 12 2 2 0 0 1 2]
        alignmentPlanBoundaryCost plan `Hspec.shouldBe` 0
      Left plannerError -> Hspec.expectationFailure $ show plannerError

  Hspec.it "rejects alignment that would add configured-width overflow" $ do
    let rows = [contentRow 0 10 20, contentRow 1 13 20]
    case planAlignmentWithin 30 20 rows of
      Right plan -> do
        alignmentPlanBreaks plan `Hspec.shouldBe` [1]
        fmap rejectionSummary (alignmentPlanRejections plan)
          `Hspec.shouldContain`
            [([0, 1], AlignmentCost 13 3 3 3 3 1 2, ConfiguredWidthOverflow)]
      Left plannerError -> Hspec.expectationFailure $ show plannerError

  Hspec.it "uses stable row identities to break equal-cost ties" $ do
    let rows = [optionalRow 0 0, optionalRow 1 15, optionalRow 2 16]
    alignmentPlanBreaks <$> planAlignment 30 rows
      `Hspec.shouldBe` Right [1]

  Hspec.it "rejects empty, missing, and invalid numeric input" $ do
    planAlignment 30 ([] :: [AlignmentRow String])
      `Hspec.shouldBe` Left EmptyAlignmentPlan
    planAlignment 30 [AlignmentRow 6 [] 1 1 0 :: AlignmentRow String]
      `Hspec.shouldBe` Left (MissingAlignmentCandidates 6)
    planAlignment 30 [optionalRow 7 (-1)]
      `Hspec.shouldBe` Left (NegativeAlignmentWidth 7 (-1))
    planAlignment 30 [AlignmentRow 8 [OptionalAlignment "siblings"] 4 3 0]
      `Hspec.shouldBe` Left (InvalidAlignmentContentWidth 8 4 3)
    planAlignment (-1) [optionalRow 0 1]
      `Hspec.shouldBe` Left (InvalidAlignmentLimit (-1))
    planAlignmentWithin 30 (-1) [optionalRow 0 1]
      `Hspec.shouldBe` Left (InvalidConfiguredWidth (-1))
    let negativeBreak = (optionalRow 9 1) { alignmentRowBreakCost = -1 }
    planAlignment 30 [negativeBreak]
      `Hspec.shouldBe` Left (NegativeAlignmentBreakCost 9 (-1))

  Hspec.it "rejects duplicate identities and contradictory constraints" $ do
    planAlignment 30 [optionalRow 2 1, optionalRow 2 2]
      `Hspec.shouldBe` Left (DuplicateAlignmentIdentity 2)
    let contradictory = AlignmentRow
          3
          [OptionalAlignment "siblings", ProhibitedAlignment "siblings"]
          1
          1
          0
    planAlignment 30 [contradictory]
      `Hspec.shouldBe` Left (ContradictoryAlignmentConstraints 3)
    let structuralContradiction = AlignmentRow
          4
          [StructuralAffinity "unit", ProhibitedAlignment "unit"]
          1
          1
          0
    planAlignment 30 [structuralContradiction]
      `Hspec.shouldBe` Left (ContradictoryAlignmentConstraints 4)

  Hspec.it "reports an impossible non-contiguous partition" $ do
    planAlignment 30 [optionalRow 4 1, optionalRow 6 2]
      `Hspec.shouldBe` Left (ImpossibleAlignmentPartition 4 6)

optionalRow :: Int -> Int -> AlignmentRow String
optionalRow identity width = contentRow identity width width

contentRow :: Int -> Int -> Int -> AlignmentRow String
contentRow identity width contentWidth = AlignmentRow
  identity
  [OptionalAlignment "siblings"]
  width
  contentWidth
  1

structuralRow :: Int -> String -> Int -> AlignmentRow String
structuralRow identity group width = AlignmentRow
  identity
  [StructuralAffinity group, OptionalAlignment "siblings"]
  width
  width
  1

rejectionSummary
  :: AlignmentRejection group
  -> ([Int], AlignmentCost, AlignmentRejectionReason)
rejectionSummary rejection =
  ( alignmentRowIdentity <$> alignmentRejectedRows rejection
  , alignmentRejectedCost rejection
  , alignmentRejectionReason rejection
  )
