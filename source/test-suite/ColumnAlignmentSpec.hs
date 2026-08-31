module ColumnAlignmentSpec (spec) where

import qualified Data.Text as Text
import Language.Haskell.Brittany.Internal.Alignment
import Language.Haskell.Brittany.Internal.ColumnAlignment
import Language.Haskell.Brittany.Internal.Types
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "column alignment integration" $ do
  Hspec.it "plans every column signature through the common interface" $ do
    let documents = concatMap signatureRows allColumnSignatures
    case columnAlignmentPlans 30 80 documents of
      Left plannerError -> Hspec.expectationFailure $ show plannerError
      Right plans -> do
        columnAlignmentPlanPath <$> plans
          `Hspec.shouldBe` replicate (length allColumnSignatures) []
        fmap (length . alignmentPlanGroups . columnAlignmentPlanResult) plans
          `Hspec.shouldBe` replicate (length allColumnSignatures) 1

  Hspec.it "partitions an outlier at its nested column path" $ do
    let documents =
          [ nestedRow "a"
          , nestedRow $ replicate 40 'b'
          ]
    case columnAlignmentPlans 30 100 documents of
      Left plannerError -> Hspec.expectationFailure $ show plannerError
      Right plans -> case
          [ columnAlignmentPlanResult plan
          | plan <- plans
          , columnAlignmentPlanPath plan == [0]
          ] of
        [nestedPlan] -> alignmentPlanBreaks nestedPlan `Hspec.shouldBe` [1]
        nestedPlans -> Hspec.expectationFailure
          $ "expected one nested plan, got " ++ show nestedPlans

  Hspec.it "reports invalid producer data with row and path context" $ do
    let invalid = BDCols (ColBindingLine []) [BDLit $ Text.pack "value"]
    columnAlignmentPlans 30 80 [invalid]
      `Hspec.shouldBe` Left
        (ColumnAlignmentError [] 0 $ MissingAlignmentCandidates 0)

signatureRows :: ColSig -> [BriDoc]
signatureRows signature =
  [ BDEnsureIndent BrIndentNone $ row signature "a"
  , row signature "bbb"
  , BDEmpty
  ]

row :: ColSig -> String -> BriDoc
row signature prefix = BDCols signature
  [ BDLit $ Text.pack prefix
  , BDLit $ Text.pack " = value"
  ]

nestedRow :: String -> BriDoc
nestedRow prefix = BDCols ColTuple
  [ row ColRec prefix
  , BDLit $ Text.pack " tail"
  ]

allColumnSignatures :: [ColSig]
allColumnSignatures =
  [ ColTyOpPrefix
  , ColPatternsFuncPrefix
  , ColPatternsFuncInfix
  , ColPatterns
  , ColCasePattern
  , ColBindingLine [OptionalAlignment $ Right ()]
  , ColGuard
  , ColGuardedBody
  , ColBindStmt
  , ColDoLet
  , ColRec
  , ColRecUpdate
  , ColRecDecl
  , ColListComp
  , ColList
  , ColApp $ Text.pack "application"
  , ColTuple
  , ColTuples
  , ColOpPrefix
  , ColImport
  ]
