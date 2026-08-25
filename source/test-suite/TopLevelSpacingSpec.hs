module TopLevelSpacingSpec (spec) where

import qualified GHC.Data.FastString as FastString
import GHC.Types.SrcLoc (RealSrcSpan, mkRealSrcLoc, mkRealSrcSpan)
import Language.Haskell.Brittany.Internal.TopLevelSpacing
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "top-level separator calculation" $ do
  Hspec.it "uses one newline for adjacent source lines" $
    separator (spanAt 3 1 3 12) (spanAt 4 1 4 9)
      `Hspec.shouldBe` 1

  Hspec.it "preserves multiple intentional blank lines" $
    separator (spanAt 3 1 3 12) (spanAt 7 1 7 9)
      `Hspec.shouldBe` 4

  Hspec.it "treats a column-one multiline end as exclusive" $
    separator (spanAt 3 1 5 1) (spanAt 6 1 6 9)
      `Hspec.shouldBe` 2

  Hspec.it "uses prior and following comments as effective boundaries" $ do
    let previous = TopLevelUnit
          (spanAt 3 1 3 12)
          []
          [spanAt 4 1 4 20]
        next = TopLevelUnit
          (spanAt 8 1 8 9)
          [spanAt 7 1 7 18]
          []
    topLevelSeparatorLines previous next `Hspec.shouldBe` 3

  Hspec.describe "preamble separator calculation" $ do
    Hspec.it "keeps adjacent preamble units adjacent" $
      preambleSeparatorLines
        (spanAt 2 1 2 30)
        (TopLevelUnit (spanAt 3 1 3 7) [] [])
        `Hspec.shouldBe` 1

    Hspec.it "preserves one intentional blank line" $
      preambleSeparatorLines
        (spanAt 2 1 2 30)
        (TopLevelUnit (spanAt 4 1 4 7) [] [])
        `Hspec.shouldBe` 2

    Hspec.it "caps larger preamble gaps at two blank lines" $
      preambleSeparatorLines
        (spanAt 2 1 2 30)
        (TopLevelUnit (spanAt 9 1 9 7) [] [])
        `Hspec.shouldBe` 3

    Hspec.it "uses comments owned by an implicit module follower" $
      preambleSeparatorLines
        (spanAt 2 1 2 30)
        (TopLevelUnit (spanAt 4 1 4 7) [spanAt 3 1 3 24] [])
        `Hspec.shouldBe` 1

separator :: RealSrcSpan -> RealSrcSpan -> Int
separator previous next = topLevelSeparatorLines
  (TopLevelUnit previous [] [])
  (TopLevelUnit next [] [])

spanAt :: Int -> Int -> Int -> Int -> RealSrcSpan
spanAt startLine startColumn endLine endColumn = mkRealSrcSpan
  (mkRealSrcLoc testFile startLine startColumn)
  (mkRealSrcLoc testFile endLine endColumn)
 where
  testFile = FastString.fsLit "TopLevelSpacingSpec.hs"
