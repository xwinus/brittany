module ExactPrintCompatSpec (spec) where

import qualified Data.List as List
import qualified GHC.Data.FastString as FastString
import qualified GHC.Data.Strict as Strict
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as EP
import Language.Haskell.Brittany.Internal.SourceComment.Types
  ( SourceCommentKey(..) )
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "structural annotation key ordering" $ do
  Hspec.it "orders real spans by source coordinates numerically" $ do
    keyAt "Node" 2 `Hspec.shouldSatisfy` (< keyAt "Node" 10)

  Hspec.it "uses constructor names after equal span lists" $ do
    keyAt "Alpha" 3 `Hspec.shouldSatisfy` (< keyAt "Beta" 3)

  Hspec.it "compares every span in a multi-span key" $ do
    let prefix = sourceSpan "Multi.hs" 1 1 1 2
        left = EP.AnnKey [prefix, sourceSpan "Multi.hs" 2 1 2 2] (EP.CN "Node")
        right = EP.AnnKey [prefix, sourceSpan "Multi.hs" 3 1 3 2] (EP.CN "Node")
    left `Hspec.shouldSatisfy` (< right)

  Hspec.it "normalizes buffer offsets in named annotation keys" $ do
    let plain = sourceSpan "Buffer.hs" 4 1 4 5
        buffered = withBuffer 20 24 plain
        left = EP.mkNamedAnnKey "Node" plain
        right = EP.mkNamedAnnKey "Node" buffered
    left `Hspec.shouldBe` right
    compare left right `Hspec.shouldBe` EQ

  Hspec.it "normalizes buffer offsets in source comment keys" $ do
    let plain = sourceSpan "Comment.hs" 4 1 4 5
        left = SourceCommentKey plain
        right = SourceCommentKey $ withBuffer 20 24 plain
    left `Hspec.shouldBe` right
    compare left right `Hspec.shouldBe` EQ

  Hspec.it "orders generated and descriptive unhelpful spans deterministically" $ do
    let generated = unhelpfulKey SrcLoc.UnhelpfulGenerated
        alpha = unhelpfulKey
          $ SrcLoc.UnhelpfulOther $ FastString.mkFastString "alpha"
        beta = unhelpfulKey
          $ SrcLoc.UnhelpfulOther $ FastString.mkFastString "beta"
    generated `Hspec.shouldSatisfy` (< alpha)
    alpha `Hspec.shouldSatisfy` (< beta)

  Hspec.it "returns EQ exactly when representative keys are equal" $ do
    and
      [ (compare left right == EQ) == (left == right)
      | left <- representativeKeys
      , right <- representativeKeys
      ] `Hspec.shouldBe` True

  Hspec.it "is antisymmetric for representative keys" $ do
    and
      [ compare left right == invertOrdering (compare right left)
      | left <- representativeKeys
      , right <- representativeKeys
      ] `Hspec.shouldBe` True

  Hspec.it "is transitive for representative keys" $ do
    and
      [ not (left <= middle && middle <= right) || left <= right
      | left <- representativeKeys
      , middle <- representativeKeys
      , right <- representativeKeys
      ] `Hspec.shouldBe` True

  Hspec.it "produces a deterministic order independent of insertion order" $ do
    let ascending = List.sort representativeKeys
    List.sort (reverse representativeKeys) `Hspec.shouldBe` ascending

representativeKeys :: [EP.AnnKey]
representativeKeys =
  [ keyAt "Alpha" 2
  , keyAt "Beta" 2
  , keyAt "Node" 10
  , EP.AnnKey
      [sourceSpan "Representative.hs" 2 1 2 2, sourceSpan "Representative.hs" 3 1 3 2]
      (EP.CN "Node")
  , unhelpfulKey SrcLoc.UnhelpfulNoLocationInfo
  , unhelpfulKey SrcLoc.UnhelpfulGenerated
  , unhelpfulKey
      $ SrcLoc.UnhelpfulOther $ FastString.mkFastString "generated fixture"
  ]

keyAt :: String -> Int -> EP.AnnKey
keyAt constructorName line = EP.AnnKey
  [sourceSpan "Representative.hs" line 1 line 8]
  (EP.CN constructorName)

unhelpfulKey :: SrcLoc.UnhelpfulSpanReason -> EP.AnnKey
unhelpfulKey reason = EP.AnnKey
  [SrcLoc.UnhelpfulSpan reason]
  (EP.CN "Node")

sourceSpan :: FilePath -> Int -> Int -> Int -> Int -> SrcLoc.SrcSpan
sourceSpan filename startLine startColumn endLine endColumn =
  EP.realSpanToSrcSpan $ SrcLoc.mkRealSrcSpan
    (SrcLoc.mkRealSrcLoc file startLine startColumn)
    (SrcLoc.mkRealSrcLoc file endLine endColumn)
 where
  file = FastString.mkFastString filename

withBuffer :: Int -> Int -> SrcLoc.SrcSpan -> SrcLoc.SrcSpan
withBuffer start end (SrcLoc.RealSrcSpan realSpan _) = SrcLoc.RealSrcSpan
  realSpan
  (Strict.Just $ SrcLoc.BufSpan (SrcLoc.BufPos start) (SrcLoc.BufPos end))
withBuffer _ _ sourceSpanValue = sourceSpanValue

invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering EQ = EQ
invertOrdering GT = LT
