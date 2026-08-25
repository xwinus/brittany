{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module ScopedPatternFallback where

spec = do
  describe "URI matcher" $ do
    it "recognizes a template URI" $ do
      let classify = \case
            [uri|http://test.com/haskell.mustache|] -> True
            _ -> False
      classify value `shouldBe` True
