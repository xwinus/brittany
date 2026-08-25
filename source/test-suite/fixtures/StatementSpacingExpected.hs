module StatementSpacingExpected where

spec = do
  describe "feature" $ do
    it "handles the first case" $ do
      prepare
      verify

    it "handles the second case" $ do
      prepareOther
      verifyOther
