{-# LANGUAGE NoFieldSelectors #-}
module NoFieldSelectorsInvalid where

data Broken = Broken
  { label ::
  }
