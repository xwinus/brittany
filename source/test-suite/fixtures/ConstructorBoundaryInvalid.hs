{-# LANGUAGE GADTs #-}
module ConstructorBoundaryInvalid where

data Broken
  = Valid
    -- ^ Documents Valid.
  |
    -- | The next constructor is missing.
  deriving (Eq, Show)
