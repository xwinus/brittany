module ConstructorHaddock where

data Mode
  = -- | Add mode.
    Add
  | -- | Check mode.
    Check
  deriving (Eq, Show)
