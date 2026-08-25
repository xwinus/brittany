module ConstructorHaddockInvalid where

data Broken
  = -- | A valid constructor comment.
    First
  | -- | The constructor is missing.
  deriving (Eq, Show)
