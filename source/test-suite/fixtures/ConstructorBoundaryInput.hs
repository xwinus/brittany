{-# LANGUAGE GADTs #-}
module ConstructorBoundary where

data Choice
  = First
    -- ^ Documents First.
  |
    -- | Documents Second.
    Second Int
  |
    -- Keep this ordinary comment with Third.
    Third Bool
    -- ^ Documents Third.
  deriving (Eq, Show)

data Request where
  -- | Reads a request.
  Read :: Int -> Request
  -- Keep this ordinary comment with Stop.
  Stop :: Request
  deriving (Eq, Show)
