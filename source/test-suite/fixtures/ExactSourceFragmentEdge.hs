{-# LANGUAGE DataKinds #-}
module ExactSourceFragmentEdge where

-- | First declaration.
first
  :: '(Int, Bool)
  {-^ same result -}
first = undefined

-- | Second declaration.
second
  :: '(Bool, Int)
  {-^ same result -}
second = undefined

-- This comment is outside both exact-source declarations.
native :: Int
native = 1
