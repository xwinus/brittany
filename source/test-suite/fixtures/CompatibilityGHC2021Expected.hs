{-# LANGUAGE GHC2021 #-}
module CompatibilityGHC2021 where
pair :: left -> right -> (left, right)
pair left right =
  ( left
    -- Keep the tuple comment.
  , right
  )
