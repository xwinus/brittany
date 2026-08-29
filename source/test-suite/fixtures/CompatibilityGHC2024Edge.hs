{-# LANGUAGE GHC2024 #-}
module CompatibilityGHC2024 where
newtype Tagged tag value = Tagged
  {
  -- Keep the field comment.
    unTagged :: value
  }
