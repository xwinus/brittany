{-# LANGUAGE TypeOperators #-}
module TypeOperatorRecordInvalid where

data Broken = Broken
  { field :: left :::
  }
