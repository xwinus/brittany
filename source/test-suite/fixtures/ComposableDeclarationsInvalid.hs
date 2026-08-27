{-# LANGUAGE TypeFamilies #-}
module ComposableDeclarationsInvalid where

type family Broken value where
  Broken Int =
