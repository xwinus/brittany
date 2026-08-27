module ConstructorFieldModifiersInvalid where

data Broken = Broken
  { brokenField :: {-# UNPACK #-} !
  }
