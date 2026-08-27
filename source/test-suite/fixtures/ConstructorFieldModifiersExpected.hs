{-# LANGUAGE StrictData #-}
module ConstructorFieldModifiers where

data Env = Env
  { lazyField     :: ~Int
  , strictField   :: !Int
  , unpackedField :: {-# UNPACK #-} !Int
  , noUnpackField :: {-# NOUNPACK #-} !Int
  }
