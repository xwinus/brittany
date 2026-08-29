{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
module DocumentedSingleConstructorEdge where

-- | A positional declaration.
data Positional
    = -- | A strict positional constructor.
        Positional !Int {-# UNPACK #-} !Int
    deriving stock (Eq, Show)

-- | A record declaration.
data NativeRecord
    -- Keep this constructor comment structural.
    = NativeRecord
          { strictField   :: !Int
              -- ^ Strict field documentation.
          , unpackedField :: {-# UNPACK #-} !Int
              -- ^ Unpacked field documentation.
          }
    deriving stock (Eq, Show)

-- | A basic single-constructor GADT remains native.
data BasicGadt where
    -- | The only GADT constructor.
    BasicGadt :: Int -> BasicGadt
    deriving stock (Eq, Show)
