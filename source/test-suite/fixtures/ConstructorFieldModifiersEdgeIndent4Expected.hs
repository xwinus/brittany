-- brittany { lconfig_indentPolicy: IndentPolicyLeft }
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
module ConstructorFieldModifiersEdge where

data Prefix
    = Prefix ~Int !Int {-# UNPACK #-} !Int {-# NOUNPACK #-} !Int
    | Alternate ~Bool

data Infix left right = ~left :*: {-# UNPACK #-} !right

data Record = Record
    { lazyRecord     :: ~Int
    , strictRecord   :: !Int
    , unpackedRecord :: {-# UNPACK #-} !Int
    , noUnpackRecord :: {-# NOUNPACK #-} !Int
    }

data Documented = Documented
    { documentedLazy     :: ~Int
        -- ^ An explicitly lazy field.
    , documentedUnpacked :: {-# UNPACK #-} !Int
        -- ^ An unpacked strict field.
    }

data Gadt where
    Gadt
        :: ~Int
        -> !Bool
        -> {-# UNPACK #-} !Int
        -> {-# NOUNPACK #-} !Int
        -> Gadt
