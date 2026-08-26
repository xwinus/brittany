{-# LANGUAGE ExistentialQuantification #-}
module DocumentedSingleConstructorUnsupported where

-- | Existential constructors retain their safe declaration fallback.
data Unsupported
    = forall value. Show value => Unsupported value
