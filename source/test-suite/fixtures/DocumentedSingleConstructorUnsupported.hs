{-# LANGUAGE ExistentialQuantification #-}
module DocumentedSingleConstructorUnsupported where

-- | Existential constructors remain composable on the native path.
data Unsupported
    = forall value. Show value => Unsupported value
