{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
module TypeArgumentsEdge where
data Some where
  Some
    :: forall hidden
       -- Keep the invisible constructor binder comment.
     . hidden
    -> Some
data Typed a where
  Typed
    :: forall visible
       -- Keep the visible constructor binder comment.
    -> visible
    -> Typed visible
identity
  :: forall a
     -- Keep the signature binder comment.
   . a
  -> a
identity
  @typeArgument
  -- Keep the type abstraction comment.
  value = value :: typeArgument
requiredIdentity
  :: forall a
     -- Keep the required binder comment.
   -> a
  -> a
requiredIdentity
  (type typeArgument)
  -- Keep the required type pattern comment.
  value = value :: typeArgument
readSome
  (Some
    @contained
    -- Keep the constructor type abstraction comment.
    value) = value `seq` ()
higherRank = requiredIdentity (forall a. a -> a) identity
