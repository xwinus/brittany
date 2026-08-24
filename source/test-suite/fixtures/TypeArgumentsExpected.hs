{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
module TypeArgumentsExpected where
data Some where
  Some :: forall a. a -> Some
data Typed a where
  Typed :: forall a -> a -> Typed a
identity :: forall a . a -> a
identity @typeArgument value = value :: typeArgument
requiredIdentity :: forall a -> a -> a
requiredIdentity (type typeArgument) value = value :: typeArgument
readSome (Some @contained value) = value `seq` ()
visibleApplication = identity @Int 42
requiredApplication = requiredIdentity Int 42
