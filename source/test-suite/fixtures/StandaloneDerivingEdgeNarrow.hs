{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE StandaloneDeriving #-}
module StandaloneDerivingEdge where

deriving stock instance
  Eq a => Eq (Wrapper a)

deriving newtype instance
  Ord a => Ord (Wrapper a)

deriving anyclass instance
  Show a => Show (Wrapper a)

deriving via
  (ParameterizedRepresentation a b)
  instance
    (Eq a, Ord b)
    => CompositeClass
         (LongContainer a b)

deriving instance
  forall a b.
  (Eq a, Ord b) => PairClass (Pair a b)

deriving instance {-# OVERLAPPING #-}
  Eq a => Marker (Wrapper a)
