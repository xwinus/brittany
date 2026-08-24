{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module QuantifiedTypesExpected where
compareAll :: (forall a . Eq a => Eq (container a)) => container Int -> Bool
compareAll value = value == value
applyRanked :: (forall a . a -> a) -> (Int, Bool)
applyRanked function = (function @Int 1, function @Bool True)
