{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module QuantifiedTypesInvalid where

broken :: (forall a. Eq a =>) => Bool
