{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module QuantifiedTypesEdge where
compareAll
  :: ( forall a
       -- Keep the quantified constraint binder comment.
     . Eq a
    => Eq (container a)
     )
  => container Int
  -> Bool
compareAll value = value == value
applyRanked
  :: ( forall a
       -- Keep the rank-n binder comment.
     . a
    -> a
     )
  -> (Int, Bool)
applyRanked function =
  ( function @Int
      -- Keep the visible type application comment.
                  1
  , function @Bool True
  )
