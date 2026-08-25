{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeOperators #-}
module FinalResultHaddockEdge where

constant
    :: Bool
    -- ^ constant result
constant = True

constrained
    :: forall a
     . (Eq a, Show a)
    => a
    -- ^ source value
    -> a
    -- ^ transformed value
constrained = id

(<==>)
    :: Int
    -- ^ left operand
    -> Int
    -- ^ right operand
    -> Bool
    -- ^ comparison result
(<==>) = (==)

leftValue, rightValue
    :: Int
    {-^ shared result -}
leftValue = 1
rightValue = 2

documented
    :: Int
    {-^ result documentation
        continued on another line
    -}
-- | Documents the following declaration.
documented = 3

native :: Int -> Int
native = id
