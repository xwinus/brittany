{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeOperators #-}
module SignaturePostDocEdge where

constant
    :: Bool
    -- ^ constant result
constant = True

constrained
    :: forall a
     . (Eq a, Show a)
    => a
    -- ^ source value
    -> Maybe a ::: Either a a
    -- ^ transformed value
constrained = undefined

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
