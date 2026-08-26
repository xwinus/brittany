{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module TypeOperatorRecordEdge where

import           Data.Kind                                ( Type )

type left :++: right = Either left right
type left `Named` right = Either left right

data OperatorRecord = OperatorRecord
    { symbolic      :: Maybe Int :++: Either Bool Char
    , backticked    :: Maybe Int `Named` Either Bool Char
    , parenthesized :: (Int :++: Bool) `Named` Char
    , kinded        :: ((Int :++: Bool) :: Type)
    , promoted      :: Int ': '[Bool , Char]
    , commented
          :: Maybe Int
             -- Keep the operator comment.
             -- Keep the right operand comment.
          `Named` Either Bool Char
    }
