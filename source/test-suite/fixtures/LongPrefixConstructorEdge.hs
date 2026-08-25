{-# LANGUAGE GADTs #-}
module LongPrefixConstructorEdge where

data Mixed
  = Short Int
  | Long
      VeryLongFirstArgumentTypeName
      -- Keep this comment with the next argument.
      VeryLongSecondArgumentTypeName
      VeryLongThirdArgumentTypeName
  | Final

data Request where
  ShortRequest :: Int -> Request
  -- | A long request constructor.
  LongRequest
    :: VeryLongFirstArgumentTypeName
    -- Keep this comment with the following GADT argument.
    -> VeryLongSecondArgumentTypeName
    -> VeryLongThirdArgumentTypeName
    -> Request
