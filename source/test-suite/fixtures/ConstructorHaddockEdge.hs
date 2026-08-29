{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
module ConstructorHaddockEdge where

data Mixed
  = -- | A short constructor.
    Short
  | Plain Int
  -- Keep this ordinary constructor comment structural.
  | Ordinary Bool
  | -- | A multiline Haddock comment starts here.
    -- Its continuation remains structural.
    Multiline Char
  | -- | A long prefix constructor.
    Long
      VeryLongFirstArgumentTypeName
      VeryLongSecondArgumentTypeName
      VeryLongThirdArgumentTypeName
  | -- | A record constructor.
    Record
      { recordField :: Int
      }
  | -- | An infix constructor.
    Int :*: Bool
  deriving (Eq, Show)

data Command where
  -- | A documented GADT constructor.
  Run :: VeryLongFirstArgumentTypeName -> Command
  -- Keep this ordinary GADT comment structural.
  Stop :: Command
  deriving (Eq, Show)
