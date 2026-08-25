{-# LANGUAGE GADTs #-}
module LeadingDataCommentEdge where

---------------------------------  DATA TYPES  ---------------------------------

-- | Standard command.
data Standard
  =
    -- | A short command.
    Short Int
  |
    -- | A long command.
    Long
      VeryLongFirstArgumentTypeName
      VeryLongSecondArgumentTypeName
      VeryLongThirdArgumentTypeName
  | Record
      { firstField  :: Int
      -- Keep this field comment with secondField.
      , secondField :: Bool
      }
  deriving (Eq, Show) -- Keep this trailing declaration comment.

-- This ordinary comment belongs to Request.
-- | GADT request.
data Request where
  -- | A short request.
  ShortRequest :: Int -> Request
  -- | A long request.
  LongRequest
    :: VeryLongFirstArgumentTypeName
    -> VeryLongSecondArgumentTypeName
    -> VeryLongThirdArgumentTypeName
    -> Request
  deriving (Eq, Show)
