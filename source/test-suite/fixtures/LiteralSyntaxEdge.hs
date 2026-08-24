{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE NumericUnderscores #-}
module LiteralSyntaxEdge where
total =
  ( 1_000
    -- Keep the numeric operator boundary comment.
    + 0b1010_0110
  )
smallWord =
  -- Keep the sized primitive literal comment.
  0xff#Word8
machineInt =
  -- Keep the machine primitive literal comment.
  3#
message =
  """
  text that looks like -- a comment stays text
  closing delimiters stay aligned
  """
