{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE NumericUnderscores #-}
module LiteralSyntaxExpected where
binary = 0b1010_0110
hexFloat = 0x1.ffp+2
machineInt = 3#
machineWord = 4##
machineFloat = 1.5#
machineDouble = 1.5##
sizedInt = 5#Int
sizedInt8 = -12#Int8
sizedInt16 = 12#Int16
sizedInt32 = 12#Int32
sizedInt64 = 12#Int64
sizedWord = 6#Word
sizedWord8 = 0xff#Word8
sizedWord16 = 12#Word16
sizedWord32 = 12#Word32
sizedWord64 = 12#Word64
message = """
  first line
  second line
  """
