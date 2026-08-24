{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module ForeignSyntaxExpected where
foreign import ccall safe "math.h sin"
  c_sin :: Double -> IO Double
foreign import capi unsafe "math.h cos"
  c_cos :: Double -> IO Double
foreign export ccall "exported"
  exported :: Int -> IO Int
