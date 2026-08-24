{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module ForeignSyntaxEdge where
foreign import ccall safe "math.h sin"
  -- Keep the foreign import comment.
  c_sin :: Double -> IO Double
foreign export ccall "exported"
  -- Keep the foreign export comment.
  exported :: Int -> IO Int
