{-# LANGUAGE ForeignFunctionInterface #-}
foreign import ccall "example"
  -- Keep this foreign declaration comment.
  example :: IO ()
