{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module ForeignSyntaxInvalid where

foreign import ccall "broken"
  broken ::
