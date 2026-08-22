{-# LANGUAGE CPP #-}
module CompatibilityCppUnsupported where

#if defined(EXAMPLE)
example = True
#else
example = False
#endif
