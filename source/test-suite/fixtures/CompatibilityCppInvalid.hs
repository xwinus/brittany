{-# LANGUAGE CPP #-}
module CompatibilityCppInvalid where

#if defined(EXAMPLE
example = True
#endif
