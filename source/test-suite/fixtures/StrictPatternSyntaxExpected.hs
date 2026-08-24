{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE ViewPatterns #-}
module StrictPatternSyntaxExpected where
strictIdentity !value = value
lazyIdentity ~value = value
isEmpty (length -> 0) = True
isEmpty _             = False
