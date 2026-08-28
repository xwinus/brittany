{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module PatternSynonymBindingEdge where

pattern Present    value <-Just value
pattern left :>: right = (left,right)
pattern left `PairWith` right = (left,right)
pattern Pair {pairFirst,pairSecond} = (pairFirst,pairSecond)
-- | Keep the declaration documentation.
pattern Signed value<-(asSigned->value) where
 -- Keep the leading builder comment.
 Signed (Negative value) = -value
 Signed Zero = 0 -- Keep the trailing builder comment.
pattern Commented value <-
 Just
  -- Keep the RHS argument comment.
  value
