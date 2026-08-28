{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedSums #-}
module PatternSynonymBindingUnsupported where

pattern LeftSum value <- (# value | #)
