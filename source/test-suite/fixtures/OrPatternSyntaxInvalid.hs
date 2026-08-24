{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module OrPatternSyntaxInvalid where

classify (Nothing ;) = True
