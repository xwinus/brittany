{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module OrPatternSyntaxExpected where
pattern Present value = Just value
classify (Nothing ; Just 0) = "empty"
classify (Just value)       = show value
