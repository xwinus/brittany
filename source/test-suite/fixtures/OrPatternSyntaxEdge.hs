{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module OrPatternSyntaxEdge where
pattern Present value <-
  Just
    -- Keep the pattern synonym argument comment.
    value
classify (Nothing
  -- Keep the or-pattern separator comment.
   ; Just 0)                = "empty"
classify (Just value) = show value
