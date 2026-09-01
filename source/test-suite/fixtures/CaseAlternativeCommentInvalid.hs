module CaseAlternativeCommentInvalid where

value input = case input of
  -- This comment must survive a parse failure byte-for-byte.
  Just result ->
