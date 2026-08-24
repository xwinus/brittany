{-# LANGUAGE QuasiQuotes #-}
module QuasiQuoteSyntaxEdge where
value =
  -- Keep the quasiquote boundary comment.
  [text|
content that looks like -- a source comment
content with { brackets } and punctuation
|]
match
  -- Keep the pattern quasiquote comment.
  [patternQuote|value|] = True
