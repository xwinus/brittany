{-# LANGUAGE QuasiQuotes #-}
module QuasiQuoteSyntaxExpected where
value = [text|alpha beta|]
match [patternQuote|value|] = True
type QuotedType = [typeQuote|value|]
[declarationQuote|
generated = 1
|]
