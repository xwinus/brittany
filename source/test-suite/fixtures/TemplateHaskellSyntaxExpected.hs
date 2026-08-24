{-# LANGUAGE TemplateHaskell #-}
module TemplateHaskellSyntaxExpected where
expressionQuote = [| 1 + 2 |]
typedQuote = [|| 1 + 2 ||]
expressionSplice = $(pure [| 1 |])
typedSplice = $$(pure [|| 1 ||])
quotedName = 'map
quotedType = ''Maybe
declarations = [d|
  generated = 1
  |]
$(pure [])
