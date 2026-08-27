{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module OpaqueSyntax where

data Holder value = Holder { held :: value }

type GeneratedType = $(pure [t|Either Int String|])

expressionQuote value=[| value + 1 |]

expressionSplice value=Holder{held = $(pure [| value |])}

interpolation name = [interpolate|Hello,  ${name}!
Keep  these   spaces.|]

matches [uri|https://example.test/a//b?x=1|] = True
matches _ = False

$(pure [])
