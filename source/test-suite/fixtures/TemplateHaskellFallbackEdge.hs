{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module TemplateHaskellFallbackEdge where

data Holder value = Holder
  { held :: value
  }

nested action flag value = do
  -- Keep this comment before the strict binding.
  let
    !untypedQuote =
      [| -- Keep the untyped bracket comment.
         value
       |]
  -- Keep this comment after the strict binding.
  typedQuote <- case flag of
    True -> pure
      [|| -- Keep the typed bracket comment.

          value
        ||]
    False -> pure [||value||]
  pure (action, untypedQuote, typedQuote, typedSplice)
 where
  typedSplice = $$(pure [||value||])
  untypedSplice =
    $( -- Keep the untyped splice comment.
     pure [|value|]
     )

recordFallback value = Holder { held = $(pure [|value|]) }
