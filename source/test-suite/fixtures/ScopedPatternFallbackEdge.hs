{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module ScopedPatternFallbackEdge where

nested action pair = do
  let classify = \case
        ([qq|left|], value) | keep value -> True
                            | otherwise  -> False
        _ -> False
  result <- action
  pure (classify (pair, result))

rawContent = \case
  [qq|
raw -- content
{ braces }
|]
    -> True
  _ -> False
