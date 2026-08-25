{-# LANGUAGE QuasiQuotes #-}
module ScopedExpressionFallbackEdge where

nestedAction value = do
  result <- case value of
    item -> do
      -- Keep this comment before the fragment.
      logInfo [i|item=#{item} -- raw content|]
      -- Keep this comment after the fragment.
      pure fallback
  pure result
  where fallback = [i|fallback value|]
