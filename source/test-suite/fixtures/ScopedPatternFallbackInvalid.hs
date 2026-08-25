{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module ScopedPatternFallbackInvalid where

broken = \case
  [qq|unterminated -> True
  _ -> False
