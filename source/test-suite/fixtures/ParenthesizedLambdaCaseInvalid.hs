{-# LANGUAGE LambdaCase #-}
module ParenthesizedLambdaCaseInvalid where

broken = runAction `shouldThrow` (
  \case
    Just value -> True
    Nothing ->
