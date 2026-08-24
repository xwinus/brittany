{-# LANGUAGE LambdaCase #-}
module ParenthesizedLambdaCaseExpected where

check =
  runAction
    `shouldThrow` (
      \case
        InitializationFileAlreadyExists paths -> existingTemplate `elem` paths
        _ -> False
    )
