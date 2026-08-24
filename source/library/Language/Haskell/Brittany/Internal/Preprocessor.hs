{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Preprocessor
  ( cppUnsupportedMessage
  ) where

import Prelude (String)

cppUnsupportedMessage :: String
cppUnsupportedMessage =
  "CPP is unsupported. Preprocess the input before running brittany or remove -XCPP."
