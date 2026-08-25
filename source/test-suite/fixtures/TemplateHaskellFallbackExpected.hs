{-# LANGUAGE TemplateHaskell #-}
module TemplateHaskellFallback where

quoteExpVersion txt = [|parseVersionUnsafe txt|]
  where parseVersionUnsafe value = value
