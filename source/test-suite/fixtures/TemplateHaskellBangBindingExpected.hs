{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module TemplateHaskellBangBinding where

pvp =
  QuasiQuoter
    { quoteExp  = quoteExpVersion
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }
 where
  quoteExpVersion txt = [|parseVersionUnsafe . pack $ txt|]
    where !_ = parseVersionUnsafe . pack $ txt -- check at compile time

re =
  QuasiQuoter
    { quoteExp  = quoteExpRegex
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }
 where
  quoteExpRegex txt = [|compileRegex txt|]
   where
    !validated = compileRegex txt -- check at compile time
    checked    = validated
