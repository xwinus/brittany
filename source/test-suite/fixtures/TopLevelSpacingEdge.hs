{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module TopLevelSpacingEdge where

import           Data.List                                ( sort )
import           Data.Maybe                               ( fromMaybe )

compact :: Int
compact = 1

-- | Keep this Haddock comment with documented.
documented :: Int
documented = compact


-- Keep two blank lines before this comment.
sorted :: [Int] -> [Int]
sorted = sort

defaulted :: Int
defaulted = fromMaybe compact Nothing

$(pure [])

convert flag value = if flag then 'y' else 'n'
{-# SPECIALISE
  convert
    -- Keep the specialised expression comment.
    @Int False
  :: Int -> Char
  #-}
