{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.SourceComment.LineBoundary
  ( priorCommentRequiresLineBoundary
  , sourceFragmentRequiresLineBoundary
  ) where

import qualified Data.Text as Text
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types

priorCommentRequiresLineBoundary :: String -> Bool
priorCommentRequiresLineBoundary comment = case
  dropWhile (`elem` [' ', '\t']) comment of
  '-' : '-' : _ -> True
  '#' : _ -> True
  _ -> False

sourceFragmentRequiresLineBoundary :: ExactSourceFragment -> Bool
sourceFragmentRequiresLineBoundary fragment = case reverse
  $ Text.lines
  $ fragmentText fragment <> Text.singleton '\n' of
  lastLine : _ -> priorCommentRequiresLineBoundary $ Text.unpack lastLine
  [] -> False
