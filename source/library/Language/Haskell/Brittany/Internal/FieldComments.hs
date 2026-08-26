{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.FieldComments
  ( markFieldPostDocs
  ) where

import qualified Data.Char as Char
import qualified Data.Map as Map
import Language.Haskell.Brittany.Internal.ExactPrintCompat
import Language.Haskell.Brittany.Internal.Prelude

markFieldPostDocs :: Anns -> Anns
markFieldPostDocs = Map.mapWithKey markAnnotation
 where
  markAnnotation (AnnKey _ constructorName) annotation
    | unConName constructorName == "HsConDeclRecField" = annotation
        { annFollowingComments = markComments
            $ annFollowingComments annotation
        , annsDP = markInnerComments $ annsDP annotation
        }
    | otherwise = annotation

  markComments = fmap $ \(comment, dp) -> (markComment comment, dp)
  markInnerComments = fmap $ \case
    (AnnComment comment, dp) -> (AnnComment $ markComment comment, dp)
    entry -> entry

  markComment comment
    | isFieldPostDoc comment =
        comment { commentOrigin = Just AnnFieldPostDoc }
    | otherwise = comment

  isFieldPostDoc = isPostDocText . dropWhile Char.isSpace . commentContents

  isPostDocText = \case
    '-' : '-' : rest -> startsWithCaret rest
    '{' : '-' : rest -> startsWithCaret rest
    _ -> False

  startsWithCaret rest = case dropWhile Char.isSpace rest of
    '^' : _ -> True
    _ -> False
