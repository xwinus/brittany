{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.DerivingComments
  ( markDerivingComments
  ) where

import qualified Data.Map as Map
import Language.Haskell.Brittany.Internal.ExactPrintCompat
import Language.Haskell.Brittany.Internal.Prelude

markDerivingComments :: Anns -> Anns
markDerivingComments = Map.mapWithKey markAnnotation
 where
  markAnnotation (AnnKey _ constructorName) annotation
    | unConName constructorName == "HsDerivingClause" = annotation
        { annFollowingComments = markComments
            $ annFollowingComments annotation
        , annPriorComments = markComments $ annPriorComments annotation
        , annsDP = markInnerComments $ annsDP annotation
        }
    | otherwise = annotation

  markComments = fmap $ \(comment, dp) -> (markComment comment, dp)
  markInnerComments = fmap $ \case
    (AnnComment comment, dp) -> (AnnComment $ markComment comment, dp)
    entry -> entry

  markComment comment =
    comment { commentOrigin = Just AnnDerivingComment }
