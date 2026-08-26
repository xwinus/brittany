{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.TypeOperatorComments
  ( markTypeOperatorComments
  ) where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.ExactPrintCompat
import Language.Haskell.Brittany.Internal.Prelude

markTypeOperatorComments :: Anns -> Anns
markTypeOperatorComments annotations = Map.map markAnnotation annotations
 where
  operatorSpans = mapMaybe operatorSpan $ Map.keys annotations

  operatorSpan key@(AnnKey _ constructorName)
    | unConName constructorName == "HsOpTy" = annKeyRealSpan key
    | otherwise = Nothing

  markAnnotation annotation = annotation
    { annFollowingComments = markComments $ annFollowingComments annotation
    , annPriorComments = markComments $ annPriorComments annotation
    , annsDP = markInnerComments $ annsDP annotation
    }

  markComments = fmap $ \(comment, dp) -> (markComment comment, dp)
  markInnerComments = fmap $ \case
    (AnnComment comment, dp) -> (AnnComment $ markComment comment, dp)
    entry -> entry

  markComment comment
    | any (containsComment comment) operatorSpans =
        comment { commentOrigin = Just AnnTypeOperatorComment }
    | otherwise = comment

  containsComment comment parentSpan = case
    srcSpanToRealSpan $ commentIdentifier comment of
      Just commentSpan ->
        spanStart commentSpan >= spanStart parentSpan
          && spanEnd commentSpan <= spanEnd parentSpan
      Nothing -> False

  spanStart span' =
    (SrcLoc.srcSpanStartLine span', SrcLoc.srcSpanStartCol span')
  spanEnd span' =
    (SrcLoc.srcSpanEndLine span', SrcLoc.srcSpanEndCol span')
