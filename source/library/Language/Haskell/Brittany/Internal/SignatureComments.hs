{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.SignatureComments
  ( markSignaturePostDocs
  ) where

import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.ExactPrintCompat
import Language.Haskell.Brittany.Internal.Prelude

markSignaturePostDocs :: Anns -> Anns
markSignaturePostDocs annotations = Map.mapWithKey markAnnotation annotations
 where
  signatureSpans =
    [ span'
    | key@(AnnKey _ constructorName) <- Map.keys annotations
    , unConName constructorName == "SigD"
    , Just span' <- [annKeyRealSpan key]
    ]

  markAnnotation key@(AnnKey _ constructorName) annotation
    | keyInsideSignature key = annotation
        { annFollowingComments = markComments origin
            $ annFollowingComments annotation
        , annPriorComments = markComments origin
            $ annPriorComments annotation
        , annsDP = markInnerComments origin $ annsDP annotation
        }
    | otherwise = annotation
   where
    origin
      | unConName constructorName == "SigD" = AnnSignatureFinalPostDoc
      | otherwise = AnnSignaturePostDoc

  keyInsideSignature key = case annKeyRealSpan key of
    Nothing -> False
    Just keySpan -> any (containsSpan keySpan) signatureSpans

  markComments origin = fmap $ \(comment, dp) ->
    (markComment origin comment, dp)
  markInnerComments origin = fmap $ \case
    (AnnComment comment, dp) -> (AnnComment $ markComment origin comment, dp)
    entry -> entry

  markComment origin comment
    | isPostDoc comment = comment { commentOrigin = Just origin }
    | otherwise = comment

  isPostDoc = isPostDocText . dropWhile Char.isSpace . commentContents

  isPostDocText = \case
    '-' : '-' : rest -> startsWithCaret rest
    '{' : '-' : rest -> startsWithCaret rest
    _ -> False

  startsWithCaret rest = case dropWhile Char.isSpace rest of
    '^' : _ -> True
    _ -> False

  containsSpan child parent =
    spanStart child >= spanStart parent && spanEnd child <= spanEnd parent

  spanStart span' =
    (SrcLoc.srcSpanStartLine span', SrcLoc.srcSpanStartCol span')
  spanEnd span' =
    (SrcLoc.srcSpanEndLine span', SrcLoc.srcSpanEndCol span')
