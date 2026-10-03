{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Pattern.Delimited
  ( layoutDelimitedPattern
  ) where

import qualified Data.Text as Text
import GHC.Hs
import Language.Haskell.Brittany.Internal.Delimiter.Types
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as ExactPrintCompat
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.IE (toL)
import Language.Haskell.Brittany.Internal.Layouters.Pattern.Types
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types

layoutDelimitedPattern
  :: (LPat GhcPs -> ToBriDocM PatternLayout)
  -> LPat GhcPs
  -> DelimiterKind
  -> Text
  -> Text
  -> [LPat GhcPs]
  -> ToBriDocM (Maybe BriDocNumbered)
layoutDelimitedPattern _ _ _ _ _ [] = pure Nothing
layoutDelimitedPattern layoutChild outer kind openToken closeToken elements = do
  elementDocs <- mapM (layoutChild >=> patternDocument) elements
  fmap Just $ docWrapNode (toL outer)
    $ docDelimitedSequence
      kind
      openToken
      closeToken
      (Just $ ExactPrintCompat.mkAnnKey $ toL outer)
      (zipWith
        (\element document ->
          ( Just $ ExactPrintCompat.mkAnnKey $ toL element
          , PresentDelimiterChild
          , pure document
          )
        )
        elements
        elementDocs)
      (replicate (max 0 $ length elements - 1)
        (RepeatedDelimiterSeparator, Text.pack ",", separatorAttachment))
      DelimiterIndentRegular
      separatorProfile
      (if length elements == 1 && kind /= SquareBracketsDelimiter
        then [DelimiterCompact]
        else [DelimiterAttached])
 where
  (separatorAttachment, separatorProfile) =
    if kind == SquareBracketsDelimiter
      then (AttachSeparatorRight, LeadingDelimiterSeparators)
      else (AttachSeparatorLeft, TrailingDelimiterSeparators)

