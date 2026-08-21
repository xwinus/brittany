{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.ExactSource
  ( declarationSourceSlice
  ) where

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified GHC
import GHC.Hs (HsDecl)
import GHC.Parser.Annotation (getLocA)
import Language.Haskell.Brittany.Internal.ExactPrintCompat
  ( Anns
  , commentIdentifier
  )
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as EP
import Language.Haskell.Brittany.Internal.LayouterBasics
  ( extractAllComments
  , isRegularComment
  )
import Language.Haskell.Brittany.Internal.Prelude

declarationSourceSlice
  :: Text.Text -> GHC.Located (HsDecl GhcPs) -> Anns -> Maybe Text.Text
declarationSourceSlice source declaration anns = do
  declarationSpan <- EP.srcSpanToRealSpan $ getLocA declaration
  let
    commentSpans =
      [ span'
      | annotation <- Map.elems anns
      , comment <- extractAllComments annotation
      , isRegularComment comment
      , Just span' <- [EP.srcSpanToRealSpan $ commentIdentifier $ fst comment]
      ]
    spans = declarationSpan : commentSpans
    firstLine = minimum $ GHC.srcSpanStartLine <$> spans
    lastLine = maximum $ GHC.srcSpanEndLine <$> spans
    selectedLines =
      take (lastLine - firstLine + 1)
        $ drop (firstLine - 1)
        $ Text.lines source
  guard $ not $ null selectedLines
  pure $ Text.intercalate (Text.singleton '\n') selectedLines
