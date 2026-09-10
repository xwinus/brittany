{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Decl.Infix
  ( layoutInfixPatternHead
  ) where

import qualified Data.Text as Text
import Language.Haskell.Brittany.Internal.Delimiter.Types
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types

layoutInfixPatternHead
  :: Text -> [BriDocNumbered] -> ToBriDocM (Maybe BriDocNumbered)
layoutInfixPatternHead operator (left : right : arguments) = do
  -- The enclosing equation supplies the continuation base for the split head.
  let operands = docPar (pure left)
        $ docSeq [appSep $ docLit operator, pure right]
  fmap Just $ case arguments of
    [] -> operands
    _ -> docAddBaseY BrIndentRegular
      $ docPar
          (docDelimitedSequence
            ParenthesesDelimiter
            (Text.pack "(")
            (Text.pack ")")
            Nothing
            [(Nothing, PresentDelimiterChild, operands)]
            []
            DelimiterIndentRegular
            PatternBlockDelimiterChild
            [DelimiterAttached])
          (docLines $ pure <$> arguments)
layoutInfixPatternHead _ _ = pure Nothing
