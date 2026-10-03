{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Pattern.Prefix
  ( layoutStructuralPrefixPattern
  ) where

import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types

layoutStructuralPrefixPattern
  :: Text -> [BriDocNumbered] -> ToBriDocM BriDocNumbered
layoutStructuralPrefixPattern name arguments = do
  nameDocument <- docLit name
  prefixes <- scanM appendArgument nameDocument arguments
  -- Keep a nonempty continuation: the caller already offers the complete
  -- compact pattern and accounts for its following arrow or delimiter.
  -- Flush pending indentation before anchoring to the constructor column.
  docSeq
    [ docLitS ""
    , docSetBaseY $ docAlt
      [ docAddBaseY BrIndentRegular $ docPar
          (docForceSingleline $ pure prefix)
          (docSetIndentLevel $ docLines $ pure <$> remaining)
      | (prefix, remaining) <- reverse $ zip prefixes $ tails arguments
      , not $ null remaining
      ]
    ]
 where
  appendArgument prefix argument = docSeq
    [appSep $ pure prefix, pure argument]

  scanM _ initial [] = pure [initial]
  scanM step initial (argument : remaining) = do
    next <- step initial argument
    (initial :) <$> scanM step next remaining

  tails [] = [[]]
  tails values@(_ : remaining) = values : tails remaining
