{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Expr.OperatorRhs
  ( layoutOperatorRhs ) where

import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types

layoutOperatorRhs
  :: Bool
  -> Bool
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
layoutOperatorRhs allowBreak literalOperand operator operand attached
  | not allowBreak = attached
  | literalOperand = docAlt [attached, detached]
  | otherwise = docAlt
      [ docForceSingleline $ docCols ColOpPrefix [appSep operator, operand]
      , docParIndented BrIndentRegular operator $ docForceSingleline operand
      -- Prefer a whole operand at either actual column before fragmenting it.
      , attached
      , detached
      -- Preserve the hanging fallback when no candidate can fit the width.
      , attached
      ]
 where
  detached = docParIndented BrIndentRegular operator operand
