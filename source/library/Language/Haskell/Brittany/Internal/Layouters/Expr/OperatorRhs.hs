{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Expr.OperatorRhs
  ( OperatorRhsBreak(..)
  , layoutOperatorRhs
  ) where

import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types

-- Compound predicates gain only a compact continuation. Their existing
-- multiline layout remains the fallback when the whole predicate cannot fit.
data OperatorRhsBreak = NoRhsBreak | CompactRhsBreak | FlexibleRhsBreak
  deriving (Eq)

layoutOperatorRhs
  :: OperatorRhsBreak
  -> Bool
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
layoutOperatorRhs breakStyle literalOperand operator operand attached
  | breakStyle == NoRhsBreak = attached
  | literalOperand = docAlt [attached, detached]
  | otherwise = docAlt $
      [ docForceSingleline $ docCols ColOpPrefix [appSep operator, operand]
      , docParIndented BrIndentRegular operator $ docForceSingleline operand
      -- Prefer a whole operand at either actual column before fragmenting it.
      , attached
      ]
      -- Preserve the hanging fallback when no candidate can fit the width.
      ++ if breakStyle == FlexibleRhsBreak then [detached, attached] else []
 where
  detached = docParIndented BrIndentRegular operator operand
