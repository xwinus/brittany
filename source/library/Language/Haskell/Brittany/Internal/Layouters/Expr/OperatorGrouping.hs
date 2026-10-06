{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Expr.OperatorGrouping
  ( OperatorChainPart(..)
  , groupedOperatorChain
  ) where

import qualified Data.List as List
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types

-- These are visual break preferences, not inferred semantic fixities. The
-- original token order and parentheses are preserved, including shadowed names.
data OperatorChainPart = OperatorChainPart
  { chainOperatorName :: Maybe String
  , chainOperatorDoc :: ToBriDocM BriDocNumbered
  , chainOperandDoc :: ToBriDocM BriDocNumbered
  , chainAllowsBreak :: Bool
  , chainBlockOperand :: Bool
  , chainLiteralOperand :: Bool
  , chainOrdinaryLambdaOperand :: Bool
  }

groupedOperatorChain
  :: (OperatorChainPart -> ToBriDocM BriDocNumbered)
  -> ToBriDocM BriDocNumbered
  -> [OperatorChainPart]
  -> Maybe (ToBriDocM BriDocNumbered)
groupedOperatorChain continuation left parts = do
  priorities <- traverse (chainOperatorName >=> groupingPriority) parts
  priority <- minimumMaybe priorities
  if priority >= 3 || 3 `notElem` priorities || any chainBlockOperand parts
    then Nothing
    else do
      let (headParts, remaining) = List.span ((/= Just priority) . partPriority) parts
          groups = splitGroups priority remaining
      if null headParts && all ((<= 1) . length) groups
        then Nothing
        else Just $ if any membershipPart headParts
            && all ((== Just 3) . partPriority) headParts
          then docAlt
            [ docAddBaseY BrIndentRegular $ docPar
                (docForceSingleline $ docSeq $ List.intersperse docSeparator
                  $ left : List.concatMap inlinePart headParts)
                (docLines $ map row groups)
            -- An unfit first predicate shares the existing continuation base;
            -- a nested paragraph would add the outer indentation to it again.
            , docAddBaseY BrIndentRegular $ docPar left
                $ docLines $ map continuation headParts ++ map row groups
            ]
          else docAddBaseY BrIndentRegular
            $ docPar (unit BrIndentNone left headParts)
            $ docLines $ map row groups
 where
  membershipPart part = chainOperatorName part `elem` [Just "elem", Just "notElem"]
  partPriority = chainOperatorName >=> groupingPriority
  minimumMaybe [] = Nothing
  minimumMaybe values = Just $ minimum values
  splitGroups _ [] = []
  splitGroups priority (first : rest) =
    let (inside, remaining) = List.span ((/= Just priority) . partPriority) rest
    in (first : inside) : splitGroups priority remaining
  unit _ first [] = first
  unit indent first rest = docAlt $
    [ docForceSingleline $ docSeq $ List.intersperse docSeparator
        $ first : List.concatMap inlinePart rest
    ]
    -- Each nested unit has a strictly higher break priority, so recursion is
    -- bounded by the fixed priority tiers rather than the chain's length.
    ++ case groupedOperatorChain continuation first rest of
      Just grouped -> [grouped]
      Nothing ->
        let multiline = docPar first $ docLines $ map continuation rest
        in [docAddBaseY indent multiline]
          -- A terminal predicate may need the boolean row's full width.
          -- Keep this fallback local so other fitting groups stay cohesive.
          ++ [multiline | indent /= BrIndentNone && any membershipPart rest]

  inlinePart part = [chainOperatorDoc part, chainOperandDoc part]
  row [] = docEmpty
  row (first : rest) =
    let operand = unit BrIndentRegular (chainOperandDoc first) rest
        grouped = first
          { chainOperandDoc = operand
          , chainAllowsBreak = null rest && chainAllowsBreak first
          -- Capture the structured group's base before its outer operator,
          -- rather than adding that operator's hanging column to every row.
          , chainBlockOperand = not (null rest) || chainBlockOperand first
          , chainLiteralOperand = null rest && chainLiteralOperand first
          , chainOrdinaryLambdaOperand = null rest && chainOrdinaryLambdaOperand first
          }
    in if null rest then continuation grouped
      else docSetBaseY $ continuation grouped

-- Unknown or qualified operators are barriers: their visual grouping is not
-- guessed from the parser's left-associated OpApp spine.
groupingPriority :: String -> Maybe Int
groupingPriority operator
  | operator `elem` ["$", "$!"] = Just 0
  | operator `elem` ["||", "&&"] = Just 1
  | operator `elem` ["<$>", "<$", "$>", "<*>", "<*", "*>", "<|>"] = Just 2
  | operator `elem`
      [ "==", "/=", "<", "<=", ">", ">=", "elem", "notElem"
      , ".:", ".:?", ".!=", ".="
      ] = Just 3
  | otherwise = Nothing
