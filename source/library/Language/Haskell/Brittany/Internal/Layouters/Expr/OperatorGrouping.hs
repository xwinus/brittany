{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Expr.OperatorGrouping
  ( OperatorChainPart(..)
  , groupedOperatorChain
  ) where

import Data.Char (isAlpha)
import qualified Data.List as List
import Data.Maybe (isNothing, mapMaybe)
import Language.Haskell.Brittany.Internal.Layouters.Expr.OperatorRhs
  ( OperatorRhsBreak(..) )
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types

-- These are visual break preferences, not inferred semantic fixities. The
-- original token order and parentheses are preserved, including shadowed names.
data OperatorChainPart = OperatorChainPart
  { chainOperatorName :: Maybe String
  , chainOperatorDoc :: ToBriDocM BriDocNumbered
  , chainOperandDoc :: ToBriDocM BriDocNumbered
  , chainRhsBreak :: OperatorRhsBreak
  , chainBlockOperand :: Bool
  , chainLiteralOperand :: Bool
  , chainOperatorEndsLine :: Bool
  , chainOrdinaryLambdaOperand :: Bool
  }

groupedOperatorChain
  :: (OperatorChainPart -> ToBriDocM BriDocNumbered)
  -> ToBriDocM BriDocNumbered
  -> [OperatorChainPart]
  -> Maybe (ToBriDocM BriDocNumbered)
groupedOperatorChain continuation left parts = do
  priority <- minimumMaybe $ mapMaybe partPriority parts
  let (headParts, remaining) = List.span (insideGroup priority) parts
      groups = splitGroups priority remaining
  if null headParts && all ((<= 1) . length) groups
    then Nothing
    else Just $ if any membershipPart headParts
        && all ((== Just 5) . partPriority) headParts
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
      -- A compact-row alternative keeps body spacing choices from crowding
      -- out a fitting multiline head under bounded search.
      else docAlt
        [ docAddBaseY BrIndentRegular
            $ docPar (unit BrIndentNone left headParts)
            $ docLines $ map (forceRow . row) groups
        | forceRow <- [docForceSingleline, id]
        ]
 where
  membershipPart part = chainOperatorName part `elem` [Just "elem", Just "notElem"]
  partPriority = chainOperatorName >=> groupingPriority
  insideGroup priority part = not (hardBarrier part)
    && partPriority part /= Just priority
  hardBarrier part = case chainOperatorName part of
    Nothing -> True
    Just [] -> True
    Just name@(first : _) -> isNothing (groupingPriority name)
      && (isAlpha first || first == '_')
  minimumMaybe [] = Nothing
  minimumMaybe values = Just $ minimum values
  splitGroups _ [] = []
  splitGroups priority (first : rest) =
    let (inside, remaining) = List.span (insideGroup priority) rest
    in (first : inside) : splitGroups priority remaining
  unit _ first [] = first
  unit indent first rest = docAlt $
    [ docForceSingleline $ docSeq $ List.intersperse docSeparator
        $ first : List.concatMap inlinePart rest
    ]
    -- Each nested unit has a strictly higher break priority, so recursion is
    -- bounded by the fixed priority tiers rather than the chain's length.
    ++ case if any (isNothing . partPriority) rest
        then Nothing
        else groupedOperatorChain continuation first rest of
      Just grouped -> [grouped]
        -- Nested predicates and blocks may need the original continuation base.
        ++ [ docPar first $ docLines $ map continuation rest
           | any membershipPart rest || any chainBlockOperand rest
           ]
      Nothing ->
        let multiline = docPar first $ docLines $ map continuation rest
        -- An opaque span retains the original continuation base if it wraps.
        in [ if any (isNothing . partPriority) rest
               then multiline
               else docAddBaseY indent multiline
             ]
          -- A predicate may need the outer row's full width.
          -- Keep this fallback local so other fitting groups stay cohesive.
          ++ [ multiline
             | indent /= BrIndentNone
             , any membershipPart rest
             ]

  inlinePart part = [chainOperatorDoc part, chainOperandDoc part]
  row [] = docEmpty
  row (first : rest) =
    let operand = unit BrIndentRegular (chainOperandDoc first) rest
        grouped = first
          { chainOperandDoc = operand
          , chainRhsBreak = if null rest then chainRhsBreak first else NoRhsBreak
          -- Capture the structured group's base before its outer operator,
          -- rather than adding that operator's hanging column to every row.
          , chainBlockOperand = not (null rest) || chainBlockOperand first
          , chainLiteralOperand = null rest && chainLiteralOperand first
          , chainOrdinaryLambdaOperand = null rest && chainOrdinaryLambdaOperand first
          }
    in if null rest then continuation grouped
      else docSetBaseY $ continuation grouped

-- Known operators delimit visual units without changing tokens or parentheses.
-- Unknown named and qualified operators delimit groups without a numeric tier.
-- Unknown unqualified symbols stay opaque inside a unit: try it whole, then use
-- the flat fallback instead of assigning an inferred priority.
groupingPriority :: String -> Maybe Int
groupingPriority operator
  | operator `elem` ["$", "$!"] = Just 0
  | operator == "||" = Just 1
  | operator == "&&" = Just 2
  | operator == "<|>" = Just 3
  | operator `elem` ["<$>", "<$", "$>", "<*>", "<*", "*>"] = Just 4
  | operator `elem`
      [ "==", "/=", "<", "<=", ">", ">=", "elem", "notElem"
      , ".:", ".:?", ".!=", ".="
      ] = Just 5
  | operator `elem` ["+", "-", "++", ":"] = Just 6
  | operator `elem` ["*", "/", "div", "mod", "quot", "rem"] = Just 7
  | operator `elem` ["^", "^^", "**"] = Just 8
  | otherwise = Nothing
