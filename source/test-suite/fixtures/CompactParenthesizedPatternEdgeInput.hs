{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module CompactParenthesizedPatternEdge where

longPattern context (ConfigurationParseErrorWithDetailedContext longScopeName longParseErrorName longAdditionalContextName) =
  combineErrorDetails longScopeName longParseErrorName longAdditionalContextName

nestedPatterns context (OuterPattern (NestedPattern firstValue secondValue) (tupleFirst, tupleSecond) [listFirst, listSecond] !strictValue (typedValue :: Int) (extract -> viewedValue)) =
  combineNestedValues firstValue secondValue strictValue typedValue viewedValue

commented command = case command of
  (OuterPattern
    firstValue
    -- Keep this with the following argument.
    (NestedPattern nestedFirst nestedSecond)
    finalValue) ->
      combineCommentedValues firstValue nestedFirst nestedSecond finalValue

adorned command = case command of
  (Decorated
    !strictValueWithLongName
    ~lazyValueWithLongName
    alias@(Nested nestedValue)
    (typedValue :: Int)
    (extract -> viewedValue)) ->
      adornedResult
