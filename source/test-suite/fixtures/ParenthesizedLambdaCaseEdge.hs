{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module ParenthesizedLambdaCaseEdge where

nested = consume
  (
    (
      \case
        Just value -> value
        Nothing    -> 0
    )
  )

guarded = apply
  (
    \case
      Just value | value > 0 -> value
                 | otherwise -> 0
      Nothing -> 0
  )

commented = apply
  (
    \case
      Just value -> value
    -- Keep this comment between alternatives.
      Nothing    -> 0
  )

argument = apply
  (
    \case
      Left  message -> report message
      Right value   -> accept value
  )

longResult = apply
  (
    \case
      Just value -> transformValueUsingAnIntentionallyLongFunctionName
        value
        anotherIntentionallyLongArgumentName
      Nothing -> fallbackValue
  )

compact = apply (\case {})

caseControl = apply
  (
    case input of
      Just value -> accept value
      Nothing    -> reject
  )

doControl = apply
  (
    do
      value <- acquire
      accept value
  )

multiWayControl = apply
  (
    if
      | ready     -> accept input
      | otherwise -> reject
  )
