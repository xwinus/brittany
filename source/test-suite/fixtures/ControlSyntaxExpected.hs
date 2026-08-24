{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ListTuplePuns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
module ControlSyntaxExpected where

type Pair = '(Int, Bool)
type Values = '[Int, Bool]

describe = \case
  Nothing -> "empty"
  Just value -> show value

combine = \cases
  Nothing _ -> Nothing
  _ Nothing -> Nothing
  Just left Just right -> Just (left, right)

sign value = if
  | value < 0 -> "negative"
  | value == 0 -> "zero"
  | otherwise -> "positive"

leftSection value = (value,)
rightSection value = (,value)
