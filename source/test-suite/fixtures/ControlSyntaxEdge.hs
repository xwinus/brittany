{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ListTuplePuns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
module ControlSyntaxEdge where

type Pair =
  '( Int
   -- Keep the tuple pun separator comment.
   , Bool
   )
type Values =
  '[ Int
   -- Keep the list pun separator comment.
   , Bool
   ]

describe = \case
  -- Keep the lambda-case alternative comment.
  Nothing -> "empty"
  Just value -> show value

combine = \cases
  -- Keep the lambda-cases alternative comment.
  Nothing _ -> Nothing
  Just left Just right -> Just (left, right)

sign value = if
  | -- Keep the multi-way-if guard comment.
    value < 0 -> "negative"
  | otherwise -> "non-negative"

leftSection value =
  ( value
  , -- Keep the tuple section hole comment.
  )
