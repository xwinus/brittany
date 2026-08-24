{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeOperators #-}
module TypeDataEdge where
type data Nat
  = Zero
  | -- Keep the type-level constructor comment.
    Succ Nat
type data Pair a b =
  a
    -- Keep the type operator constructor comment.
    :*: b
type Nested =
  'Succ
    -- Keep the promoted constructor application comment.
    'Zero
type Infix =
  Int
    -- Keep the type operator use comment.
    ':*: Bool
