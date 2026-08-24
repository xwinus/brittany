{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeOperators #-}
module TypeDataExpected where
type data Flag = Enabled
type data Nat = Zero | Succ Nat
type data Pair a b = a :*: b
type One = 'Succ 'Zero
type IntAndBool = Int ':*: Bool
