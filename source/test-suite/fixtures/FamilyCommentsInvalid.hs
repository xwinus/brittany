{-# LANGUAGE TypeFamilies #-}
type family Broken input where
  Broken Int =
  -- missing right-hand side
