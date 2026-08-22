{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
type family Result input
  = output
  -- injectivity comment
  | output -> input
  where
    -- closed equation comment
    Result Int = Bool
data family Wrapped value
data instance Wrapped Int where
  -- GADT constructor comment
  WrappedInt :: Int -> Wrapped Int
