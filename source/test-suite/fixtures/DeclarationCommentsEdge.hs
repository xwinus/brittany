{-# LANGUAGE DerivingVia #-}
data Wrapped value = Wrapped
  { unWrapped :: value -- field comment
  -- , removed :: Bool
  }
  deriving
  -- deriving comment
  -- class comment
  Show
   via (Identity value) -- representation comment
