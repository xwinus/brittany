{-# LANGUAGE DerivingVia #-}
data Wrapped value = Wrapped
  { unWrapped :: value -- field comment
  -- , removed :: Bool
  }
  deriving -- deriving comment
           Show -- class comment
           via (Identity value) -- representation comment
