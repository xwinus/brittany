{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module ComposableDeclarations where

-- | Configuration phase.
data Phase
  = Partial
  | Complete

-- | Selects a field representation for a phase.
type family (phase :: Phase) ::: value where
  'Partial ::: value = Maybe value
  'Complete ::: value = value

-- | Complete integer configuration.
type CompleteInt = 'Complete ::: Int

-- | Phase-indexed settings.
data Settings (phase :: Phase) = Settings
  { enabled :: phase ::: Bool
    -- ^ whether the feature is enabled
  , retries :: phase ::: Int
    -- ^ retry limit
  }

-- | Values with a textual representation.
class Codec value where
  {-# MINIMAL encode #-}
  -- | Encodes a value.
  encode
    :: value
    -- ^ input value
    -> String
    -- ^ encoded value
