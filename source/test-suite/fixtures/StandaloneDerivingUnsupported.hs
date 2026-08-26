{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
module StandaloneDerivingUnsupported where

deriving via '(Int, Bool) instance Show (Wrapper a)
