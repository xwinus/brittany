{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
module StandaloneDeriving where

deriving via
    (Generically (PtPostProcessConfig c))
    instance
        (Semigroup (c 'Partial)) => Semigroup (PtPostProcessConfig c)
