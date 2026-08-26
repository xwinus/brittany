{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
module StandaloneDerivingCommented where

deriving via
    -- Keep the representation comment.
    (Maybe a)
    instance
        -- Keep the derived head comment.
        Eq a => Eq (Wrapper a)
