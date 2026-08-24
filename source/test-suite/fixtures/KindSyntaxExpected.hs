{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module KindSyntaxExpected where
import           Data.Kind                                ( Type )
type Identity :: forall kind. kind -> kind
type Identity value = value
data Proxy (value :: kind) = Proxy
type Apply (constructor :: kind -> Type) (value :: kind) = constructor value
