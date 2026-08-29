{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module KindSyntaxEdge where
import           Data.Kind                                ( Type )
type Identity
  :: forall kind
     -- Keep the standalone kind binder comment.
   . kind
  -> kind
type Identity value = value
data Proxy (value ::
-- Keep the declaration binder kind comment.
                     kind) = Proxy
type Apply (constructor :: kind -> Type) (value ::
-- Keep the synonym binder kind comment.
                                                   kind) = constructor value
