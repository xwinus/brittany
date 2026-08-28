{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module StandaloneKindSignatureEdge where

import qualified Data.Kind as Kind

-- | Keep the declaration documentation.
type Commented
  :: forall k
     -- Keep the standalone kind binder comment.
   . (k -> Kind.Type)
  -> '[k]
  -> Kind.Type
type Visible :: forall k -> k -> Kind.Type
type Qualified :: Kind.Type
