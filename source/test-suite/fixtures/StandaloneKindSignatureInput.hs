{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module StandaloneKindSignatureExpected where

import qualified Data.Kind as Kind

type Mapper::forall k.(k -> Kind.Type)->[k]->Kind.Type
type (:+:)::Kind.Type -> Kind.Type -> Kind.Type
type Visible::forall k -> k -> Kind.Type
