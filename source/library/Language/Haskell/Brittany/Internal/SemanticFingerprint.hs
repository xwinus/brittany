{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.SemanticFingerprint
  ( SemanticDifference(..)
  , SemanticFingerprint
  , SemanticProjectionError(..)
  , compareSemanticSyntax
  , renderSemanticPath
  , semanticFingerprint
  ) where

import qualified Data.Data as Data
import Data.Kind (Type)
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SemanticFingerprint.Ghc
  ( projectSemanticSyntax
  )
import Language.Haskell.Brittany.Internal.SemanticModel

type SemanticFingerprint :: Type
type SemanticFingerprint = SemanticModel

semanticFingerprint
  :: Data.Data ast
  => ast
  -> Either SemanticProjectionError SemanticFingerprint
semanticFingerprint = projectSemanticSyntax

compareSemanticSyntax
  :: (Data.Data input, Data.Data output)
  => input
  -> output
  -> Either SemanticProjectionError (Maybe SemanticDifference)
compareSemanticSyntax input output = do
  inputFingerprint <- semanticFingerprint input
  outputFingerprint <- semanticFingerprint output
  pure $ firstDifference [] inputFingerprint outputFingerprint
