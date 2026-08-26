{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.TypeFallbacks
  ( exactSourceTypes
  , requiresExactTypeDeclaration
  ) where

import Data.Data (Data)
import qualified Data.Generics as SYB
import GHC (GenLocated(L), Located, unLoc)
import GHC.Hs
  ( HsDataDefn(dd_cons)
  , HsType(..)
  , LHsType
  , TyClDecl(..)
  , isTypeDataDefnCons
  )
import GHC.Parser.Annotation (getLocA)
import Language.Haskell.Brittany.Internal.Prelude

-- | Find type shapes whose inline exact printer is not layout-stable.
exactSourceTypes :: Data ast => ast -> [Located (HsType GhcPs)]
exactSourceTypes = SYB.everything (++) typeQuery
 where
  typeQuery :: SYB.GenericQ [Located (HsType GhcPs)]
  typeQuery = const [] `SYB.extQ` collectType

  collectType :: LHsType GhcPs -> [Located (HsType GhcPs)]
  collectType type'
    | requiresExactSource $ unLoc type' =
        [L (getLocA type') (unLoc type')]
    | otherwise = []

requiresExactSource :: HsType GhcPs -> Bool
requiresExactSource = \case
  HsExplicitTupleTy{} -> True
  HsSpliceTy{} -> True
  HsSumTy{} -> True
  _ -> False

-- | Type-level data declarations need the exact @type data@ keyword sequence.
requiresExactTypeDeclaration :: TyClDecl GhcPs -> Bool
requiresExactTypeDeclaration = \case
  DataDecl _ _ _ _ definition ->
    isTypeDataDefnCons $ dd_cons definition
  _ -> False
