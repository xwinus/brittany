{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.PatternComments
  ( commentSensitivePatterns
  , exactSourcePatterns
  , supportedBangPatterns
  , supportedStrictBindingNames
  ) where

import Data.Data (Data)
import qualified Data.Generics as SYB
import GHC (GenLocated(L), Located, unLoc)
import GHC.Hs
  ( HsConDetails(..)
  , HsMatchContext(..)
  , HsUntypedSplice(..)
  , LHsExpr
  , LMatch
  , LPat
  , Match(..)
  , Pat(..)
  , SrcStrictness(SrcStrict)
  )
import GHC.Parser.Annotation (getLocA)
import Language.Haskell.Brittany.Internal.Prelude

-- | Find patterns whose comments cannot be laid out natively without losing
-- their attachment to syntax punctuation.
commentSensitivePatterns :: Data ast => ast -> [Located (Pat GhcPs)]
commentSensitivePatterns = collectPatterns isCommentSensitive

-- | Find pattern shapes that require exact-source rendering even without
-- comments.
exactSourcePatterns :: Data ast => ast -> [Located (Pat GhcPs)]
exactSourcePatterns = collectPatterns requiresExactSource

-- | Find bang patterns that the native pattern layouter can render safely.
supportedBangPatterns :: Data ast => ast -> [Located (Pat GhcPs)]
supportedBangPatterns = collectPatterns $ \case
  BangPat{} -> True
  _ -> False

-- | Find strict function bindings, which GHC represents separately from
-- 'BangPat' pattern bindings.
supportedStrictBindingNames :: Data ast => ast -> [Located RdrName]
supportedStrictBindingNames = SYB.everything (++) strictBindingQuery
 where
  strictBindingQuery :: SYB.GenericQ [Located RdrName]
  strictBindingQuery = const [] `SYB.extQ` collectStrictBinding

  collectStrictBinding
    :: LMatch GhcPs (LHsExpr GhcPs) -> [Located RdrName]
  collectStrictBinding (L _ (Match _ context _ _)) = case context of
    FunRhs bindingName _ SrcStrict _ ->
      [L (getLocA bindingName) (unLoc bindingName)]
    _ -> []

collectPatterns
  :: Data ast
  => (Pat GhcPs -> Bool)
  -> ast
  -> [Located (Pat GhcPs)]
collectPatterns predicate = SYB.everything (++) patternQuery
 where
  patternQuery :: SYB.GenericQ [Located (Pat GhcPs)]
  patternQuery = const [] `SYB.extQ` collectPattern

  collectPattern :: LPat GhcPs -> [Located (Pat GhcPs)]
  collectPattern pattern'
    | predicate $ unLoc pattern' =
        [L (getLocA pattern') (unLoc pattern')]
    | otherwise = []

isCommentSensitive :: Pat GhcPs -> Bool
isCommentSensitive = \case
  BangPat{} -> True
  ConPat _ _ (RecCon _) -> True
  EmbTyPat{} -> True
  InvisPat{} -> True
  LazyPat{} -> True
  OrPat{} -> True
  SplicePat{} -> True
  SumPat{} -> True
  TuplePat{} -> True
  ViewPat{} -> True
  _ -> False

requiresExactSource :: Pat GhcPs -> Bool
requiresExactSource = \case
  InvisPat{} -> True
  LazyPat{} -> True
  OrPat{} -> True
  SplicePat _ HsQuasiQuote{} -> False
  SplicePat{} -> True
  SumPat{} -> True
  _ -> False
