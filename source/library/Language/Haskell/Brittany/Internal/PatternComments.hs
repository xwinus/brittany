{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.PatternComments
  ( commentSensitivePatterns
  , exactSourcePatterns
  ) where

import Data.Data (Data)
import qualified Data.Generics as SYB
import GHC (GenLocated(L), Located, unLoc)
import GHC.Hs (HsConDetails(..), HsUntypedSplice(..), LPat, Pat(..))
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
  BangPat{} -> True
  InvisPat{} -> True
  LazyPat{} -> True
  OrPat{} -> True
  SplicePat _ HsQuasiQuote{} -> False
  SplicePat{} -> True
  SumPat{} -> True
  _ -> False
