{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.PatternComments
  ( commentSensitivePatterns
  ) where

import Data.Data (Data)
import qualified Data.Generics as SYB
import GHC (GenLocated(L), Located, unLoc)
import GHC.Hs (HsConDetails(..), LPat, Pat(..))
import GHC.Parser.Annotation (getLocA)
import Language.Haskell.Brittany.Internal.Prelude

-- | Find record patterns whose comments cannot be laid out natively without
-- losing their attachment to a field.
commentSensitivePatterns :: Data ast => ast -> [Located (Pat GhcPs)]
commentSensitivePatterns = SYB.everything (++) patternQuery
 where
  patternQuery :: SYB.GenericQ [Located (Pat GhcPs)]
  patternQuery = const [] `SYB.extQ` collectPattern

  collectPattern :: LPat GhcPs -> [Located (Pat GhcPs)]
  collectPattern pattern'
    | isCommentSensitive $ unLoc pattern' =
        [L (getLocA pattern') (unLoc pattern')]
    | otherwise = []

isCommentSensitive :: Pat GhcPs -> Bool
isCommentSensitive = \case
  ConPat _ _ (RecCon _) -> True
  _ -> False
