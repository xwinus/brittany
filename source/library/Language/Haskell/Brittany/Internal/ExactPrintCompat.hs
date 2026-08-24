{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneKindSignatures #-}
-- | Compatibility types for ghc-exactprint 1.14 / GHC 9.14.
-- In GHC 9.14 the old API annotations (ApiAnns, AnnKeywordId, Anns, AnnKey)
-- were removed. This module provides stub types so that the rest of Brittany
-- compiles. Annotation lookups will return empty; comment preservation is
-- not supported on this port.
module Language.Haskell.Brittany.Internal.ExactPrintCompat where

import Control.Monad.Trans.State (State, StateT, runState, state)
import Data.Data (Data)
import Data.Function (on)
import Data.Kind (Type)
import qualified Data.Map as Map
import GHC (GenLocated(L))
import qualified GHC.Data.Strict as Strict
import GHC.Types.SrcLoc (RealSrcSpan, SrcSpan(..))
import Language.Haskell.Brittany.Internal.Prelude

-- Stub for GHC 9.14: AnnKeywordId was removed from GHC.Parser.Annotation.
-- Constructors used by layouters (partial list).
type AnnKeywordId :: Type
data AnnKeywordId
  = AnnModule
  | AnnWhere
  | AnnOpenP
  | AnnCloseP
  | AnnIf
  | AnnThen
  | AnnLet
  | AnnOpenS
  | AnnOpenC
  | AnnCloseC
  | AnnDotdot
  | AnnSimpleQuote
  | AnnBackquote
  | AnnCommaTuple
  | AnnEqual
  | AnnIn
  | AnnComma
  | AnnSemi
  | AnnVbar
  | AnnDarrow
  | AnnForall
  | AnnDot
  | AnnVal
  | AnnTilde
  | AnnAt
  | AnnLarrow
  | AnnRarrow
  | AnnType
  | AnnCase
  | AnnOf
  | AnnDo
  | AnnMdo
  | AnnStock
  | AnnAnyclass
  | AnnVia
  | AnnUnit
  | AnnOpenB
  | AnnCloseB
  | AnnData
  | AnnElse
  deriving (Data, Eq, Ord, Show)

type AnnSpan :: Type
type AnnSpan = [SrcSpan]

type AnnConName :: Type
data AnnConName = CN { unConName :: String }
  deriving (Data, Eq, Ord, Show)

type AnnKey :: Type
data AnnKey = AnnKey AnnSpan AnnConName
  deriving (Data, Eq, Show)

annKeyCon :: AnnKey -> AnnConName
annKeyCon (AnnKey _ c) = c

instance Ord AnnKey where
  compare = compare `on` show

type DeltaPos :: Type
data DeltaPos = DP !(Int, Int)
  deriving (Eq, Ord, Show)

deltaRow :: DeltaPos -> Int
deltaRow (DP (r, _)) = r

type Comment :: Type
data Comment = Comment
  { commentOrigin :: Maybe AnnKeywordId
  , commentIdentifier :: SrcSpan
  , commentContents :: String
  }
  deriving (Eq, Show)

instance Ord Comment where
  compare = compare `on` show

type KeywordId :: Type
data KeywordId
  = AnnString [String]
  | AnnComment Comment
  | AnnTypeApp
  | AnnSemiSep
  | G AnnKeywordId
  | AnnEofPos
  deriving (Eq, Ord, Show)

type Annotation :: Type
data Annotation = Ann
  { annCapturedSpan :: Maybe AnnKey
  , annSortKey :: Maybe [SrcSpan]
  , annsDP :: [(KeywordId, DeltaPos)]
  , annFollowingComments :: [(Comment, DeltaPos)]
  , annPriorComments :: [(Comment, DeltaPos)]
  , annEntryDelta :: DeltaPos
  }
  deriving (Eq, Show)

instance Ord Annotation where
  compare = compare `on` show

type Anns :: Type
type Anns = Map.Map AnnKey Annotation

emptyAnns :: Anns
emptyAnns = Map.empty

mkAnnKey :: Data a => GenLocated SrcSpan a -> AnnKey
mkAnnKey (L span a) = AnnKey [stripBufSpan span] (CN (show (toConstr a)))

-- | Strip BufSpan from SrcSpan to normalize keys.
-- GHC 9.14 getLocA preserves BufSpan but epaLocationRealSrcSpan/realSpanToSrcSpan
-- strips it, causing key mismatches. Normalize to always strip.
stripBufSpan :: SrcSpan -> SrcSpan
stripBufSpan (RealSrcSpan r _) = RealSrcSpan r Strict.Nothing
stripBufSpan s = s

-- | Convert SrcSpan to RealSrcSpan when possible (for use with realSrcSpanStart/End).
srcSpanToRealSpan :: SrcSpan -> Maybe RealSrcSpan
srcSpanToRealSpan (RealSrcSpan r _) = Just r
srcSpanToRealSpan _ = Nothing

-- | Convert RealSrcSpan to SrcSpan for use in Comment etc. (GHC 9.14).
realSpanToSrcSpan :: RealSrcSpan -> SrcSpan
realSpanToSrcSpan r = RealSrcSpan r Strict.Nothing

-- | Extract RealSrcSpan from AnnKey for use with realSrcSpanStart/End (first span if RealSrcSpan).
annKeyRealSpan :: AnnKey -> Maybe RealSrcSpan
annKeyRealSpan (AnnKey spans _) = listToMaybe spans >>= srcSpanToRealSpan

-- Stub for old ExactPrint.Transform (state over Anns). No-op on GHC 9.14 port.
type Transform :: Type -> Type
type Transform a = State Anns a
type TransformT :: (Type -> Type) -> Type -> Type
type TransformT m a = StateT Anns m a

modifyAnnsT :: (Anns -> Anns) -> Transform ()
modifyAnnsT f = state $ \anns -> ((), f anns)

runTransform :: Anns -> Transform a -> (Anns, a)
runTransform anns m = let (a, anns') = runState m anns in (anns', a)
