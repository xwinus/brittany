{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Compatibility types for ghc-exactprint 1.14 / GHC 9.14.
-- In GHC 9.14 the old API annotations (ApiAnns, AnnKeywordId, Anns, AnnKey)
-- were removed. This module provides stub types so that the rest of Brittany
-- compiles. Annotation lookups will return empty; comment preservation is
-- not supported on this port.
module Language.Haskell.Brittany.Internal.ExactPrintCompat where

import Control.Monad.Trans.State (State, StateT, runState, runStateT, state)
import Data.Data (Data, toConstr, Typeable)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import GHC (GenLocated(L))
import qualified GHC.Data.FastString as FastString
import qualified GHC.Data.Strict as Strict
import GHC.Types.SrcLoc (RealSrcSpan, SrcSpan(..))
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.Prelude

-- Stub for GHC 9.14: AnnKeywordId was removed from GHC.Parser.Annotation.
-- Constructors used by layouters (partial list).
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

type AnnSpan = [SrcSpan]

data AnnConName = CN { unConName :: String }
  deriving (Data, Eq, Ord, Show)

data AnnKey = AnnKey AnnSpan AnnConName
  deriving (Data, Eq, Show)

annKeyCon :: AnnKey -> AnnConName
annKeyCon (AnnKey _ c) = c

instance Ord AnnKey where
  compare (AnnKey leftSpans leftConstructor) (AnnKey rightSpans rightConstructor) =
    compareSpanLists leftSpans rightSpans
      <> compare leftConstructor rightConstructor

data DeltaPos = DP !(Int, Int)
  deriving (Eq, Ord, Show)

deltaRow :: DeltaPos -> Int
deltaRow (DP (r, _)) = r

data Comment = Comment
  { commentOrigin :: Maybe AnnKeywordId
  , commentIdentifier :: SrcSpan
  , commentContents :: String
  }
  deriving (Eq, Show)

instance Ord Comment where
  compare left right =
    compare (commentOrigin left) (commentOrigin right)
      <> compareSrcSpan
        (commentIdentifier left)
        (commentIdentifier right)
      <> compare (commentContents left) (commentContents right)

data KeywordId
  = AnnString [String]
  | AnnComment Comment
  | AnnTypeApp
  | AnnSemiSep
  | G AnnKeywordId
  | AnnEofPos
  deriving (Eq, Ord, Show)

data Annotation = Ann
  { annCapturedSpan :: Maybe AnnKey
  , annSortKey :: Maybe [SrcSpan]
  , annsDP :: [(KeywordId, DeltaPos)]
  , annFollowingComments :: [(Comment, DeltaPos)]
  , annPriorComments :: [(Comment, DeltaPos)]
  , annEntryDelta :: DeltaPos
  }
  deriving (Eq, Show)

type Anns = Map.Map AnnKey Annotation

emptyAnns :: Anns
emptyAnns = Map.empty

mkAnnKey :: Data a => GenLocated SrcSpan a -> AnnKey
mkAnnKey (L span a) = AnnKey [stripBufSpan span] (CN (show (toConstr a)))

mkNamedAnnKey :: String -> SrcSpan -> AnnKey
mkNamedAnnKey name span = AnnKey [stripBufSpan span] (CN name)

-- | Strip BufSpan from SrcSpan to normalize keys.
-- GHC 9.14 getLocA preserves BufSpan but epaLocationRealSrcSpan/realSpanToSrcSpan
-- strips it, causing key mismatches. Normalize to always strip.
stripBufSpan :: SrcSpan -> SrcSpan
stripBufSpan (RealSrcSpan r _) = RealSrcSpan r Strict.Nothing
stripBufSpan s = s

-- | Structural ordering for source spans. Unlike rendering-based ordering,
-- this remains lawful if the 'Show' representation changes.
compareSrcSpan :: SrcSpan -> SrcSpan -> Ordering
compareSrcSpan (RealSrcSpan left leftBuffer) (RealSrcSpan right rightBuffer) =
  compareRealSrcSpan left right <> compareStrictMaybe leftBuffer rightBuffer
compareSrcSpan RealSrcSpan{} UnhelpfulSpan{} = LT
compareSrcSpan UnhelpfulSpan{} RealSrcSpan{} = GT
compareSrcSpan (UnhelpfulSpan left) (UnhelpfulSpan right) =
  compareUnhelpfulSpanReason left right

compareSpanLists :: [SrcSpan] -> [SrcSpan] -> Ordering
compareSpanLists [] [] = EQ
compareSpanLists [] (_ : _) = LT
compareSpanLists (_ : _) [] = GT
compareSpanLists (left : leftRest) (right : rightRest) =
  compareSrcSpan left right <> compareSpanLists leftRest rightRest

compareRealSrcSpan :: RealSrcSpan -> RealSrcSpan -> Ordering
compareRealSrcSpan left right =
  FastString.lexicalCompareFS
    (SrcLoc.srcSpanFile left)
    (SrcLoc.srcSpanFile right)
    <> compare (SrcLoc.srcSpanStartLine left) (SrcLoc.srcSpanStartLine right)
    <> compare (SrcLoc.srcSpanStartCol left) (SrcLoc.srcSpanStartCol right)
    <> compare (SrcLoc.srcSpanEndLine left) (SrcLoc.srcSpanEndLine right)
    <> compare (SrcLoc.srcSpanEndCol left) (SrcLoc.srcSpanEndCol right)

compareStrictMaybe :: Ord a => Strict.Maybe a -> Strict.Maybe a -> Ordering
compareStrictMaybe Strict.Nothing Strict.Nothing = EQ
compareStrictMaybe Strict.Nothing Strict.Just{} = LT
compareStrictMaybe Strict.Just{} Strict.Nothing = GT
compareStrictMaybe (Strict.Just left) (Strict.Just right) = compare left right

compareUnhelpfulSpanReason
  :: SrcLoc.UnhelpfulSpanReason -> SrcLoc.UnhelpfulSpanReason -> Ordering
compareUnhelpfulSpanReason left right = case (left, right) of
  (SrcLoc.UnhelpfulOther leftText, SrcLoc.UnhelpfulOther rightText) ->
    FastString.lexicalCompareFS leftText rightText
  _ -> compare (unhelpfulSpanReasonRank left) (unhelpfulSpanReasonRank right)

unhelpfulSpanReasonRank :: SrcLoc.UnhelpfulSpanReason -> Int
unhelpfulSpanReasonRank SrcLoc.UnhelpfulNoLocationInfo = 0
unhelpfulSpanReasonRank SrcLoc.UnhelpfulWiredIn = 1
unhelpfulSpanReasonRank SrcLoc.UnhelpfulInteractive = 2
unhelpfulSpanReasonRank SrcLoc.UnhelpfulGenerated = 3
unhelpfulSpanReasonRank SrcLoc.UnhelpfulOther{} = 4

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
type Transform a = State Anns a
type TransformT m a = StateT Anns m a

modifyAnnsT :: (Anns -> Anns) -> Transform ()
modifyAnnsT f = state $ \anns -> ((), f anns)

runTransform :: Anns -> Transform a -> (Anns, a)
runTransform anns m = let (a, anns') = runState m anns in (anns', a)
