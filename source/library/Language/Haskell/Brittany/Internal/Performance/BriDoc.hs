{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.Performance.BriDoc
  ( profileNumberedBriDoc
  , profileBriDoc
  , profileValue
  , profileValueWithCounter
  ) where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Kind as Kind
import qualified Data.IntSet as IntSet
import Data.Generics.Uniplate.Direct (children)
import Language.Haskell.Brittany.Internal.Delimiter.Types
import Language.Haskell.Brittany.Internal.Performance
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types
import System.IO.Unsafe (unsafePerformIO)

type BriDocStats :: Kind.Type
data BriDocStats = BriDocStats
  { statsNodes :: !Int
  , statsAlternatives :: !Int
  , statsAlternativeDepth :: !Int
  , statsDelimiterGroups :: !Int
  , statsGeneratedVariants :: !Int
  }

profileNumberedBriDoc
  :: Maybe PerformanceCollector
  -> PerformanceCounter
  -> BriDocNumbered
  -> BriDocNumbered
profileNumberedBriDoc Nothing _ document = document
profileNumberedBriDoc (Just collector) nodeCounter document =
  if performanceCollectorProfilesBriDocStructure collector
    then unsafePerformIO $ do
      let stats = numberedStats document
      _ <- evaluate $ forceStats stats
      recordStats collector nodeCounter True stats
      pure document
    else document
{-# NOINLINE profileNumberedBriDoc #-}

profileBriDoc
  :: Maybe PerformanceCollector
  -> PerformancePhase
  -> PerformanceCounter
  -> BriDoc
  -> BriDoc
profileBriDoc Nothing _ _ document = document
profileBriDoc (Just collector) phase nodeCounter document = unsafePerformIO
  $ measurePhase (Just collector) phase $ do
      let stats = briDocStats document
      _ <- evaluate $ forceStats stats
      recordStats collector nodeCounter False stats
      pure document
{-# NOINLINE profileBriDoc #-}

profileValue
  :: Maybe PerformanceCollector
  -> PerformancePhase
  -> (value -> Int)
  -> value
  -> value
profileValue Nothing _ _ value = value
profileValue (Just collector) phase forceValue value = unsafePerformIO
  $ measurePhase (Just collector) phase $ do
      _ <- evaluate $ forceValue value
      pure value
{-# NOINLINE profileValue #-}

profileValueWithCounter
  :: Maybe PerformanceCollector
  -> PerformancePhase
  -> PerformanceCounter
  -> (value -> Int)
  -> value
  -> value
profileValueWithCounter Nothing _ _ _ value = value
profileValueWithCounter (Just collector) phase counter forceValue value =
  unsafePerformIO $ measurePhase (Just collector) phase $ do
    count <- evaluate $ forceValue value
    recordPerformanceCounter collector counter count
    pure value
{-# NOINLINE profileValueWithCounter #-}

recordStats
  :: PerformanceCollector
  -> PerformanceCounter
  -> Bool
  -> BriDocStats
  -> IO ()
recordStats collector nodeCounter includeStructure stats = do
  recordPerformanceCounter collector nodeCounter $ statsNodes stats
  when includeStructure $ do
    recordPerformanceCounter collector BriDocAlternatives
      $ statsAlternatives stats
    recordPerformanceCounter collector BriDocAlternativeDepth
      $ statsAlternativeDepth stats
    recordPerformanceCounter collector BriDocDelimiterGroups
      $ statsDelimiterGroups stats
    recordPerformanceCounter collector BriDocGeneratedVariants
      $ statsGeneratedVariants stats

forceStats :: BriDocStats -> Int
forceStats stats = statsNodes stats
  + statsAlternatives stats
  + statsAlternativeDepth stats
  + statsDelimiterGroups stats
  + statsGeneratedVariants stats

numberedStats :: BriDocNumbered -> BriDocStats
numberedStats document = State.evalState (visit 0 document) IntSet.empty
 where
  visit depth (nodeId, node) = do
    visited <- State.get
    if IntSet.member nodeId visited
      then pure mempty
      else do
        State.put $ IntSet.insert nodeId visited
        childStats <- visitChildren mempty $ numberedChildren node
        pure $ nodeStats depth node <> childStats
   where
    nextDepth = case node of
      BDFAlt{} -> depth + 1
      _ -> depth
    visitChildren stats [] = pure stats
    visitChildren stats (child : remaining) = do
      childStats <- visit nextDepth child
      visitChildren (stats <> childStats) remaining

briDocStats :: BriDoc -> BriDocStats
briDocStats = visit 0
 where
  visit !depth node = foldl'
    (\stats child -> stats <> visit nextDepth child)
    (plainNodeStats depth node)
    (children node)
   where
    nextDepth = case node of
      BDAlt{} -> depth + 1
      _ -> depth

nodeStats :: Int -> BriDocF f -> BriDocStats
nodeStats depth = \case
  BDFAlt alternatives -> BriDocStats 1 (length alternatives) (depth + 1) 0 0
  BDFDelimited group -> BriDocStats
    1 0 depth 1 (length $ delimitedAllowedLayouts group)
  _ -> BriDocStats 1 0 depth 0 0

plainNodeStats :: Int -> BriDoc -> BriDocStats
plainNodeStats depth = \case
  BDAlt alternatives -> BriDocStats 1 (length alternatives) (depth + 1) 0 0
  BDDelimited group -> BriDocStats
    1 0 depth 1 (length $ delimitedAllowedLayouts group)
  _ -> BriDocStats 1 0 depth 0 0

numberedChildren :: BriDocFInt -> [BriDocNumbered]
numberedChildren = \case
  BDFSeq documents -> documents
  BDFCols _ documents -> documents
  BDFAddBaseY _ document -> [document]
  BDFBaseYPushCur document -> [document]
  BDFBaseYPop document -> [document]
  BDFIndentLevelPushCur document -> [document]
  BDFIndentLevelPop document -> [document]
  BDFPar _ line indented -> [line, indented]
  BDFDelimited group -> activeDelimitedDocuments group
  BDFAlt documents -> documents
  BDFForwardLineMode document -> [document]
  BDFAnnotationPrior _ _ document -> [document]
  BDFAnnotationKW _ _ document -> [document]
  BDFAnnotationRest _ document -> [document]
  BDFMoveToKWDP _ _ _ document -> [document]
  BDFLines documents -> documents
  BDFEnsureIndent _ document -> [document]
  BDFForceMultiline document -> [document]
  BDFForceSingleline document -> [document]
  BDFColumnsLimit _ document -> [document]
  BDFNonBottomSpacing _ document -> [document]
  BDFSetParSpacing document -> [document]
  BDFForceParSpacing document -> [document]
  BDFDebug _ document -> [document]
  _ -> []

instance Semigroup BriDocStats where
  left <> right = BriDocStats
    { statsNodes = statsNodes left + statsNodes right
    , statsAlternatives = statsAlternatives left + statsAlternatives right
    , statsAlternativeDepth = max
        (statsAlternativeDepth left) (statsAlternativeDepth right)
    , statsDelimiterGroups = statsDelimiterGroups left
        + statsDelimiterGroups right
    , statsGeneratedVariants = statsGeneratedVariants left
        + statsGeneratedVariants right
    }

instance Monoid BriDocStats where
  mempty = BriDocStats 0 0 0 0 0
