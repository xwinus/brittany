{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.Performance
  ( PerformancePhase(..)
  , PerformanceCounter(..)
  , CounterAggregate(..)
  , PhaseStatus(..)
  , PhaseSample(..)
  , PhaseAggregate(..)
  , RuntimeSnapshot
  , RuntimeMetrics(..)
  , PerformanceCollector
  , newPerformanceCollector
  , newPerformanceCollectorWithBriDocStructure
  , performanceCollectorProfilesBriDocStructure
  , readPerformanceSamples
  , readPerformanceCounters
  , recordPerformanceCounter
  , aggregatePerformanceCounters
  , measurePhase
  , measurePhaseM
  , aggregatePhaseSamples
  , captureRuntimeSnapshot
  , runtimeMetricsDifference
  , performancePhaseName
  , performanceCounterName
  ) where

import Control.Exception (SomeException, throwIO, try)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import qualified Data.Kind as Kind
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getAllocationCounter)
import qualified GHC.Stats as Stats
import Language.Haskell.Brittany.Internal.Prelude
import System.CPUTime (getCPUTime)

type PerformancePhase :: Kind.Type
data PerformancePhase
  = AnnKeyComparison
  | AnnKeyMapOperations
  | GhcSession
  | GhcSessionSetup
  | DynamicFlagParsing
  | SourceParsing
  | CommentRecovery
  | AnnotationExtraction
  | AnnotationIndexConstruction
  | InlineConfiguration
  | CommentPlanning
  | TopLevelGrouping
  | LayoutAndRendering
  | BriDocConstruction
  | CommentLowering
  | AlternativeResolution
  | SimplifyFloating
  | SimplifyPar
  | SimplifyColumns
  | SimplifyIndent
  | BackendRendering
  | PlannedCommentValidation
  | OutputValidation
  | OutputParsing
  | SemanticValidation
  | CommentValidation
  deriving (Bounded, Enum, Eq, Ord, Show)

type PhaseStatus :: Kind.Type
data PhaseStatus
  = PhaseSucceeded
  | PhaseFailed
  deriving (Eq, Ord, Show)

type PhaseSample :: Kind.Type
data PhaseSample = PhaseSample
  { phaseSamplePhase :: PerformancePhase
  , phaseSampleStatus :: PhaseStatus
  , phaseSampleElapsedNs :: Word64
  , phaseSampleCpuNs :: Word64
  , phaseSampleAllocatedBytes :: Maybe Word64
  }
  deriving (Eq, Show)

type PhaseAggregate :: Kind.Type
data PhaseAggregate = PhaseAggregate
  { phaseAggregatePhase :: PerformancePhase
  , phaseAggregateCalls :: Int
  , phaseAggregateFailures :: Int
  , phaseAggregateElapsedNs :: Word64
  , phaseAggregateCpuNs :: Word64
  , phaseAggregateAllocatedBytes :: Maybe Word64
  }
  deriving (Eq, Show)

type PerformanceCounter :: Kind.Type
data PerformanceCounter
  = RawBriDocNodes
  | PostCommentLoweringBriDocNodes
  | PostAlternativeBriDocNodes
  | PostFloatingBriDocNodes
  | PostParBriDocNodes
  | PostColumnsBriDocNodes
  | PostIndentBriDocNodes
  | BriDocAlternatives
  | BriDocAlternativeDepth
  | BriDocDelimiterGroups
  | BriDocGeneratedVariants
  | GetSpacingCalls
  | GetSpacingsCalls
  | GetSpacingsMemoHits
  | MaximumSpacingWidthBeforePruning
  | MaximumSpacingWidthAfterPruning
  deriving (Bounded, Enum, Eq, Ord, Show)

type CounterAggregate :: Kind.Type
data CounterAggregate = CounterAggregate
  { counterAggregateCounter :: PerformanceCounter
  , counterAggregateValue :: Int
  }
  deriving (Eq, Show)

type PerformanceCollector :: Kind.Type
data PerformanceCollector = PerformanceCollector
  { collectorPhaseSamples :: IORef [PhaseSample]
  , collectorCounterSamples :: IORef [(PerformanceCounter, Int)]
  , collectorProfilesBriDocStructure :: Bool
  }

type RuntimeSnapshot :: Kind.Type
data RuntimeSnapshot = RuntimeSnapshot
  { runtimeSnapshotElapsedNs :: Word64
  , runtimeSnapshotCpuNs :: Word64
  , runtimeSnapshotAllocatedBytes :: Maybe Word64
  , runtimeSnapshotCopiedBytes :: Maybe Word64
  , runtimeSnapshotGcCpuNs :: Maybe Word64
  , runtimeSnapshotMutatorCpuNs :: Maybe Word64
  , runtimeSnapshotGcCount :: Maybe Word64
  , runtimeSnapshotMaximumResidencyBytes :: Maybe Word64
  , runtimeSnapshotAllocationCounter :: Int64
  }

type RuntimeMetrics :: Kind.Type
data RuntimeMetrics = RuntimeMetrics
  { runtimeElapsedNs :: Word64
  , runtimeCpuNs :: Word64
  , runtimeAllocatedBytes :: Maybe Word64
  , runtimeCopiedBytes :: Maybe Word64
  , runtimeGcCpuNs :: Maybe Word64
  , runtimeMutatorCpuNs :: Maybe Word64
  , runtimeGcCount :: Maybe Word64
  , runtimeMaximumResidencyBytes :: Maybe Word64
  , runtimeProductivity :: Maybe Double
  }
  deriving (Eq, Show)

newPerformanceCollector :: IO PerformanceCollector
newPerformanceCollector = newPerformanceCollectorWithBriDocStructure True

newPerformanceCollectorWithBriDocStructure :: Bool -> IO PerformanceCollector
newPerformanceCollectorWithBriDocStructure profileBriDocStructure =
  PerformanceCollector
  <$> newIORef []
  <*> newIORef []
  <*> pure profileBriDocStructure

performanceCollectorProfilesBriDocStructure
  :: PerformanceCollector -> Bool
performanceCollectorProfilesBriDocStructure = collectorProfilesBriDocStructure

readPerformanceSamples :: PerformanceCollector -> IO [PhaseSample]
readPerformanceSamples PerformanceCollector{collectorPhaseSamples = samplesRef} =
  reverse <$> readIORef samplesRef

readPerformanceCounters
  :: PerformanceCollector -> IO [(PerformanceCounter, Int)]
readPerformanceCounters PerformanceCollector
    {collectorCounterSamples = samplesRef} = reverse <$> readIORef samplesRef

recordPerformanceCounter
  :: PerformanceCollector -> PerformanceCounter -> Int -> IO ()
recordPerformanceCounter PerformanceCollector
    {collectorCounterSamples = samplesRef} counter value =
  atomicModifyIORef' samplesRef $ \samples -> ((counter, value) : samples, ())

aggregatePerformanceCounters
  :: [(PerformanceCounter, Int)] -> [CounterAggregate]
aggregatePerformanceCounters = fmap (uncurry CounterAggregate)
  . Map.toAscList
  . List.foldl' insertCounter Map.empty
 where
  insertCounter counters (counter, value) = Map.insertWith
    (if isMaximumCounter counter then max else (+)) counter value counters
  isMaximumCounter counter = counter `elem`
    [ BriDocAlternativeDepth
    , MaximumSpacingWidthBeforePruning
    , MaximumSpacingWidthAfterPruning
    ]

measurePhase
  :: Maybe PerformanceCollector
  -> PerformancePhase
  -> IO value
  -> IO value
measurePhase Nothing _ action = action
measurePhase (Just collector) phase action = do
  before <- captureRuntimeSnapshot
  result <- try action
  after <- captureRuntimeSnapshot
  let status = case result of
        Left (_ :: SomeException) -> PhaseFailed
        Right{} -> PhaseSucceeded
  recordSample collector $ snapshotDifference phase status before after
  case result of
    Left exception -> throwIO (exception :: SomeException)
    Right value -> pure value

measurePhaseM
  :: MonadIO monad
  => Maybe PerformanceCollector
  -> PerformancePhase
  -> monad value
  -> monad value
measurePhaseM Nothing _ action = action
measurePhaseM (Just collector) phase action = do
  before <- liftIO captureRuntimeSnapshot
  value <- action
  after <- liftIO captureRuntimeSnapshot
  liftIO $ recordSample collector
    $ snapshotDifference phase PhaseSucceeded before after
  pure value

aggregatePhaseSamples :: [PhaseSample] -> [PhaseAggregate]
aggregatePhaseSamples = List.sortOn phaseAggregatePhase
  . Map.elems
  . List.foldl' insertSample Map.empty
 where
  insertSample aggregates sample = Map.insertWith combine
    (phaseSamplePhase sample)
    (sampleAggregate sample)
    aggregates

  combine newer older = PhaseAggregate
    { phaseAggregatePhase = phaseAggregatePhase older
    , phaseAggregateCalls = phaseAggregateCalls older
        + phaseAggregateCalls newer
    , phaseAggregateFailures = phaseAggregateFailures older
        + phaseAggregateFailures newer
    , phaseAggregateElapsedNs = phaseAggregateElapsedNs older
        + phaseAggregateElapsedNs newer
    , phaseAggregateCpuNs = phaseAggregateCpuNs older
        + phaseAggregateCpuNs newer
    , phaseAggregateAllocatedBytes = (+)
        <$> phaseAggregateAllocatedBytes older
        <*> phaseAggregateAllocatedBytes newer
    }

  sampleAggregate PhaseSample
    { phaseSamplePhase
    , phaseSampleStatus
    , phaseSampleElapsedNs
    , phaseSampleCpuNs
    , phaseSampleAllocatedBytes
    } = PhaseAggregate
      { phaseAggregatePhase = phaseSamplePhase
      , phaseAggregateCalls = 1
      , phaseAggregateFailures = case phaseSampleStatus of
          PhaseSucceeded -> 0
          PhaseFailed -> 1
      , phaseAggregateElapsedNs = phaseSampleElapsedNs
      , phaseAggregateCpuNs = phaseSampleCpuNs
      , phaseAggregateAllocatedBytes = phaseSampleAllocatedBytes
      }

performancePhaseName :: PerformancePhase -> String
performancePhaseName = \case
  AnnKeyComparison -> "ann-key-comparison"
  AnnKeyMapOperations -> "ann-key-map-operations"
  GhcSession -> "ghc-session"
  GhcSessionSetup -> "ghc-session-setup"
  DynamicFlagParsing -> "dynamic-flag-parsing"
  SourceParsing -> "source-parsing"
  CommentRecovery -> "comment-recovery"
  AnnotationExtraction -> "annotation-extraction"
  AnnotationIndexConstruction -> "annotation-index-construction"
  InlineConfiguration -> "inline-configuration"
  CommentPlanning -> "comment-planning"
  TopLevelGrouping -> "top-level-grouping"
  LayoutAndRendering -> "layout-and-rendering"
  BriDocConstruction -> "bridoc-construction"
  CommentLowering -> "comment-lowering"
  AlternativeResolution -> "alternative-resolution"
  SimplifyFloating -> "simplify-floating"
  SimplifyPar -> "simplify-par"
  SimplifyColumns -> "simplify-columns"
  SimplifyIndent -> "simplify-indent"
  BackendRendering -> "backend-rendering"
  PlannedCommentValidation -> "planned-comment-validation"
  OutputValidation -> "output-validation"
  OutputParsing -> "output-parsing"
  SemanticValidation -> "semantic-validation"
  CommentValidation -> "comment-validation"

performanceCounterName :: PerformanceCounter -> String
performanceCounterName = \case
  RawBriDocNodes -> "raw-bridoc-nodes"
  PostCommentLoweringBriDocNodes -> "post-comment-lowering-bridoc-nodes"
  PostAlternativeBriDocNodes -> "post-alternative-bridoc-nodes"
  PostFloatingBriDocNodes -> "post-floating-bridoc-nodes"
  PostParBriDocNodes -> "post-par-bridoc-nodes"
  PostColumnsBriDocNodes -> "post-columns-bridoc-nodes"
  PostIndentBriDocNodes -> "post-indent-bridoc-nodes"
  BriDocAlternatives -> "bridoc-alternatives"
  BriDocAlternativeDepth -> "bridoc-alternative-depth"
  BriDocDelimiterGroups -> "bridoc-delimiter-groups"
  BriDocGeneratedVariants -> "bridoc-generated-variants"
  GetSpacingCalls -> "get-spacing-calls"
  GetSpacingsCalls -> "get-spacings-calls"
  GetSpacingsMemoHits -> "get-spacings-memo-hits"
  MaximumSpacingWidthBeforePruning -> "maximum-spacing-width-before-pruning"
  MaximumSpacingWidthAfterPruning -> "maximum-spacing-width-after-pruning"

captureRuntimeSnapshot :: IO RuntimeSnapshot
captureRuntimeSnapshot = do
  runtimeSnapshotElapsedNs <- getMonotonicTimeNSec
  cpuPicoseconds <- getCPUTime
  runtimeSnapshotAllocationCounter <- getAllocationCounter
  statsEnabled <- Stats.getRTSStatsEnabled
  runtimeStats <- if statsEnabled then Just <$> Stats.getRTSStats else pure Nothing
  pure RuntimeSnapshot
    { runtimeSnapshotElapsedNs
    , runtimeSnapshotCpuNs = fromInteger $ cpuPicoseconds `div` 1000
    , runtimeSnapshotAllocatedBytes = Stats.allocated_bytes <$> runtimeStats
    , runtimeSnapshotCopiedBytes = Stats.copied_bytes <$> runtimeStats
    , runtimeSnapshotGcCpuNs = fromIntegral . max 0 . Stats.gc_cpu_ns
        <$> runtimeStats
    , runtimeSnapshotMutatorCpuNs = fromIntegral
        . max 0 . Stats.mutator_cpu_ns <$> runtimeStats
    , runtimeSnapshotGcCount = fromIntegral . Stats.gcs <$> runtimeStats
    , runtimeSnapshotMaximumResidencyBytes = Stats.max_live_bytes <$> runtimeStats
    , runtimeSnapshotAllocationCounter
    }

runtimeMetricsDifference
  :: RuntimeSnapshot
  -> RuntimeSnapshot
  -> RuntimeMetrics
runtimeMetricsDifference before after = RuntimeMetrics
  { runtimeElapsedNs = difference runtimeSnapshotElapsedNs
  , runtimeCpuNs = difference runtimeSnapshotCpuNs
  , runtimeAllocatedBytes = optionalDifference runtimeSnapshotAllocatedBytes
  , runtimeCopiedBytes = optionalDifference runtimeSnapshotCopiedBytes
  , runtimeGcCpuNs = optionalDifference runtimeSnapshotGcCpuNs
  , runtimeMutatorCpuNs = optionalDifference runtimeSnapshotMutatorCpuNs
  , runtimeGcCount = optionalDifference runtimeSnapshotGcCount
  , runtimeMaximumResidencyBytes = runtimeSnapshotMaximumResidencyBytes after
  , runtimeProductivity = productivity
      (optionalDifference runtimeSnapshotMutatorCpuNs)
      (optionalDifference runtimeSnapshotGcCpuNs)
  }
 where
  difference field = saturatingDifference (field after) (field before)
  optionalDifference field = saturatingDifference <$> field after <*> field before
  productivity (Just mutator) (Just gc)
    | mutator > 0 || gc > 0 = Just
        $ fromIntegral mutator / (fromIntegral mutator + fromIntegral gc)
  productivity _ _ = Nothing

snapshotDifference
  :: PerformancePhase
  -> PhaseStatus
  -> RuntimeSnapshot
  -> RuntimeSnapshot
  -> PhaseSample
snapshotDifference phase status before after = PhaseSample
  { phaseSamplePhase = phase
  , phaseSampleStatus = status
  , phaseSampleElapsedNs = saturatingDifference
      (runtimeSnapshotElapsedNs after) (runtimeSnapshotElapsedNs before)
  , phaseSampleCpuNs = saturatingDifference
      (runtimeSnapshotCpuNs after) (runtimeSnapshotCpuNs before)
  , phaseSampleAllocatedBytes = saturatingDifference
      <$> Just (fromIntegral $ runtimeSnapshotAllocationCounter before)
      <*> Just (fromIntegral $ runtimeSnapshotAllocationCounter after)
  }

saturatingDifference :: Word64 -> Word64 -> Word64
saturatingDifference newer older
  | newer >= older = newer - older
  | otherwise = 0

recordSample :: PerformanceCollector -> PhaseSample -> IO ()
recordSample PerformanceCollector{collectorPhaseSamples = samplesRef} sample =
  atomicModifyIORef' samplesRef $ \samples -> (sample : samples, ())
