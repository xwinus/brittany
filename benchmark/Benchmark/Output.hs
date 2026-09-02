module Benchmark.Output
  ( renderSummary
  , renderComparison
  ) where

import qualified Data.Map.Strict as Map
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Language.Haskell.Brittany.Internal.Performance
import Language.Haskell.Brittany.Internal.Performance.Report
import Text.Printf (printf)

renderSummary :: BenchmarkReport -> String
renderSummary report = unlines
  $ [ "Brittany performance report"
    , "commit: " ++ metadataCommit (reportMetadata report)
        ++ if metadataDirty (reportMetadata report) then " (dirty)" else ""
    , ""
    , printf "%-29s %-39s %9s %11s %10s %s"
        "scenario" "mode" "cpu (s)" "alloc (GB)" "resid (MB)" "outcome"
    ]
  ++ fmap renderScenario (reportScenarios report)

renderComparison :: BenchmarkReport -> BenchmarkReport -> String
renderComparison baseline candidate = unlines
  $ [ "Comparison against " ++ metadataCommit (reportMetadata baseline)
    , printf "%-27s %12s %12s" "scenario" "CPU change" "alloc change"
    ]
  ++ fmap renderChange (reportScenarios candidate)
 where
  baselineByName = Map.fromList
    [(scenarioName result, result) | result <- reportScenarios baseline]
  renderChange result = case Map.lookup (scenarioName result) baselineByName of
    Nothing -> printf "%-27s %12s %12s" (scenarioName result) "new" "new"
    Just previous -> printf "%-27s %12s %12s"
      (scenarioName result)
      (percentageChange
        (scenarioCpuNs previous)
        (scenarioCpuNs result))
      (maybePercentageChange
        (scenarioAllocatedBytes previous)
        (scenarioAllocatedBytes result))

renderScenario :: ScenarioResult -> String
renderScenario result = printf "%-29s %-39s %9.3f %11s %10s %s"
  (scenarioName result)
  (benchmarkModeName $ scenarioMode result)
  (nanosecondsToSeconds $ scenarioCpuNs result)
  (renderGigabytes $ scenarioAllocatedBytes result)
  (renderMegabytes $ runtimeMaximumResidencyBytes $ scenarioRuntime result)
  (renderOutcome $ scenarioOutcome result)

renderOutcome :: BenchmarkOutcome -> String
renderOutcome BenchmarkSucceeded = "succeeded"
renderOutcome BenchmarkExpectedFailure{} = "expected failure"
renderOutcome (BenchmarkUnexpectedFailure message) = "FAILED: " ++ message
renderOutcome (BenchmarkHarnessError message) = "HARNESS ERROR: " ++ message

scenarioCpuNs :: ScenarioResult -> Word64
scenarioCpuNs result = maybe
  (runtimeCpuNs $ scenarioRuntime result)
  phaseAggregateCpuNs
  (focusedAggregate result)

scenarioAllocatedBytes :: ScenarioResult -> Maybe Word64
scenarioAllocatedBytes result = maybe
  (runtimeAllocatedBytes $ scenarioRuntime result)
  phaseAggregateAllocatedBytes
  (focusedAggregate result)

focusedAggregate :: ScenarioResult -> Maybe PhaseAggregate
focusedAggregate result = case scenarioMode result of
  FocusedOperation phase -> find ((== phase) . phaseAggregatePhase)
    $ scenarioPhases result
  _ -> Nothing

nanosecondsToSeconds :: Word64 -> Double
nanosecondsToSeconds value = fromIntegral value / 1.0e9

renderGigabytes :: Maybe Word64 -> String
renderGigabytes = maybe "n/a" $ printf "%.3f" . bytesToGigabytes

renderMegabytes :: Maybe Word64 -> String
renderMegabytes = maybe "n/a" $ printf "%.1f" . bytesToMegabytes

bytesToGigabytes :: Word64 -> Double
bytesToGigabytes value = fromIntegral value / 1.0e9

bytesToMegabytes :: Word64 -> Double
bytesToMegabytes value = fromIntegral value / 1.0e6

maybePercentageChange :: Maybe Word64 -> Maybe Word64 -> String
maybePercentageChange baseline candidate = fromMaybe "n/a"
  $ percentageChange <$> baseline <*> candidate

percentageChange :: Word64 -> Word64 -> String
percentageChange 0 _ = "n/a"
percentageChange baseline candidate = printf "%+.1f%%"
  (100 * (fromIntegral candidate - fromIntegral baseline)
    / fromIntegral baseline :: Double)
