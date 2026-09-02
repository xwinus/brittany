{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.Performance.Report
  ( BenchmarkMode(..)
  , BenchmarkOutcome(..)
  , BenchmarkInputSummary(..)
  , BenchmarkMetadata(..)
  , ScenarioResult(..)
  , BenchmarkReport(..)
  , benchmarkInputSummary
  , benchmarkModeName
  , completePhaseAggregates
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value
  , object
  , withObject
  , (.:)
  , (.:?)
  , (.=)
  )
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as ByteString
import qualified Data.Kind as Kind
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Language.Haskell.Brittany.Internal.Performance
import Language.Haskell.Brittany.Internal.Performance.Fixtures
import Language.Haskell.Brittany.Internal.Prelude
import qualified Prelude as Base

type BenchmarkMode :: Kind.Type
data BenchmarkMode
  = ParseAndAnnotations
  | FormatWithoutValidation
  | FullSafeFormatting
  | FocusedOperation PerformancePhase
  deriving (Eq, Ord, Show)

type BenchmarkOutcome :: Kind.Type
data BenchmarkOutcome
  = BenchmarkSucceeded
  | BenchmarkExpectedFailure String
  | BenchmarkUnexpectedFailure String
  | BenchmarkHarnessError String
  deriving (Eq, Show)

type BenchmarkInputSummary :: Kind.Type
data BenchmarkInputSummary = BenchmarkInputSummary
  { inputSummaryName :: String
  , inputSummaryOrigin :: String
  , inputSummaryBytes :: Int
  , inputSummaryLines :: Int
  , inputSummaryDeclarations :: Maybe Int
  , inputSummaryNestingDepth :: Maybe Int
  , inputSummaryComments :: Maybe Int
  , inputSummaryAlternatives :: Maybe Int
  , inputSummaryAlternativeDepth :: Maybe Int
  , inputSummaryDelimiterGroups :: Maybe Int
  , inputSummaryDeclarationSize :: Maybe Int
  }
  deriving (Eq, Show)

type BenchmarkMetadata :: Kind.Type
data BenchmarkMetadata = BenchmarkMetadata
  { metadataCommit :: String
  , metadataDirty :: Bool
  , metadataCompiler :: String
  , metadataCabal :: String
  , metadataOperatingSystem :: String
  , metadataArchitecture :: String
  , metadataMachine :: String
  , metadataConfiguration :: String
  , metadataOptimization :: String
  , metadataRtsStatsEnabled :: Bool
  , metadataCommand :: [String]
  }
  deriving (Eq, Show)

type ScenarioResult :: Kind.Type
data ScenarioResult = ScenarioResult
  { scenarioName :: String
  , scenarioMode :: BenchmarkMode
  , scenarioIterations :: Int
  , scenarioInput :: BenchmarkInputSummary
  , scenarioOutcome :: BenchmarkOutcome
  , scenarioFormatterErrors :: Int
  , scenarioRuntime :: RuntimeMetrics
  , scenarioPhases :: [PhaseAggregate]
  }
  deriving (Eq, Show)

type BenchmarkReport :: Kind.Type
data BenchmarkReport = BenchmarkReport
  { reportSchemaVersion :: Int
  , reportMetadata :: BenchmarkMetadata
  , reportScenarios :: [ScenarioResult]
  }
  deriving (Eq, Show)

benchmarkInputSummary :: BenchmarkInput -> BenchmarkInputSummary
benchmarkInputSummary BenchmarkInput
    { benchmarkInputName
    , benchmarkInputOrigin
    , benchmarkInputSource
    , benchmarkInputDeclarationCount
    , benchmarkInputNestingDepth
    , benchmarkInputCommentCount
    , benchmarkInputAlternativeCount
    , benchmarkInputAlternativeDepth
    , benchmarkInputDelimiterGroupCount
    , benchmarkInputDeclarationSize
    } = BenchmarkInputSummary
      { inputSummaryName = benchmarkInputName
      , inputSummaryOrigin = benchmarkInputOrigin
      , inputSummaryBytes = ByteString.length
          $ Text.Encoding.encodeUtf8 $ Text.pack benchmarkInputSource
      , inputSummaryLines = length $ Base.lines benchmarkInputSource
      , inputSummaryDeclarations = benchmarkInputDeclarationCount
      , inputSummaryNestingDepth = benchmarkInputNestingDepth
      , inputSummaryComments = benchmarkInputCommentCount
      , inputSummaryAlternatives = benchmarkInputAlternativeCount
      , inputSummaryAlternativeDepth = benchmarkInputAlternativeDepth
      , inputSummaryDelimiterGroups = benchmarkInputDelimiterGroupCount
      , inputSummaryDeclarationSize = benchmarkInputDeclarationSize
      }

completePhaseAggregates :: [PhaseAggregate] -> [PhaseAggregate]
completePhaseAggregates aggregates = fmap aggregateFor [minBound .. maxBound]
 where
  byPhase = Map.fromList
    [(phaseAggregatePhase aggregate, aggregate) | aggregate <- aggregates]
  aggregateFor phase = Map.findWithDefault (emptyAggregate phase) phase byPhase
  emptyAggregate phase = PhaseAggregate
    { phaseAggregatePhase = phase
    , phaseAggregateCalls = 0
    , phaseAggregateFailures = 0
    , phaseAggregateElapsedNs = 0
    , phaseAggregateCpuNs = 0
    , phaseAggregateAllocatedBytes = Nothing
    }

instance ToJSON BenchmarkMode where
  toJSON = toJSON . benchmarkModeName

instance FromJSON BenchmarkMode where
  parseJSON value = parseJSON value >>= parseBenchmarkMode

instance ToJSON BenchmarkOutcome where
  toJSON outcome = object
    [ "status" .= outcomeStatus outcome
    , "message" .= outcomeMessage outcome
    ]

instance FromJSON BenchmarkOutcome where
  parseJSON = withObject "BenchmarkOutcome" $ \value -> do
    status <- value .: "status"
    message <- value .:? "message"
    parseBenchmarkOutcome status message

instance ToJSON BenchmarkInputSummary where
  toJSON summary = object
    [ "name" .= inputSummaryName summary
    , "origin" .= inputSummaryOrigin summary
    , "bytes" .= inputSummaryBytes summary
    , "lines" .= inputSummaryLines summary
    , "declarations" .= inputSummaryDeclarations summary
    , "nestingDepth" .= inputSummaryNestingDepth summary
    , "comments" .= inputSummaryComments summary
    , "alternatives" .= inputSummaryAlternatives summary
    , "alternativeDepth" .= inputSummaryAlternativeDepth summary
    , "delimiterGroups" .= inputSummaryDelimiterGroups summary
    , "declarationSize" .= inputSummaryDeclarationSize summary
    ]

instance FromJSON BenchmarkInputSummary where
  parseJSON = withObject "BenchmarkInputSummary" $ \value ->
    BenchmarkInputSummary
      <$> value .: "name"
      <*> value .: "origin"
      <*> value .: "bytes"
      <*> value .: "lines"
      <*> value .:? "declarations"
      <*> value .:? "nestingDepth"
      <*> value .:? "comments"
      <*> value .:? "alternatives"
      <*> value .:? "alternativeDepth"
      <*> value .:? "delimiterGroups"
      <*> value .:? "declarationSize"

instance ToJSON BenchmarkMetadata where
  toJSON metadata = object
    [ "commit" .= metadataCommit metadata
    , "dirty" .= metadataDirty metadata
    , "compiler" .= metadataCompiler metadata
    , "cabal" .= metadataCabal metadata
    , "os" .= metadataOperatingSystem metadata
    , "architecture" .= metadataArchitecture metadata
    , "machine" .= metadataMachine metadata
    , "configuration" .= metadataConfiguration metadata
    , "optimization" .= metadataOptimization metadata
    , "rtsStatsEnabled" .= metadataRtsStatsEnabled metadata
    , "command" .= metadataCommand metadata
    ]

instance FromJSON BenchmarkMetadata where
  parseJSON = withObject "BenchmarkMetadata" $ \value -> BenchmarkMetadata
    <$> value .: "commit"
    <*> value .: "dirty"
    <*> value .: "compiler"
    <*> value .: "cabal"
    <*> value .: "os"
    <*> value .: "architecture"
    <*> value .: "machine"
    <*> value .: "configuration"
    <*> value .: "optimization"
    <*> value .: "rtsStatsEnabled"
    <*> value .: "command"

instance ToJSON ScenarioResult where
  toJSON result = object
    [ "name" .= scenarioName result
    , "mode" .= scenarioMode result
    , "iterations" .= scenarioIterations result
    , "input" .= scenarioInput result
    , "outcome" .= scenarioOutcome result
    , "formatterErrors" .= scenarioFormatterErrors result
    , "runtime" .= runtimeMetricsValue (scenarioRuntime result)
    , "phases" .= fmap phaseAggregateValue (scenarioPhases result)
    ]

instance FromJSON ScenarioResult where
  parseJSON = withObject "ScenarioResult" $ \value ->
    ScenarioResult
      <$> value .: "name"
      <*> value .: "mode"
      <*> value .: "iterations"
      <*> value .: "input"
      <*> value .: "outcome"
      <*> value .: "formatterErrors"
      <*> (value .: "runtime" >>= parseRuntimeMetrics)
      <*> (value .: "phases" >>= traverse parsePhaseAggregate)

instance ToJSON BenchmarkReport where
  toJSON report = object
    [ "schemaVersion" .= reportSchemaVersion report
    , "metadata" .= reportMetadata report
    , "scenarios" .= reportScenarios report
    ]

instance FromJSON BenchmarkReport where
  parseJSON = withObject "BenchmarkReport" $ \value -> BenchmarkReport
    <$> value .: "schemaVersion"
    <*> value .: "metadata"
    <*> value .: "scenarios"

benchmarkModeName :: BenchmarkMode -> String
benchmarkModeName = \case
  ParseAndAnnotations -> "parse-and-annotations"
  FormatWithoutValidation -> "format-without-validation"
  FullSafeFormatting -> "full-safe-formatting"
  FocusedOperation phase -> "focused-" ++ performancePhaseName phase

parseBenchmarkMode :: String -> Parser BenchmarkMode
parseBenchmarkMode = \case
  "parse-and-annotations" -> pure ParseAndAnnotations
  "format-without-validation" -> pure FormatWithoutValidation
  "full-safe-formatting" -> pure FullSafeFormatting
  unknown -> case List.stripPrefix "focused-" unknown of
    Nothing -> Base.fail $ "unknown benchmark mode: " ++ unknown
    Just name -> maybe
      (Base.fail $ "unknown focused benchmark phase: " ++ name)
      (pure . FocusedOperation)
      (Base.lookup name phaseNames)
 where
  phaseNames =
    [(performancePhaseName phase, phase) | phase <- [minBound .. maxBound]]

outcomeStatus :: BenchmarkOutcome -> String
outcomeStatus = \case
  BenchmarkSucceeded -> "succeeded"
  BenchmarkExpectedFailure{} -> "expected-failure"
  BenchmarkUnexpectedFailure{} -> "unexpected-failure"
  BenchmarkHarnessError{} -> "harness-error"

outcomeMessage :: BenchmarkOutcome -> Maybe String
outcomeMessage = \case
  BenchmarkSucceeded -> Nothing
  BenchmarkExpectedFailure message -> Just message
  BenchmarkUnexpectedFailure message -> Just message
  BenchmarkHarnessError message -> Just message

parseBenchmarkOutcome :: String -> Maybe String -> Parser BenchmarkOutcome
parseBenchmarkOutcome status message = case status of
  "succeeded" -> pure BenchmarkSucceeded
  "expected-failure" -> BenchmarkExpectedFailure <$> requiredMessage
  "unexpected-failure" -> BenchmarkUnexpectedFailure <$> requiredMessage
  "harness-error" -> BenchmarkHarnessError <$> requiredMessage
  _ -> Base.fail $ "unknown benchmark outcome: " ++ status
 where
  requiredMessage = maybe
    (Base.fail "benchmark outcome message is missing") pure message

runtimeMetricsValue :: RuntimeMetrics -> Value
runtimeMetricsValue metrics = object
  [ "elapsedNs" .= runtimeElapsedNs metrics
  , "cpuNs" .= runtimeCpuNs metrics
  , "allocatedBytes" .= runtimeAllocatedBytes metrics
  , "copiedBytes" .= runtimeCopiedBytes metrics
  , "gcCpuNs" .= runtimeGcCpuNs metrics
  , "mutatorCpuNs" .= runtimeMutatorCpuNs metrics
  , "gcCount" .= runtimeGcCount metrics
  , "maximumResidencyBytes" .= runtimeMaximumResidencyBytes metrics
  , "productivity" .= runtimeProductivity metrics
  ]

parseRuntimeMetrics :: Value -> Parser RuntimeMetrics
parseRuntimeMetrics = withObject "RuntimeMetrics" $ \value -> RuntimeMetrics
  <$> value .: "elapsedNs"
  <*> value .: "cpuNs"
  <*> value .:? "allocatedBytes"
  <*> value .:? "copiedBytes"
  <*> value .:? "gcCpuNs"
  <*> value .:? "mutatorCpuNs"
  <*> value .:? "gcCount"
  <*> value .:? "maximumResidencyBytes"
  <*> value .:? "productivity"

phaseAggregateValue :: PhaseAggregate -> Value
phaseAggregateValue aggregate = object
  [ "name" .= performancePhaseName (phaseAggregatePhase aggregate)
  , "calls" .= phaseAggregateCalls aggregate
  , "failures" .= phaseAggregateFailures aggregate
  , "elapsedNs" .= phaseAggregateElapsedNs aggregate
  , "cpuNs" .= phaseAggregateCpuNs aggregate
  , "allocatedBytes" .= phaseAggregateAllocatedBytes aggregate
  ]

parsePhaseAggregate :: Value -> Parser PhaseAggregate
parsePhaseAggregate = withObject "PhaseAggregate" $ \value -> do
  name <- value .: "name"
  phase <- maybe (Base.fail $ "unknown performance phase: " ++ name) pure
    $ Base.lookup name
      [(performancePhaseName candidate, candidate) | candidate <- [minBound .. maxBound]]
  PhaseAggregate phase
    <$> value .: "calls"
    <*> value .: "failures"
    <*> value .: "elapsedNs"
    <*> value .: "cpuNs"
    <*> value .:? "allocatedBytes"
