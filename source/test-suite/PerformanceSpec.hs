{-# LANGUAGE ScopedTypeVariables #-}

module PerformanceSpec (spec) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteStringL
import Data.Either (isLeft)
import Data.List (isPrefixOf)
import Language.Haskell.Brittany.Internal.Config (staticDefaultConfig)
import Language.Haskell.Brittany.Internal.Performance
import Language.Haskell.Brittany.Internal.Performance.Fixtures
import Language.Haskell.Brittany.Internal.Performance.Micro
import Language.Haskell.Brittany.Internal.Performance.Report
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Test.Hspec

spec :: Spec
spec = do
  describe "performance phase collection" $ do
    it "attributes and aggregates repeated phase samples" $ do
      let aggregates = aggregatePhaseSamples
            [ PhaseSample SourceParsing PhaseSucceeded 10 8 $ Just 100
            , PhaseSample SourceParsing PhaseFailed 5 4 $ Just 25
            ]
      aggregates `shouldBe`
        [PhaseAggregate SourceParsing 2 1 15 12 $ Just 125]

    it "records nested measurements independently" $ do
      collector <- newPerformanceCollector
      measurePhase (Just collector) GhcSession
        $ measurePhase (Just collector) SourceParsing $ pure ()
      samples <- readPerformanceSamples collector
      fmap phaseSamplePhase samples `shouldBe` [SourceParsing, GhcSession]

    it "records a failed phase before rethrowing its exception" $ do
      collector <- newPerformanceCollector
      result <- try (measurePhase (Just collector) SourceParsing
        $ ioError $ userError "expected") :: IO (Either SomeException ())
      result `shouldSatisfy` isLeft
      samples <- readPerformanceSamples collector
      fmap phaseSampleStatus samples `shouldBe` [PhaseFailed]

    it "does not require RTS statistics when collection is disabled" $ do
      measurePhase Nothing SourceParsing (pure (42 :: Int)) `shouldReturn` 42

    it "aggregates counter totals and maximum values" $ do
      collector <- newPerformanceCollector
      recordPerformanceCounter collector RawBriDocNodes 4
      recordPerformanceCounter collector RawBriDocNodes 3
      recordPerformanceCounter collector BriDocAlternativeDepth 2
      recordPerformanceCounter collector BriDocAlternativeDepth 5
      counters <- aggregatePerformanceCounters
        <$> readPerformanceCounters collector
      counters `shouldBe`
        [ CounterAggregate RawBriDocNodes 7
        , CounterAggregate BriDocAlternativeDepth 5
        ]

    it "keeps an empty counter collection empty" $ do
      aggregatePerformanceCounters [] `shouldBe` []

    it "can disable structure-forcing BriDoc diagnostics" $ do
      collector <- newPerformanceCollectorWithBriDocStructure False
      performanceCollectorProfilesBriDocStructure collector `shouldBe` False

    it "rejects unknown performance counters in JSON" $ do
      let decoded = Aeson.eitherDecode
            (ByteStringL.pack unknownCounterScenario)
            :: Either String ScenarioResult
      decoded `shouldSatisfy` isLeft

  describe "performance fixtures" $ do
    it "generates the requested number of similarly shaped declarations" $ do
      let input = declarationScalingInput 3
          declarations = filter ("value" `isPrefixOf`)
            $ lines $ benchmarkInputSource input
      declarations `shouldBe` ["value1 = 1", "value2 = 2", "value3 = 3"]
      benchmarkInputDeclarationCount input `shouldBe` Just 3

    it "clamps negative scaling values to an empty declaration set" $ do
      let input = declarationScalingInput (-1)
      benchmarkInputDeclarationCount input `shouldBe` Just 0
      benchmarkInputSource input `shouldBe`
        "module GeneratedDeclarations where\n\n"

    it "records the requested nesting depth deterministically" $ do
      let first = nestingScalingInput 4
          second = nestingScalingInput 4
      first `shouldBe` second
      benchmarkInputNestingDepth first `shouldBe` Just 4

    it "varies comment, delimiter, and declaration size independently" $ do
      benchmarkInputCommentCount (commentScalingInput 7) `shouldBe` Just 7
      benchmarkInputDelimiterGroupCount (delimiterCountScalingInput 5)
        `shouldBe` Just 5
      benchmarkInputNestingDepth (delimiterDepthScalingInput 5)
        `shouldBe` Just 5
      benchmarkInputAlternativeCount (layoutAlternativeScalingInput 7 3)
        `shouldBe` Just 7
      benchmarkInputAlternativeDepth (layoutAlternativeScalingInput 7 3)
        `shouldBe` Just 3
      benchmarkInputDeclarationSize (declarationSizeScalingInput 9)
        `shouldBe` Just 9

    it "exercises the parser error path without throwing" $ do
      parsed <- ParseModule.parseModuleWithMetrics Nothing []
        (benchmarkInputName malformedInput)
        (const $ pure $ Right ())
        (benchmarkInputSource malformedInput)
      case parsed of
        Left{} -> pure ()
        Right{} -> expectationFailure "malformed fixture unexpectedly parsed"

  describe "performance reports" $ do
    it "includes zero-valued entries for unobserved phases" $ do
      let phases = completePhaseAggregates []
      length phases `shouldBe`
        fromEnum (maxBound :: PerformancePhase) + 1
      phases `shouldSatisfy` all ((== 0) . phaseAggregateCalls)

    it "round-trips scenario results through the versioned JSON shape" $ do
      let result = sampleScenario BenchmarkSucceeded
          decoded = Aeson.eitherDecode $ Aeson.encode result
      decoded `shouldBe` Right result

    it "preserves expected error outcomes in machine-readable reports" $ do
      let result = sampleScenario $ BenchmarkExpectedFailure "parse error"
          decoded = Aeson.eitherDecode $ Aeson.encode result
      decoded `shouldBe` Right result

  describe "focused performance operations" $ do
    it "runs an AnnKey workload and attributes its phase" $ do
      collector <- newPerformanceCollector
      result <- runFocusedOperation collector staticDefaultConfig
        AnnKeyComparison (declarationScalingInput 0)
      result `shouldSatisfy` either (const False) (> 0)
      samples <- readPerformanceSamples collector
      fmap phaseSamplePhase samples `shouldBe` [AnnKeyComparison]

    it "round-trips a focused benchmark mode" $ do
      let mode = FocusedOperation BackendRendering
          decoded = Aeson.eitherDecode $ Aeson.encode mode
      decoded `shouldBe` Right mode

    it "rejects an unknown focused phase in JSON" $ do
      let decoded = Aeson.eitherDecode
            (ByteStringL.pack "\"focused-not-a-phase\"")
            :: Either String BenchmarkMode
      decoded `shouldSatisfy` isLeft

    it "reports an unsupported phase without throwing" $ do
      collector <- newPerformanceCollector
      result <- runFocusedOperation collector staticDefaultConfig GhcSession
        (declarationScalingInput 0)
      result `shouldBe` Left "unsupported focused phase: ghc-session"

    it "records a malformed layout operation before it fails" $ do
      collector <- newPerformanceCollector
      result <- try $ runFocusedOperation collector staticDefaultConfig
        AlternativeResolution malformedLayoutInput
        :: IO (Either SomeException (Either String Int))
      result `shouldSatisfy` isLeft
      samples <- readPerformanceSamples collector
      fmap phaseSampleStatus samples `shouldBe` [PhaseFailed]

sampleScenario :: BenchmarkOutcome -> ScenarioResult
sampleScenario outcome = ScenarioResult
  { scenarioName = "sample"
  , scenarioMode = ParseAndAnnotations
  , scenarioIterations = 1
  , scenarioInput = BenchmarkInputSummary "sample" "generated" 10 2
      (Just 1) Nothing (Just 0) (Just 0) (Just 0) (Just 0) (Just 1)
  , scenarioOutcome = outcome
  , scenarioFormatterErrors = 0
  , scenarioRuntime = RuntimeMetrics 10 9 (Just 8) (Just 7) (Just 2)
      (Just 6) (Just 1) (Just 5) (Just 0.75)
  , scenarioPhases = completePhaseAggregates
      [PhaseAggregate SourceParsing 1 0 4 3 (Just 2)]
  , scenarioCounters = [CounterAggregate RawBriDocNodes 7]
  }

unknownCounterScenario :: String
unknownCounterScenario =
  "{\"name\":\"sample\",\"mode\":\"parse-and-annotations\","
    ++ "\"iterations\":1,\"input\":{\"name\":\"sample\","
    ++ "\"origin\":\"generated\",\"bytes\":0,\"lines\":0},"
    ++ "\"outcome\":{\"status\":\"succeeded\"},\"formatterErrors\":0,"
    ++ "\"runtime\":{\"elapsedNs\":0,\"cpuNs\":0},\"phases\":[],"
    ++ "\"counters\":[{\"name\":\"unknown\",\"value\":1}]}"
