{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Benchmark.Scenario
  ( BenchmarkSuite(..)
  , ScenarioSpec(..)
  , scenarioNames
  , loadScenario
  , runScenario
  , failedScenarioResult
  ) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM, replicateM)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.CZipWith (cZipWith)
import Data.Functor.Identity (runIdentity)
import Data.Function ((&))
import qualified Data.Kind as Kind
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import Language.Haskell.Brittany.Internal
  ( extractCommentConfigs
  , getTopLevelDeclNameMap
  )
import Language.Haskell.Brittany.Internal.Config (readConfigs)
import Language.Haskell.Brittany.Internal.Config.Types
  ( Config
  , _conf_forward
  , _options_ghc
  )
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.Performance
import Language.Haskell.Brittany.Internal.Performance.Fixtures
import Language.Haskell.Brittany.Internal.Performance.Micro
  ( runFocusedOperation )
import Language.Haskell.Brittany.Internal.Performance.Pipeline
  ( pPrintModuleAndCheckWithSourceMeasured
  , pPrintModuleWithSourceMeasured
  )
import Language.Haskell.Brittany.Internal.Performance.Report
import Language.Haskell.Brittany.Internal.Utils (fromOptionIdentity)
import System.FilePath (makeRelative, (</>))
import System.Mem (performGC)

type BenchmarkSuite :: Kind.Type
data BenchmarkSuite
  = QuickSuite
  | StandardSuite
  | ScalingSuite
  | MicroSuite
  deriving (Eq, Show)

type ScenarioSpec :: Kind.Type
data ScenarioSpec = ScenarioSpec
  { specName :: String
  , specMode :: BenchmarkMode
  , specInputs :: [BenchmarkInput]
  , specIterations :: Int
  , specExpectsFailure :: Bool
  }
  deriving (Eq, Show)

scenarioNames :: BenchmarkSuite -> [String]
scenarioNames = \case
  QuickSuite ->
    [ "quick-parse"
    , "quick-format"
    , "quick-full"
    , "micro-ann-key-comparison"
    , "malformed-parse"
    ]
  StandardSuite ->
    [ "alt-parse"
    , "alt-format"
    , "alt-full"
    , "extract-parse"
    , "warm-small-full"
    , "batch-small-full"
    ] ++ scenarioNames MicroSuite
      ++ scenarioNames ScalingSuite
      ++ ["malformed-parse"]
  ScalingSuite ->
    [ "declarations-50-parse"
    , "declarations-100-parse"
    , "declarations-200-parse"
    , "declarations-400-parse"
    , "declarations-50-grouping"
    , "declarations-100-grouping"
    , "declarations-200-grouping"
    , "declarations-400-grouping"
    , "nesting-5-format"
    , "nesting-10-format"
    , "nesting-15-format"
    , "comments-100-format"
    , "alternatives-count-100"
    , "alternatives-count-200"
    , "alternatives-depth-2"
    , "alternatives-depth-4"
    , "delimiter-count-10-format"
    , "delimiter-count-25-format"
    , "delimiter-depth-10-format"
    , "delimiter-depth-25-format"
    , "declaration-size-400-format"
    ]
  MicroSuite ->
    [ "micro-ann-key-comparison"
    , "micro-ann-key-map"
    , "micro-annotation-extraction"
    , "micro-comment-plan"
    , "micro-top-level-grouping"
    , "micro-annotation-index"
    , "micro-alternative-resolution"
    , "micro-simplify-floating"
    , "micro-simplify-par"
    , "micro-simplify-columns"
    , "micro-simplify-indent"
    , "micro-backend-rendering"
    , "micro-semantic-comparison"
    , "micro-comment-comparison"
    , "malformed-layout"
    ]

loadScenario :: FilePath -> String -> IO (Either String ScenarioSpec)
loadScenario projectRoot name = case name of
  "quick-parse" -> pure $ Right $ one name ParseAndAnnotations
    (declarationScalingInput 5)
  "quick-format" -> pure $ Right $ one name FormatWithoutValidation
    (declarationScalingInput 5)
  "quick-full" -> pure $ Right $ one name FullSafeFormatting
    (declarationScalingInput 5)
  "micro-ann-key-comparison" -> focused AnnKeyComparison declarationInput
  "micro-ann-key-map" -> focused AnnKeyMapOperations declarationInput
  "micro-annotation-extraction" -> focused AnnotationExtraction declarationInput
  "micro-comment-plan" -> focused CommentPlanning commentInput
  "micro-top-level-grouping" -> focused TopLevelGrouping declarationInput
  "micro-annotation-index" -> focused AnnotationIndexConstruction declarationInput
  "micro-alternative-resolution" -> focused AlternativeResolution layoutInput
  "micro-simplify-floating" -> focused SimplifyFloating layoutInput
  "micro-simplify-par" -> focused SimplifyPar layoutInput
  "micro-simplify-columns" -> focused SimplifyColumns layoutInput
  "micro-simplify-indent" -> focused SimplifyIndent layoutInput
  "micro-backend-rendering" -> focused BackendRendering layoutInput
  "micro-semantic-comparison" -> focused SemanticValidation declarationInput
  "micro-comment-comparison" -> focused CommentValidation commentInput
  "malformed-layout" -> pure $ Right
    $ (one name (FocusedOperation AlternativeResolution) malformedLayoutInput)
      { specExpectsFailure = True }
  "malformed-parse" -> pure $ Right
    $ (one name ParseAndAnnotations malformedInput) { specExpectsFailure = True }
  "alt-parse" -> source name ParseAndAnnotations altPath
  "alt-format" -> source name FormatWithoutValidation altPath
  "alt-full" -> source name FullSafeFormatting altPath
  "extract-parse" -> source name ParseAndAnnotations extractPath
  "warm-small-full" -> pure $ Right
    $ (one name FullSafeFormatting $ declarationScalingInput 10)
      { specIterations = 5 }
  "batch-small-full" -> pure $ Right ScenarioSpec
    { specName = name
    , specMode = FullSafeFormatting
    , specInputs = fmap declarationScalingInput [5 .. 14]
    , specIterations = 1
    , specExpectsFailure = False
    }
  "declarations-50-parse" -> declarations name 50
  "declarations-100-parse" -> declarations name 100
  "declarations-200-parse" -> declarations name 200
  "declarations-400-parse" -> declarations name 400
  "declarations-50-grouping" -> declarationGrouping name 50
  "declarations-100-grouping" -> declarationGrouping name 100
  "declarations-200-grouping" -> declarationGrouping name 200
  "declarations-400-grouping" -> declarationGrouping name 400
  "nesting-5-format" -> nesting name 5
  "nesting-10-format" -> nesting name 10
  "nesting-15-format" -> nesting name 15
  "comments-100-format" -> pure $ Right
    $ one name FormatWithoutValidation $ commentScalingInput 100
  "alternatives-count-100" -> alternatives 100 1
  "alternatives-count-200" -> alternatives 200 1
  "alternatives-depth-2" -> alternatives 100 2
  "alternatives-depth-4" -> alternatives 100 4
  "delimiter-count-10-format" -> pure $ Right
    $ one name FormatWithoutValidation $ delimiterCountScalingInput 10
  "delimiter-count-25-format" -> pure $ Right
    $ one name FormatWithoutValidation $ delimiterCountScalingInput 25
  "delimiter-depth-10-format" -> pure $ Right
    $ one name FormatWithoutValidation $ delimiterDepthScalingInput 10
  "delimiter-depth-25-format" -> pure $ Right
    $ one name FormatWithoutValidation $ delimiterDepthScalingInput 25
  "declaration-size-400-format" -> pure $ Right
    $ one name FormatWithoutValidation $ declarationSizeScalingInput 400
  unknown -> pure $ Left $ "unknown benchmark scenario: " ++ unknown
 where
  altPath = projectRoot </>
    "source/library/Language/Haskell/Brittany/Internal/Transformations/Alt.hs"
  extractPath = projectRoot </>
    "source/library/Language/Haskell/Brittany/Internal/ExtractAnns.hs"
  one scenarioName mode input = ScenarioSpec
    { specName = scenarioName
    , specMode = mode
    , specInputs = [input]
    , specIterations = 1
    , specExpectsFailure = False
    }
  source scenarioName mode path = do
    absoluteInput <- sourceFileInput path
    let input = absoluteInput
          { benchmarkInputName = makeRelative projectRoot path }
    pure $ Right $ one scenarioName mode input
  declarations scenarioName count = pure $ Right
    $ one scenarioName ParseAndAnnotations $ declarationScalingInput count
  declarationGrouping scenarioName count = pure $ Right
    $ one scenarioName (FocusedOperation TopLevelGrouping)
    $ declarationScalingInput count
  nesting scenarioName depth = pure $ Right
    $ one scenarioName FormatWithoutValidation $ nestingScalingInput depth
  focused phase input = pure $ Right $ one name (FocusedOperation phase) input
  alternatives count depth = focused AlternativeResolution
    $ layoutAlternativeScalingInput count depth
  declarationInput = declarationScalingInput 200
  commentInput = commentScalingInput 100
  layoutInput = layoutAlternativeScalingInput 2000 1

runScenario :: FilePath -> ScenarioSpec -> IO ScenarioResult
runScenario configPath spec@ScenarioSpec
    { specName
    , specMode
    , specInputs
    , specIterations
    , specExpectsFailure
    } = do
  configResult <- runMaybeT $ readConfigs mempty [configPath]
  case configResult of
    Nothing -> pure $ configurationFailure spec configPath
    Just config -> do
      collector <- newPerformanceCollector
      performGC
      before <- captureRuntimeSnapshot
      execution <- try $ concat <$> replicateM specIterations
        (forM specInputs $ runInput collector config specMode)
      performGC
      after <- captureRuntimeSnapshot
      samples <- readPerformanceSamples collector
      let runtime = runtimeMetricsDifference before after
          phases = completePhaseAggregates $ aggregatePhaseSamples samples
          (outcome, formatterErrors) = classifyOutcome
            specExpectsFailure execution
      pure ScenarioResult
        { scenarioName = specName
        , scenarioMode = specMode
        , scenarioIterations = specIterations
        , scenarioInput = summarizeInputs specInputs
        , scenarioOutcome = outcome
        , scenarioFormatterErrors = formatterErrors
        , scenarioRuntime = runtime
        , scenarioPhases = phases
        }

runInput
  :: PerformanceCollector
  -> Config
  -> BenchmarkMode
  -> BenchmarkInput
  -> IO (Either String Int)
runInput collector config mode input = do
  case mode of
    FocusedOperation phase -> runFocusedOperation collector config phase input
    _ -> runFormatterInput collector config mode input

runFormatterInput
  :: PerformanceCollector
  -> Config
  -> BenchmarkMode
  -> BenchmarkInput
  -> IO (Either String Int)
runFormatterInput collector config mode input = do
  let source = benchmarkInputSource input
      ghcOptions = config & _conf_forward & _options_ghc & runIdentity
      metrics = Just collector
  parseResult <- ParseModule.parseModuleWithMetrics metrics
    ghcOptions
    (benchmarkInputName input)
    (const $ pure $ Right ())
    source
  case parseResult of
    Left parseError -> pure $ Left parseError
    Right (annotations, parsedModule, _) -> do
      inlineResult <- measurePhase metrics InlineConfiguration
        $ evaluate
        $ extractCommentConfigs annotations
        $ getTopLevelDeclNameMap parsedModule
      case inlineResult of
        Left inlineError -> pure $ Left $ show inlineError
        Right (inlineConfig, perItemConfig) -> case mode of
          ParseAndAnnotations -> pure $ Right 0
          FocusedOperation{} ->
            pure $ Left "focused operation reached formatter path"
          FormatWithoutValidation -> do
            let moduleConfig = cZipWith fromOptionIdentity config inlineConfig
                originalSource = Just $ Text.pack source
            (errors, output) <- pPrintModuleWithSourceMeasured metrics
              originalSource moduleConfig perItemConfig annotations parsedModule
            _ <- evaluateOutput output
            pure $ Right $ length errors
          FullSafeFormatting -> do
            let moduleConfig = cZipWith fromOptionIdentity config inlineConfig
                originalSource = Just $ Text.pack source
            (errors, output) <- pPrintModuleAndCheckWithSourceMeasured metrics
              originalSource moduleConfig perItemConfig annotations parsedModule
            _ <- evaluateOutput output
            pure $ Right $ length errors

evaluateOutput :: TextL.Text -> IO Int
evaluateOutput output = evaluate $ fromIntegral $ TextL.length output

classifyOutcome
  :: Bool
  -> Either SomeException [Either String Int]
  -> (BenchmarkOutcome, Int)
classifyOutcome expectsFailure = \case
  Left exception
    | expectsFailure ->
        (BenchmarkExpectedFailure $ displayException exception, 0)
    | otherwise -> (BenchmarkHarnessError $ displayException exception, 0)
  Right results ->
    let failures = [message | Left message <- results]
        errorCount = sum [count | Right count <- results]
    in case (expectsFailure, failures) of
      (True, message : _) -> (BenchmarkExpectedFailure message, errorCount)
      (True, []) ->
        (BenchmarkUnexpectedFailure "scenario unexpectedly succeeded", errorCount)
      (False, message : _) -> (BenchmarkUnexpectedFailure message, errorCount)
      (False, []) -> (BenchmarkSucceeded, errorCount)

summarizeInputs :: [BenchmarkInput] -> BenchmarkInputSummary
summarizeInputs [input] = benchmarkInputSummary input
summarizeInputs inputs = BenchmarkInputSummary
  { inputSummaryName = "batch-" ++ show (length inputs) ++ "-modules"
  , inputSummaryOrigin = "generated-batch"
  , inputSummaryBytes = sum $ inputSummaryBytes <$> summaries
  , inputSummaryLines = sum $ inputSummaryLines <$> summaries
  , inputSummaryDeclarations = Just $ sum
      [count | Just count <- benchmarkInputDeclarationCount <$> inputs]
  , inputSummaryNestingDepth = Nothing
  , inputSummaryComments = Just $ sum
      [count | Just count <- benchmarkInputCommentCount <$> inputs]
  , inputSummaryAlternatives = Just $ sum
      [count | Just count <- benchmarkInputAlternativeCount <$> inputs]
  , inputSummaryAlternativeDepth = Nothing
  , inputSummaryDelimiterGroups = Just $ sum
      [count | Just count <- benchmarkInputDelimiterGroupCount <$> inputs]
  , inputSummaryDeclarationSize = Nothing
  }
 where
  summaries = benchmarkInputSummary <$> inputs

configurationFailure :: ScenarioSpec -> FilePath -> ScenarioResult
configurationFailure ScenarioSpec
    { specName
    , specMode
    , specInputs
    , specIterations
    } configPath = ScenarioResult
      { scenarioName = specName
      , scenarioMode = specMode
      , scenarioIterations = specIterations
      , scenarioInput = summarizeInputs specInputs
      , scenarioOutcome = BenchmarkHarnessError
          $ "cannot load configuration: " ++ configPath
      , scenarioFormatterErrors = 0
      , scenarioRuntime = RuntimeMetrics 0 0 Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing
      , scenarioPhases = completePhaseAggregates []
      }

failedScenarioResult :: ScenarioSpec -> String -> ScenarioResult
failedScenarioResult ScenarioSpec
    { specName
    , specMode
    , specInputs
    , specIterations
    } message = ScenarioResult
      { scenarioName = specName
      , scenarioMode = specMode
      , scenarioIterations = specIterations
      , scenarioInput = summarizeInputs specInputs
      , scenarioOutcome = BenchmarkHarnessError message
      , scenarioFormatterErrors = 0
      , scenarioRuntime = RuntimeMetrics 0 0 Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing
      , scenarioPhases = completePhaseAggregates []
      }
