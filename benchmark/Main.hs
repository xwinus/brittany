{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Main (main) where

import Benchmark.Output (renderComparison, renderSummary)
import Benchmark.Scenario
import Control.Exception (bracket_)
import Control.Monad (forM, forM_, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteStringL
import qualified Data.Kind as Kind
import Language.Haskell.Brittany.Internal.Performance.Report
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , getCurrentDirectory
  , setCurrentDirectory
  )
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath (takeDirectory, (</>))
import qualified System.Info as Info
import System.Process (readProcessWithExitCode)

type Options :: Kind.Type
data Options = Options
  { optionSuite :: BenchmarkSuite
  , optionOutput :: FilePath
  , optionCompare :: Maybe FilePath
  , optionConfiguration :: FilePath
  }

main :: IO ()
main = do
  arguments <- getArgs
  projectRoot <- findProjectRoot =<< getCurrentDirectory
  case arguments of
    "--worker" : scenario : "--project-root" : root
        : "--config" : config : [] -> runWorker root config scenario
    _ -> case parseOptions (defaultOptions projectRoot) arguments of
      Left message -> putStrLn message >> exitFailure
      Right options -> runController projectRoot arguments options

defaultOptions :: FilePath -> Options
defaultOptions projectRoot = Options
  { optionSuite = QuickSuite
  , optionOutput = projectRoot </> "benchmark-results.json"
  , optionCompare = Nothing
  , optionConfiguration = projectRoot </> "data/brittany.yaml"
  }

parseOptions :: Options -> [String] -> Either String Options
parseOptions options = \case
  [] -> Right options
  "--suite" : value : remaining -> do
    suite <- parseSuite value
    parseOptions options { optionSuite = suite } remaining
  "--output" : value : remaining ->
    parseOptions options { optionOutput = value } remaining
  "--compare" : value : remaining ->
    parseOptions options { optionCompare = Just value } remaining
  "--config" : value : remaining ->
    parseOptions options { optionConfiguration = value } remaining
  ["--help"] -> Left usage
  unknown -> Left $ "unknown benchmark arguments: " ++ unwords unknown
    ++ "\n\n" ++ usage

parseSuite :: String -> Either String BenchmarkSuite
parseSuite = \case
  "quick" -> Right QuickSuite
  "standard" -> Right StandardSuite
  "scaling" -> Right ScalingSuite
  "micro" -> Right MicroSuite
  unknown -> Left $ "unknown benchmark suite: " ++ unknown

usage :: String
usage = unlines
  [ "Usage: brittany-performance [OPTIONS]"
  , ""
  , "  --suite quick|standard|scaling|micro"
  , "  --output PATH"
  , "  --compare BASELINE.json"
  , "  --config PATH"
  ]

runController :: FilePath -> [String] -> Options -> IO ()
runController projectRoot arguments options = do
  executable <- getExecutablePath
  results <- forM (scenarioNames $ optionSuite options) $ \scenario -> do
    loaded <- loadScenario projectRoot scenario
    case loaded of
      Left message -> pure $ Left message
      Right spec -> do
        (exitCode, stdoutText, stderrText) <- readProcessWithExitCode executable
          [ "--worker"
          , scenario
          , "--project-root"
          , projectRoot
          , "--config"
          , optionConfiguration options
          , "+RTS"
          , "-T"
          , "-RTS"
          ]
          ""
        pure $ decodeWorkerResult spec exitCode stdoutText stderrText
  metadata <- collectMetadata projectRoot arguments options
  let scenarioResults =
        [ either (\message -> controllerFailure (name, message)) id result
        | (name, result) <- zip (scenarioNames $ optionSuite options) results
        ]
      report = BenchmarkReport
        { reportSchemaVersion = 1
        , reportMetadata = metadata
        , reportScenarios = scenarioResults
        }
  createDirectoryIfMissing True $ takeDirectory $ optionOutput options
  Aeson.encodeFile (optionOutput options) report
  putStrLn $ renderSummary report
  putStrLn $ "JSON report: " ++ optionOutput options
  forM_ (optionCompare options) $ \baselinePath -> do
    baselineResult <- Aeson.eitherDecodeFileStrict baselinePath
    case baselineResult of
      Left message -> putStrLn ("cannot read baseline: " ++ message) >> exitFailure
      Right baseline -> putStrLn $ renderComparison baseline report
  when (any failed $ reportScenarios report) exitFailure
 where
  controllerFailure (name, message) = failedScenarioResult
    ScenarioSpec
      { specName = name
      , specMode = ParseAndAnnotations
      , specInputs = []
      , specIterations = 0
      , specExpectsFailure = False
      }
    message

runWorker :: FilePath -> FilePath -> String -> IO ()
runWorker projectRoot configPath scenario = do
  loadScenario projectRoot scenario >>= \case
    Left message -> putStrLn message >> exitFailure
    Right spec -> runScenario configPath spec >>= ByteStringL.putStrLn . Aeson.encode

decodeWorkerResult
  :: ScenarioSpec
  -> ExitCode
  -> String
  -> String
  -> Either String ScenarioResult
decodeWorkerResult _spec exitCode stdoutText stderrText = case exitCode of
  ExitFailure code -> Left $ "worker exited with " ++ show code
    ++ stderrSuffix stderrText
  ExitSuccess -> case Aeson.eitherDecode $ ByteStringL.pack stdoutText of
    Left message -> Left $ "invalid worker report: " ++ message
      ++ stderrSuffix stderrText
    Right result -> Right result
 where
  stderrSuffix "" = ""
  stderrSuffix value = ": " ++ value

collectMetadata :: FilePath -> [String] -> Options -> IO BenchmarkMetadata
collectMetadata projectRoot arguments options = do
  commit <- commandOutput projectRoot "git" ["rev-parse", "HEAD"]
  dirtyOutput <- commandOutput projectRoot "git" ["status", "--porcelain"]
  cabalVersion <- commandOutput projectRoot "cabal" ["--numeric-version"]
  ghcVersion <- commandOutput projectRoot "ghc" ["--numeric-version"]
  machine <- commandOutput projectRoot "uname" ["-a"]
  pure BenchmarkMetadata
    { metadataCommit = commit
    , metadataDirty = not $ null dirtyOutput
    , metadataCompiler = Info.compilerName ++ "-" ++ ghcVersion
    , metadataCabal = cabalVersion
    , metadataOperatingSystem = Info.os
    , metadataArchitecture = Info.arch
    , metadataMachine = machine
    , metadataConfiguration = optionConfiguration options
    , metadataOptimization = "Cabal default optimized -O1 profile"
    , metadataRtsStatsEnabled = True
    , metadataCommand = "brittany-performance" : arguments
    }

commandOutput :: FilePath -> FilePath -> [String] -> IO String
commandOutput workingDirectory command arguments = do
  originalDirectory <- getCurrentDirectory
  (exitCode, stdoutText, _) <- bracket_
    (setCurrentDirectory workingDirectory)
    (setCurrentDirectory originalDirectory)
    (readProcessWithExitCode command arguments "")
  pure $ case exitCode of
    ExitSuccess -> trim stdoutText
    ExitFailure{} -> "unavailable"

trim :: String -> String
trim = reverse . dropWhile (`elem` ['\n', '\r', ' ', '\t']) . reverse

failed :: ScenarioResult -> Bool
failed result = case scenarioOutcome result of
  BenchmarkUnexpectedFailure{} -> True
  BenchmarkHarnessError{} -> True
  _ -> False

findProjectRoot :: FilePath -> IO FilePath
findProjectRoot directory = do
  found <- doesDirectoryExist $ directory </> "data"
  if found
    then pure directory
    else do
      let parent = takeDirectory directory
      if parent == directory then pure directory else findProjectRoot parent
