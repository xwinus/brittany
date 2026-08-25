module FallbackSpec (spec) where

import qualified CompatibilityMatrix as Matrix
import qualified Control.Monad as Monad
import Data.Functor.Identity (Identity(..))
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Semigroup as Semigroup
import qualified Data.Text.IO as Text
import Language.Haskell.Brittany
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "fallback inventory and reporting" $ do
  Hspec.it "classifies every fallback exactly once" $ do
    let identifiers = fallbackId <$> fallbackInventory
    identifiers `Hspec.shouldBe` [minBound .. maxBound]
    List.nub identifiers `Hspec.shouldBe` identifiers

  Hspec.it "documents a trigger, reason, and existing test for every fallback" $
    Monad.forM_ fallbackInventory $ \fallback -> do
      fallbackTrigger fallback `Hspec.shouldNotBe` ""
      fallbackReason fallback `Hspec.shouldNotBe` ""
      fallbackTests fallback `Hspec.shouldNotBe` []
      Monad.forM_ (fallbackTests fallback) $ \testPath ->
        Directory.doesFileExist (FilePath.combine projectRoot testPath)
          `Hspec.shouldReturn` True

  Hspec.it "keeps the reference documentation synchronized with the registry" $ do
    documentation <- readFile $ FilePath.combine projectRoot "doc/fallbacks.md"
    Monad.forM_ fallbackInventory $ \fallback ->
      documentation `Hspec.shouldContain` show (fallbackId fallback)

  Hspec.it "links fallback features to exact-source matrix classifications" $ do
    loaded <- Matrix.loadMatrix
      $ FilePath.combine projectRoot "data/compatibility.yaml"
    case loaded of
      Left loadError -> Hspec.expectationFailure loadError
      Right matrix -> Monad.forM_ fallbackInventory $ \fallback ->
        Monad.forM_ (fallbackFeatures fallback) $ \featureName ->
          case List.find ((== featureName) . Matrix.featureName)
              (Matrix.matrixFeatures matrix) of
            Nothing -> Hspec.expectationFailure
              $ "missing compatibility feature " ++ featureName
            Just feature ->
              Matrix.featureSupport feature `Hspec.shouldBe` Matrix.ExactSource

  Hspec.it "does not report pass-through paths unless explicitly requested" $ do
    messages <- runFormatter projectRoot staticDefaultConfig
      "SpecialisePragmasEdge.hs"
    messages `Hspec.shouldNotSatisfy` any (List.isInfixOf "Fallback")

  Hspec.it "reports scoped pass-through without changing formatter behavior" $ do
    messages <- runFormatter projectRoot fallbackReportingConfig
      "SpecialisePragmasEdge.hs"
    messages `Hspec.shouldSatisfy` any (List.isInfixOf "SignatureFallback")
    messages `Hspec.shouldNotSatisfy` any
      (List.isInfixOf "WholeModuleFallback")

  Hspec.it "keeps a strict quasiquote fallback at inline expression scope" $ do
    (result, messages) <- runFormatterResult projectRoot strictFallbackConfig
      True
      "ScopedExpressionFallbackInput.hs"
      Nothing
    (result == Left 70) `Hspec.shouldBe` True
    messages `Hspec.shouldSatisfy` any
      (List.isInfixOf "ExpressionFallback")
    messages `Hspec.shouldSatisfy` any (List.isInfixOf "inline scope")
    messages `Hspec.shouldNotSatisfy` any
      (List.isInfixOf "DeclarationFallback")
    messages `Hspec.shouldNotSatisfy` any
      (List.isInfixOf "WholeModuleFallback")

  Hspec.it "succeeds in strict mode when native layout handles the input" $ do
    let input = fixturePath projectRoot "LambdaCaseEdge.hs"
        output = FilePath.combine projectRoot "output/FallbackStrictNative.hs"
        config = strictFallbackConfig
          { _conf_layout = (_conf_layout strictFallbackConfig)
            { _lconfig_indentAmount = Identity $ Semigroup.Last 4
            }
          }
    expected <- readFile input
    Directory.copyFile input output
    (result, messages) <- runCore config False
      (Just output)
      (Just output)
    case result of
      Right Brittany.Changes -> pure ()
      Right Brittany.NoChanges ->
        Hspec.expectationFailure "expected native formatting output"
      Left exitCode -> Hspec.expectationFailure
        $ "native strict formatting failed with exit code " ++ show exitCode
    actual <- readFile output
    actual `Hspec.shouldNotBe` expected
    messages `Hspec.shouldNotSatisfy` any (List.isInfixOf "Fallback")

  Hspec.it "fails stdout mode for a scoped fallback with a precise notice" $ do
    (result, messages) <- runFormatterResult projectRoot strictFallbackConfig
      False
      "SpecialisePragmasEdge.hs"
      Nothing
    (result == Left 70) `Hspec.shouldBe` True
    messages `Hspec.shouldSatisfy` any (List.isInfixOf "SignatureFallback")
    messages `Hspec.shouldSatisfy` any (List.isInfixOf "declaration scope")
    messages `Hspec.shouldSatisfy` any (List.isInfixOf "RealSrcSpan")

  Hspec.it "fails for a whole-module fallback with its source span" $ do
    (result, messages) <- runFormatterResult projectRoot strictFallbackConfig
      True
      "ControlSyntaxEdge.hs"
      Nothing
    (result == Left 70) `Hspec.shouldBe` True
    messages `Hspec.shouldSatisfy` any (List.isInfixOf "WholeModuleFallback")
    messages `Hspec.shouldSatisfy` any (List.isInfixOf "module scope")
    messages `Hspec.shouldSatisfy` any (List.isInfixOf "RealSrcSpan")

  Hspec.it "returns fallback errors through the library API" $ do
    input <- Text.readFile $ fixturePath projectRoot "SpecialisePragmasEdge.hs"
    result <- parsePrintModule strictFallbackConfig input
    case result of
      Left errors -> do
        let notices = [notice | ExactSourceFallback notice <- errors]
        notices `Hspec.shouldSatisfy` any (List.isInfixOf "SignatureFallback")
        notices `Hspec.shouldSatisfy` any (List.isInfixOf "declaration scope")
        notices `Hspec.shouldSatisfy` any (List.isInfixOf "RealSrcSpan")
      Right _ -> Hspec.expectationFailure "expected a strict fallback error"

  Hspec.it "classifies exactprint-only output as a strict fallback" $ do
    let config = strictFallbackConfig
          { _conf_roundtrip_exactprint_only =
              Identity $ Semigroup.Last True
          }
    (result, messages) <- runFormatterResult projectRoot config
      True
      "DataDeclMultipleExpected.hs"
      Nothing
    (result == Left 70) `Hspec.shouldBe` True
    messages `Hspec.shouldSatisfy` any
      (List.isInfixOf "ExactPrintOnlyFallback")

  Hspec.it "propagates strict fallback errors from check mode" $ do
    Brittany.mainWith "brittany"
      [ "--config-file"
      , FilePath.combine projectRoot "data/brittany.yaml"
      , "--no-user-config"
      , "--fail-on-fallback"
      , "--check-mode"
      , fixturePath projectRoot "SpecialisePragmasEdge.hs"
      ] `Hspec.shouldThrow` (== Exit.ExitFailure 70)

  Hspec.it "does not overwrite an input file after a strict fallback" $ do
    let input = fixturePath projectRoot "SpecialisePragmasEdge.hs"
        output = FilePath.combine projectRoot "output/FallbackStrict.hs"
    expected <- readFile input
    Directory.copyFile input output
    Brittany.mainWith "brittany"
      [ "--config-file"
      , FilePath.combine projectRoot "data/brittany.yaml"
      , "--no-user-config"
      , "--fail-on-fallback"
      , "--write-mode"
      , "inplace"
      , output
      ] `Hspec.shouldThrow` (== Exit.ExitFailure 70)
    readFile output `Hspec.shouldReturn` expected

  Hspec.it "allows explicit output while retaining the strict exit code" $ do
    let input = fixturePath projectRoot "DoSyntaxEdge.hs"
        output = FilePath.combine projectRoot "output/FallbackStrictOutput.hs"
        config = strictFallbackConfig
          { _conf_errorHandling =
              (_conf_errorHandling strictFallbackConfig)
                { _econf_produceOutputOnErrors =
                    Identity $ Semigroup.Last True
                }
          }
    expected <- readFile input
    Directory.copyFile input output
    (result, _) <- runCore config False (Just output) (Just output)
    (result == Left 70) `Hspec.shouldBe` True
    actual <- readFile output
    actual `Hspec.shouldNotBe` expected

  Hspec.it "does not report a fallback for natively formatted data declarations" $ do
    messages <- runFormatter projectRoot fallbackReportingConfig
      "DataDeclMultipleExpected.hs"
    messages `Hspec.shouldNotSatisfy` any
      (List.isInfixOf "DataDeclarationFallback")

  Hspec.it "does not escalate lambda-case formatting to a whole-module fallback" $ do
    messages <- runFormatter projectRoot fallbackReportingConfig
      "LambdaCaseEdge.hs"
    messages `Hspec.shouldNotSatisfy` any
      (List.isInfixOf "WholeModuleFallback")

  Hspec.it "formats export section comments without fallback" $ do
    messages <- runFormatter projectRoot fallbackReportingConfig
      "ExportSectionCommentsExpected.hs"
    messages `Hspec.shouldNotSatisfy` any (List.isInfixOf "Fallback")

  Hspec.it "uses the same spacing policy for native and exact-source declarations" $ do
    messages <- runFormatter projectRoot fallbackReportingConfig
      "TopLevelSpacingEdge.hs"
    messages `Hspec.shouldSatisfy` any
      (List.isInfixOf "SignatureFallback")
    messages `Hspec.shouldNotSatisfy` any
      (List.isInfixOf "WholeModuleFallback")

  Hspec.it "preserves parse-failure behavior when reporting is enabled" $ do
    let input = fixturePath projectRoot "DataDeclMultipleInvalid.hs"
        output = FilePath.combine projectRoot "output/FallbackInvalid.hs"
    Directory.copyFile input output
    messagesRef <- IORef.newIORef []
    result <- Brittany.coreIO
      (appendMessage messagesRef)
      fallbackReportingConfig
      False
      False
      (Just output)
      (Just output)
    case result of
      Left 60 -> pure ()
      _ -> Hspec.expectationFailure "expected formatter parse failure 60"
    expected <- readFile input
    readFile output `Hspec.shouldReturn` expected

  Hspec.it "keeps parse failure 60 and input preservation in strict mode" $ do
    let input = fixturePath projectRoot "DataDeclMultipleInvalid.hs"
        output = FilePath.combine projectRoot "output/FallbackStrictInvalid.hs"
    Directory.copyFile input output
    (result, _) <- runCore strictFallbackConfig True
      (Just output)
      (Just output)
    (result == Left 60) `Hspec.shouldBe` True
    expected <- readFile input
    readFile output `Hspec.shouldReturn` expected

fallbackReportingConfig :: Config
fallbackReportingConfig = staticDefaultConfig
  { _conf_debug = (_conf_debug staticDefaultConfig)
    { _dconf_dump_fallbacks = Identity $ Semigroup.Last True
    }
  , _conf_errorHandling = (_conf_errorHandling staticDefaultConfig)
    { _econf_Werror = Identity $ Semigroup.Last True
    }
  }

strictFallbackConfig :: Config
strictFallbackConfig = staticDefaultConfig
  { _conf_errorHandling = (_conf_errorHandling staticDefaultConfig)
    { _econf_failOnExactSourceFallback = Identity $ Semigroup.Last True
    }
  }

runFormatter :: FilePath -> Config -> FilePath -> IO [String]
runFormatter projectRoot config fixtureName = do
  (result, messages) <- runFormatterResult projectRoot config
    True
    fixtureName
    Nothing
  case result of
    Left exitCode ->
      Hspec.expectationFailure $ "formatter failed with exit code " ++ show exitCode
    Right _ -> pure ()
  pure messages

runFormatterResult
  :: FilePath
  -> Config
  -> Bool
  -> FilePath
  -> Maybe FilePath
  -> IO (Either Int Brittany.ChangeStatus, [String])
runFormatterResult projectRoot config suppressOutput fixtureName outputPath =
  runCore config suppressOutput
    (Just $ fixturePath projectRoot fixtureName)
    outputPath

runCore
  :: Config
  -> Bool
  -> Maybe FilePath
  -> Maybe FilePath
  -> IO (Either Int Brittany.ChangeStatus, [String])
runCore config suppressOutput inputPath outputPath = do
  messagesRef <- IORef.newIORef []
  result <- Brittany.coreIO
    (appendMessage messagesRef)
    config
    suppressOutput
    False
    inputPath
    outputPath
  messages <- IORef.readIORef messagesRef
  pure (result, messages)

appendMessage :: IORef.IORef [String] -> String -> IO ()
appendMessage messagesRef message =
  IORef.modifyIORef' messagesRef (++ [message])

fixturePath :: FilePath -> FilePath -> FilePath
fixturePath projectRoot fixtureName = FilePath.combine
  (FilePath.combine projectRoot "source/test-suite/fixtures")
  fixtureName
