module FallbackSpec (spec) where

import qualified CompatibilityMatrix as Matrix
import qualified Control.Monad as Monad
import Data.Functor.Identity (Identity(..))
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Semigroup as Semigroup
import Language.Haskell.Brittany
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
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

fallbackReportingConfig :: Config
fallbackReportingConfig = staticDefaultConfig
  { _conf_debug = (_conf_debug staticDefaultConfig)
    { _dconf_dump_fallbacks = Identity $ Semigroup.Last True
    }
  , _conf_errorHandling = (_conf_errorHandling staticDefaultConfig)
    { _econf_Werror = Identity $ Semigroup.Last True
    }
  }

runFormatter :: FilePath -> Config -> FilePath -> IO [String]
runFormatter projectRoot config fixtureName = do
  messagesRef <- IORef.newIORef []
  result <- Brittany.coreIO
    (appendMessage messagesRef)
    config
    True
    False
    (Just $ fixturePath projectRoot fixtureName)
    Nothing
  case result of
    Left exitCode ->
      Hspec.expectationFailure $ "formatter failed with exit code " ++ show exitCode
    Right _ -> pure ()
  IORef.readIORef messagesRef

appendMessage :: IORef.IORef [String] -> String -> IO ()
appendMessage messagesRef message =
  IORef.modifyIORef' messagesRef (++ [message])

fixturePath :: FilePath -> FilePath -> FilePath
fixturePath projectRoot fixtureName = FilePath.combine
  (FilePath.combine projectRoot "source/test-suite/fixtures")
  fixtureName
