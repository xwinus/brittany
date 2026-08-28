module PatternSynonymBindingSpec (spec) where

import qualified Data.Functor.Identity                   as Identity
import qualified Data.IORef                              as IORef
import qualified Data.List                               as List
import qualified Data.Semigroup                          as Semigroup
import           Language.Haskell.Brittany
import qualified Language.Haskell.Brittany.Internal.ParseModule
                                                         as ParseModule
import qualified Language.Haskell.Brittany.Main          as Brittany
import qualified System.Directory                        as Directory
import qualified System.Exit                             as Exit
import qualified System.FilePath                         as FilePath
import qualified Test.Hspec                              as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "pattern synonym bindings" $ do
  formattingExampleAt
    80
    projectRoot
    "formats the LineMode pattern synonyms natively"
    "PatternSynonymBindingInput.hs"
    "PatternSynonymBindingExpected.hs"
  formattingExampleAt
    50
    projectRoot
    "preserves every direction, argument form, and comment position"
    "PatternSynonymBindingEdgeInput.hs"
    "PatternSynonymBindingEdgeExpected.hs"
  scopedFallbackExample
    projectRoot
    "keeps unsupported pattern syntax at inline scope"
    "PatternSynonymBindingUnsupported.hs"
  parseFailureExample
    projectRoot
    "rejects an incomplete pattern synonym without changing input"
    "PatternSynonymBindingInvalid.hs"

formattingExampleAt
  :: Int -> FilePath -> String -> FilePath -> FilePath -> Hspec.SpecWith ()
formattingExampleAt columns projectRoot description inputName expectedName =
  Hspec.it description $ do
    let input           = fixturePath projectRoot inputName
        expectedFixture = fixturePath projectRoot expectedName
        output          = outputPath projectRoot inputName
        args =
          ["--columns", show columns, "--fail-on-fallback"]
            ++ formatterArgs projectRoot output
    expected <- readFile expectedFixture
    Directory.copyFile input output
    Brittany.mainWith "brittany" args
    firstPass <- readFile output
    firstPass `Hspec.shouldBe` expected
    assertParses output firstPass
    Brittany.mainWith "brittany" args
    readFile output `Hspec.shouldReturn` firstPass

scopedFallbackExample
  :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
scopedFallbackExample projectRoot description fixtureName =
  Hspec.it description $ do
    messagesRef <- IORef.newIORef []
    result <- Brittany.coreIO
      (\message -> IORef.modifyIORef' messagesRef (++ [message]))
      strictFallbackConfig
      True
      False
      (Just $ fixturePath projectRoot fixtureName)
      Nothing
    messages <- IORef.readIORef messagesRef
    (result == Left 70) `Hspec.shouldBe` True
    length (filter (List.isInfixOf "PatternFallback") messages)
      `Hspec.shouldBe` 1
    messages `Hspec.shouldSatisfy` any (List.isInfixOf "inline scope")
    messages `Hspec.shouldNotSatisfy` any
      (List.isInfixOf "DeclarationFallback")
    messages `Hspec.shouldNotSatisfy` any
      (List.isInfixOf "WholeModuleFallback")

parseFailureExample :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
parseFailureExample projectRoot description fixtureName =
  Hspec.it description $ do
    let fixture = fixturePath projectRoot fixtureName
        output  = outputPath projectRoot fixtureName
    expected <- readFile fixture
    Directory.copyFile fixture output
    Brittany.mainWith "brittany" (formatterArgs projectRoot output)
      `Hspec.shouldThrow` (== Exit.ExitFailure 60)
    readFile output `Hspec.shouldReturn` expected

assertParses :: FilePath -> String -> IO ()
assertParses output source = do
  parsed <- ParseModule.parseModule [] output (const $ pure $ Right ()) source
  case parsed of
    Left  parseError -> Hspec.expectationFailure parseError
    Right _          -> pure ()

strictFallbackConfig :: Config
strictFallbackConfig = staticDefaultConfig
  { _conf_errorHandling = (_conf_errorHandling staticDefaultConfig)
    { _econf_failOnExactSourceFallback =
        Identity.Identity $ Semigroup.Last True
    }
  }

fixturePath :: FilePath -> FilePath -> FilePath
fixturePath projectRoot =
  FilePath.combine $ FilePath.combine projectRoot "source/test-suite/fixtures"

outputPath :: FilePath -> FilePath -> FilePath
outputPath projectRoot =
  FilePath.combine $ FilePath.combine projectRoot "output"

formatterArgs :: FilePath -> FilePath -> [String]
formatterArgs projectRoot input =
  [ "--config-file"
  , FilePath.combine projectRoot "data/brittany.yaml"
  , "--no-user-config"
  , "--write-mode"
  , "inplace"
  , input
  ]
