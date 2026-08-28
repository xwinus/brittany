module StandaloneKindSignatureSpec (spec) where

import qualified Language.Haskell.Brittany.Internal.ParseModule
                                                         as ParseModule
import qualified Language.Haskell.Brittany.Main          as Brittany
import qualified System.Directory                        as Directory
import qualified System.Exit                             as Exit
import qualified System.FilePath                         as FilePath
import qualified Test.Hspec                              as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "standalone kind signatures" $ do
  formattingExampleAt
    80
    projectRoot
    "formats prefix, operator, invisible, and visible signatures natively"
    "StandaloneKindSignatureInput.hs"
    "StandaloneKindSignatureExpected.hs"
  formattingExampleAt
    40
    projectRoot
    "wraps promoted and qualified kinds while preserving documentation"
    "StandaloneKindSignatureEdgeInput.hs"
    "StandaloneKindSignatureEdgeExpected.hs"
  parseFailureExample
    projectRoot
    "rejects an incomplete kind signature without changing input"
    "StandaloneKindSignatureInvalid.hs"

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
