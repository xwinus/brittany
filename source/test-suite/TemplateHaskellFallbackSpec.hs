module TemplateHaskellFallbackSpec (spec) where

import qualified Language.Haskell.Brittany.Main          as Brittany
import qualified System.Directory                        as Directory
import qualified System.Exit                             as Exit
import qualified System.FilePath                         as FilePath
import qualified Test.Hspec                              as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "scoped Template Haskell fallbacks" $ do
  idempotentFormattingFromInputExample
    projectRoot
    "formats a binding around an untyped expression quote"
    "TemplateHaskellFallbackInput.hs"
    "TemplateHaskellFallbackExpected.hs"
  idempotentFormattingFromInputExample
    projectRoot
    "rebases brackets and splices in nested expression contexts"
    "TemplateHaskellFallbackEdgeInput.hs"
    "TemplateHaskellFallbackEdge.hs"
  parseFailureExample
    projectRoot
    "rejects an unterminated quote without changing the input"
    "TemplateHaskellFallbackInvalid.hs"

idempotentFormattingFromInputExample
  :: FilePath -> String -> FilePath -> FilePath -> Hspec.SpecWith ()
idempotentFormattingFromInputExample projectRoot description inputName expectedName
  = Hspec.it description $ do
    let input = fixturePath projectRoot inputName
        expectedFixture = fixturePath projectRoot expectedName
        output = outputPath projectRoot inputName
    expected <- readFile expectedFixture
    Directory.copyFile input output
    Brittany.mainWith "brittany" $ formatterArgs projectRoot output
    firstPass <- readFile output
    firstPass `Hspec.shouldBe` expected
    Brittany.mainWith "brittany" $ formatterArgs projectRoot output
    secondPass <- readFile output
    secondPass `Hspec.shouldBe` firstPass

parseFailureExample :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
parseFailureExample projectRoot description fixtureName = Hspec.it description $ do
  let fixture = fixturePath projectRoot fixtureName
      output = outputPath projectRoot fixtureName
  expected <- readFile fixture
  Directory.copyFile fixture output
  Brittany.mainWith "brittany" (formatterArgs projectRoot output)
    `Hspec.shouldThrow` (== Exit.ExitFailure 60)
  readFile output `Hspec.shouldReturn` expected

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
