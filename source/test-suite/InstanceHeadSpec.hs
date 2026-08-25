module InstanceHeadSpec (spec) where

import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "instance head layout" $ do
  columnsIdempotentFormattingExample projectRoot
    "wraps the Headroom instance head at 80 columns"
    80
    "InstanceHeadInput.hs"
    "InstanceHeadExpected.hs"
    True
  columnsIdempotentFormattingExample projectRoot
    "keeps the same instance head within a narrow width"
    40
    "InstanceHeadInput.hs"
    "InstanceHeadNarrowExpected.hs"
    True
  columnsIdempotentFormattingExample projectRoot
    "preserves binders, overlap pragmas, comments, and associated members"
    80
    "InstanceHeadEdgeInput.hs"
    "InstanceHeadEdge.hs"
    False
  parseFailureExample projectRoot
    "rejects a malformed instance head without changing the input"
    "InstanceHeadInvalid.hs"

columnsIdempotentFormattingExample
  :: FilePath
  -> String
  -> Int
  -> FilePath
  -> FilePath
  -> Bool
  -> Hspec.SpecWith ()
columnsIdempotentFormattingExample
  projectRoot description columns inputName expectedName strict =
    Hspec.it description $ do
      let input = fixturePath projectRoot inputName
          expectedFixture = fixturePath projectRoot expectedName
          output = outputPath projectRoot inputName
          strictArgs = ["--fail-on-fallback" | strict]
          args = "--columns" : show columns
            : strictArgs ++ formatterArgs projectRoot output
      expected <- readFile expectedFixture
      Directory.copyFile input output
      Brittany.mainWith "brittany" args
      firstPass <- readFile output
      firstPass `Hspec.shouldBe` expected
      filter ((> columns) . length) (lines firstPass) `Hspec.shouldBe` []
      Brittany.mainWith "brittany" args
      secondPass <- readFile output
      secondPass `Hspec.shouldBe` firstPass

parseFailureExample :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
parseFailureExample projectRoot description fixtureName =
  Hspec.it description $ do
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
outputPath projectRoot = FilePath.combine $ FilePath.combine projectRoot "output"

formatterArgs :: FilePath -> FilePath -> [String]
formatterArgs projectRoot input =
  [ "--config-file"
  , FilePath.combine projectRoot "data/brittany.yaml"
  , "--no-user-config"
  , "--write-mode"
  , "inplace"
  , input
  ]
