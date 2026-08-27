module CompactParenthesizedPatternSpec (spec) where

import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "compact parenthesized constructor patterns" $ do
  formattingExample
    80
    ["--fail-on-fallback"]
    projectRoot
    "keeps fitting patterns compact in every binding context"
    "CompactParenthesizedPatternInput.hs"
    "CompactParenthesizedPatternExpected.hs"
  formattingExample
    50
    []
    projectRoot
    "wraps long patterns and preserves nested syntax and comments"
    "CompactParenthesizedPatternEdgeInput.hs"
    "CompactParenthesizedPatternEdgeExpected.hs"
  parseFailureExample
    projectRoot
    "rejects malformed parenthesized constructor syntax without changing input"
    "CompactParenthesizedPatternInvalid.hs"

formattingExample
  :: Int
  -> [String]
  -> FilePath
  -> String
  -> FilePath
  -> FilePath
  -> Hspec.SpecWith ()
formattingExample
  columns extraArgs projectRoot description inputName expectedName =
    Hspec.it description $ do
      let input = fixturePath projectRoot inputName
          expectedFixture = fixturePath projectRoot expectedName
          output = outputPath projectRoot inputName
          args =
            "--columns" : show columns : extraArgs ++ formatterArgs projectRoot output
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
parseFailureExample projectRoot description fixtureName = Hspec.it description $ do
  let fixture = fixturePath projectRoot fixtureName
      output = outputPath projectRoot fixtureName
  expected <- readFile fixture
  Directory.copyFile fixture output
  Brittany.mainWith "brittany" (formatterArgs projectRoot output)
    `Hspec.shouldThrow` (== Exit.ExitFailure 60)
  actual <- readFile output
  actual `Hspec.shouldBe` expected

fixturePath :: FilePath -> FilePath -> FilePath
fixturePath projectRoot fixtureName =
  FilePath.combine
    (FilePath.combine projectRoot "source/test-suite/fixtures")
    fixtureName

outputPath :: FilePath -> FilePath -> FilePath
outputPath projectRoot fixtureName =
  FilePath.combine (FilePath.combine projectRoot "output") fixtureName

formatterArgs :: FilePath -> FilePath -> [String]
formatterArgs projectRoot input =
  [ "--config-file"
  , FilePath.combine projectRoot "data/brittany.yaml"
  , "--no-user-config"
  , "--write-mode"
  , "inplace"
  , input
  ]
