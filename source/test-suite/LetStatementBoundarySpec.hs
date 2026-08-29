module LetStatementBoundarySpec (spec) where

import qualified Data.List as List
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "statements after let bindings" $ do
  formattingExampleAt
    80
    2
    projectRoot
    "keeps a parenthesized do expression outside a preceding let"
    "LetStatementBoundaryInput.hs"
    "LetStatementBoundaryExpected.hs"
  formattingExampleAt
    46
    2
    projectRoot
    "preserves multi-binding lets and nested block operators at narrow width"
    "LetStatementBoundaryEdgeInput.hs"
    "LetStatementBoundaryEdgeExpected.hs"
  formattingExampleAt
    54
    4
    projectRoot
    "uses alternate indentation without merging statement boundaries"
    "LetStatementBoundaryEdgeInput.hs"
    "LetStatementBoundaryIndent4Expected.hs"
  formattingExampleAt
    80
    2
    projectRoot
    "preserves comments around the let and nested do statements"
    "LetStatementBoundaryComments.hs"
    "LetStatementBoundaryCommentsExpected.hs"
  productionModuleExample projectRoot
  parseFailureExample projectRoot

formattingExampleAt
  :: Int
  -> Int
  -> FilePath
  -> String
  -> FilePath
  -> FilePath
  -> Hspec.SpecWith ()
formattingExampleAt columns indent projectRoot description inputName expectedName =
  Hspec.it description $ do
    let input = fixturePath projectRoot inputName
        expectedFixture = fixturePath projectRoot expectedName
        output = outputPath projectRoot inputName
        args = formatterArgs projectRoot columns indent output
    expected <- readFile expectedFixture
    Directory.copyFile input output
    Brittany.mainWith "brittany" args
    firstPass <- readFile output
    firstPass `Hspec.shouldBe` expected
    assertParses output firstPass
    Brittany.mainWith "brittany" args
    readFile output `Hspec.shouldReturn` firstPass

productionModuleExample :: FilePath -> Hspec.SpecWith ()
productionModuleExample projectRoot = Hspec.it
  "formats both production cleanup statements without rejection"
  $ do
      let input = FilePath.combine
            projectRoot
            "source/library/Language/Haskell/Brittany/Internal/TransactionalWrite.hs"
          output = outputPath projectRoot "TransactionalWriteIssue121.hs"
          args = formatterArgs projectRoot 80 2 output
      Directory.copyFile input output
      Brittany.mainWith "brittany" args
      firstPass <- readFile output
      firstPass `Hspec.shouldSatisfy` List.isInfixOf
        "   ) `Exception.onException` cleanupReplacement"
      firstPass `Hspec.shouldSatisfy` List.isInfixOf
        "   ) `Exception.onException` cleanupBoth"
      assertParses output firstPass
      Brittany.mainWith "brittany" args
      readFile output `Hspec.shouldReturn` firstPass

parseFailureExample :: FilePath -> Hspec.SpecWith ()
parseFailureExample projectRoot = Hspec.it
  "leaves a malformed parenthesized operator statement byte-identical"
  $ do
      let fixture = fixturePath projectRoot "LetStatementBoundaryInvalid.hs"
          output = outputPath projectRoot "LetStatementBoundaryInvalid.hs"
      expected <- readFile fixture
      Directory.copyFile fixture output
      Brittany.mainWith "brittany" (formatterArgs projectRoot 80 2 output)
        `Hspec.shouldThrow` (== Exit.ExitFailure 60)
      readFile output `Hspec.shouldReturn` expected

assertParses :: FilePath -> String -> IO ()
assertParses output source = do
  parsed <- ParseModule.parseModule [] output (const $ pure $ Right ()) source
  case parsed of
    Left parseError -> Hspec.expectationFailure parseError
    Right _ -> pure ()

fixturePath :: FilePath -> FilePath -> FilePath
fixturePath projectRoot =
  FilePath.combine $ FilePath.combine projectRoot "source/test-suite/fixtures"

outputPath :: FilePath -> FilePath -> FilePath
outputPath projectRoot = FilePath.combine $ FilePath.combine projectRoot "output"

formatterArgs :: FilePath -> Int -> Int -> FilePath -> [String]
formatterArgs projectRoot columns indent input =
  [ "--config-file"
  , FilePath.combine projectRoot "data/brittany.yaml"
  , "--no-user-config"
  , "--columns"
  , show columns
  , "--indent"
  , show indent
  , "--fail-on-fallback"
  , "--write-mode"
  , "inplace"
  , input
  ]
