module RegressionSpec (spec) where

import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "GHC 9.14 regressions" $ do
  Hspec.describe "DerivingVia" $ do
    formattingExample projectRoot
      "formats a basic clause without crashing"
      "DerivingViaExpected.hs"
    formattingExample projectRoot
      "formats a multi-class parameterized clause"
      "DerivingViaEdge.hs"
    parseFailureExample projectRoot
      "rejects malformed syntax"
      "DerivingViaInvalid.hs"

  Hspec.describe "multiple-constructor data declarations" $ do
    formattingExample projectRoot
      "preserves all constructors"
      "DataDeclMultipleExpected.hs"
    formattingExample projectRoot
      "preserves mixed constructors and deriving clauses"
      "DataDeclMultipleEdge.hs"
    parseFailureExample projectRoot
      "rejects a malformed constructor"
      "DataDeclMultipleInvalid.hs"

  Hspec.describe "comment preservation" $ do
    formattingExample projectRoot
      "preserves a top-level comment"
      "CommentPreservationExpected.hs"
    formattingExample projectRoot
      "preserves a trailing expression comment"
      "CommentPreservationEdge.hs"
    Hspec.it "rejects output that would lose a class method comment" $ do
      let fixture = fixturePath projectRoot "CommentPreservationLost.hs"
          output = outputPath projectRoot "CommentPreservationLost.hs"
      expected <- readFile fixture
      Directory.copyFile fixture output
      Brittany.mainWith "brittany" (formatterArgs projectRoot output)
        `Hspec.shouldThrow` isFormattingFailure
      actual <- readFile output
      actual `Hspec.shouldBe` expected

  Hspec.describe "type declaration comments" $ do
    idempotentFormattingExample projectRoot
      "preserves comments in a type signature"
      "TypeCommentsExpected.hs"
    idempotentFormattingExample projectRoot
      "preserves comments in a tuple type synonym"
      "TypeCommentsEdge.hs"
    parseFailureExample projectRoot
      "rejects a malformed commented type synonym"
      "TypeCommentsInvalid.hs"

  Hspec.describe "import sub-list comments" $ do
    idempotentFormattingExample projectRoot
      "preserves a comment beside an imported name"
      "ImportCommentsExpected.hs"
    idempotentFormattingExample projectRoot
      "preserves comments around names, operators, and punctuation"
      "ImportCommentsEdge.hs"
    parseFailureExample projectRoot
      "rejects a malformed commented import list"
      "ImportCommentsInvalid.hs"

  Hspec.describe "expression comments" $ do
    idempotentFormattingExample projectRoot
      "preserves comments around a multi-way if guard"
      "ExpressionCommentsExpected.hs"
    idempotentFormattingExample projectRoot
      "preserves comments in let bindings and operator chains"
      "ExpressionCommentsEdge.hs"
    parseFailureExample projectRoot
      "rejects a malformed commented multi-way if"
      "ExpressionCommentsInvalid.hs"

  Hspec.describe "data declaration comments" $ do
    idempotentFormattingExample projectRoot
      "preserves comments between record fields"
      "DeclarationCommentsExpected.hs"
    idempotentFormattingExample projectRoot
      "preserves deriving-via and commented-out fields"
      "DeclarationCommentsEdge.hs"
    parseFailureExample projectRoot
      "rejects a malformed commented record declaration"
      "DeclarationCommentsInvalid.hs"

formattingExample :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
formattingExample projectRoot description fixtureName =
  Hspec.it description $ do
    let fixture = fixturePath projectRoot fixtureName
        output = outputPath projectRoot fixtureName
    expected <- readFile fixture
    Directory.copyFile fixture output
    Brittany.mainWith "brittany" (formatterArgs projectRoot output)
    actual <- readFile output
    actual `Hspec.shouldBe` expected

idempotentFormattingExample :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
idempotentFormattingExample projectRoot description fixtureName =
  Hspec.it description $ do
    let fixture = fixturePath projectRoot fixtureName
        output = outputPath projectRoot fixtureName
    expected <- readFile fixture
    Directory.copyFile fixture output
    Brittany.mainWith "brittany" (formatterArgs projectRoot output)
    firstPass <- readFile output
    firstPass `Hspec.shouldBe` expected
    Brittany.mainWith "brittany" (formatterArgs projectRoot output)
    secondPass <- readFile output
    secondPass `Hspec.shouldBe` firstPass

parseFailureExample :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
parseFailureExample projectRoot description fixtureName =
  Hspec.it description $ do
    let fixture = fixturePath projectRoot fixtureName
    Brittany.mainWith "brittany" (formatterArgs projectRoot fixture)
      `Hspec.shouldThrow` isParseFailure

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

isParseFailure :: Exit.ExitCode -> Bool
isParseFailure exitCode = exitCode == Exit.ExitFailure 60

isFormattingFailure :: Exit.ExitCode -> Bool
isFormattingFailure exitCode = exitCode == Exit.ExitFailure 70
