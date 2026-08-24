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
    idempotentFormattingExample projectRoot
      "preserves a class method comment"
      "CommentPreservationLost.hs"
    idempotentFormattingExample projectRoot
      "preserves a type family equation comment"
      "CommentPreservationFamilyLost.hs"
    idempotentFormattingExample projectRoot
      "preserves a foreign declaration comment"
      "CommentPreservationForeignLost.hs"

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

  Hspec.describe "class and instance declaration comments" $ do
    idempotentFormattingExample projectRoot
      "preserves comments before class and instance methods"
      "ClassInstanceCommentsExpected.hs"
    idempotentFormattingExample projectRoot
      "preserves comments around associated types and default methods"
      "ClassInstanceCommentsEdge.hs"
    parseFailureExample projectRoot
      "rejects a malformed commented class method signature"
      "ClassInstanceCommentsInvalid.hs"

  Hspec.describe "family declaration comments" $ do
    idempotentFormattingExample projectRoot
      "preserves comments in open families and standalone instances"
      "FamilyCommentsExpected.hs"
    idempotentFormattingExample projectRoot
      "preserves comments around injectivity and GADT data instances"
      "FamilyCommentsEdge.hs"
    parseFailureExample projectRoot
      "rejects a malformed commented type family equation"
      "FamilyCommentsInvalid.hs"

  Hspec.describe "unit constructor patterns" $ do
    idempotentFormattingExample projectRoot
      "keeps a unit constructor pattern stable"
      "UnitPatternExpected.hs"
    idempotentFormattingExample projectRoot
      "keeps nested and explicitly parenthesized unit patterns stable"
      "UnitPatternEdge.hs"
    parseFailureExample projectRoot
      "rejects malformed unit pattern syntax without changing the input"
      "UnitPatternInvalid.hs"

  Hspec.describe "tuple constructors in expressions" $ do
    idempotentFormattingExample projectRoot
      "keeps a boxed pair constructor stable"
      "TupleConstructorExpected.hs"
    idempotentFormattingExample projectRoot
      "keeps tuple arities and symbolic controls stable"
      "TupleConstructorEdge.hs"
    parseFailureExample projectRoot
      "rejects malformed tuple constructor syntax without changing the input"
      "TupleConstructorInvalid.hs"

  Hspec.describe "lambda-case expressions" $ do
    idempotentFormattingExample projectRoot
      "keeps lambda-case distinct from an ordinary lambda"
      "LambdaCaseExpected.hs"
    idempotentFormattingExample projectRoot
      "formats empty, guarded, commented, and nested lambda-case expressions"
      "LambdaCaseEdge.hs"
    parseFailureExample projectRoot
      "rejects malformed lambda-case syntax without changing the input"
      "LambdaCaseInvalid.hs"

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
        output = outputPath projectRoot fixtureName
    expected <- readFile fixture
    Directory.copyFile fixture output
    Brittany.mainWith "brittany" (formatterArgs projectRoot output)
      `Hspec.shouldThrow` isParseFailure
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

isParseFailure :: Exit.ExitCode -> Bool
isParseFailure exitCode = exitCode == Exit.ExitFailure 60
