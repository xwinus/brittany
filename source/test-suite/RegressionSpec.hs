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

  Hspec.describe "parenthesized block expressions" $ do
    strictIdempotentFormattingExample projectRoot
      "aligns multiline lambda-case delimiters"
      "ParenthesizedLambdaCaseExpected.hs"
    strictIdempotentFormattingExample projectRoot
      "preserves nested, commented, and control-expression blocks"
      "ParenthesizedLambdaCaseEdge.hs"
    parseFailureExample projectRoot
      "rejects malformed parenthesized lambda-case syntax"
      "ParenthesizedLambdaCaseInvalid.hs"

  Hspec.describe "multiline constructor patterns" $ do
    strictColumnsIdempotentFormattingExample projectRoot
      "wraps a long case-alternative pattern at 80 columns"
      "MultilineConstructorPatternExpected.hs"
    columnsIdempotentFormattingExample projectRoot
      "preserves nested patterns, comments, guards, adornments, and bindings"
      "MultilineConstructorPatternEdge.hs"
    parseFailureExample projectRoot
      "rejects a malformed multiline constructor pattern"
      "MultilineConstructorPatternInvalid.hs"

  Hspec.describe "final result Haddock comments" $ do
    idempotentFormattingExample projectRoot
      "keeps a final result comment aligned with its signature"
      "FinalResultHaddockExpected.hs"
    idempotentFormattingExample projectRoot
      "preserves post-doc forms without taking the next declaration's comment"
      "FinalResultHaddockEdge.hs"
    parseFailureExample projectRoot
      "rejects malformed result type syntax without changing the input"
      "FinalResultHaddockInvalid.hs"

  Hspec.describe "scoped expression fallbacks" $ do
    idempotentFormattingFromInputExample projectRoot
      "uses native indentation around a quasiquote fallback"
      "ScopedExpressionFallbackInput.hs"
      "ScopedExpressionFallbackExpected.hs"
    idempotentFormattingFromInputExample projectRoot
      "preserves nested fragment comments and raw content"
      "ScopedExpressionFallbackEdgeInput.hs"
      "ScopedExpressionFallbackEdge.hs"
    parseFailureExample projectRoot
      "rejects an unterminated quasiquote without changing the input"
      "ScopedExpressionFallbackInvalid.hs"

  Hspec.describe "module export-list comments" $ do
    idempotentFormattingExample projectRoot
      "keeps Haddock section headings before their exports"
      "ExportSectionCommentsExpected.hs"
    idempotentFormattingExample projectRoot
      "preserves mixed comments and export forms in source order"
      "ExportSectionCommentsEdge.hs"
    parseFailureExample projectRoot
      "rejects a malformed commented export list without changing the input"
      "ExportSectionCommentsInvalid.hs"

  Hspec.describe "top-level spacing" $ do
    idempotentFormattingExample projectRoot
      "preserves section gaps around imports and declaration groups"
      "TopLevelSpacingExpected.hs"
    idempotentFormattingExample projectRoot
      "preserves mixed gaps, comments, splices, and exact-source declarations"
      "TopLevelSpacingEdge.hs"
    parseFailureExample projectRoot
      "rejects malformed spaced syntax without changing the input"
      "TopLevelSpacingInvalid.hs"

  Hspec.describe "statement spacing" $ do
    idempotentFormattingExample projectRoot
      "preserves a blank line between Hspec-style statements"
      "StatementSpacingExpected.hs"
    idempotentFormattingExample projectRoot
      "preserves mixed nested gaps and comment ownership"
      "StatementSpacingEdge.hs"
    parseFailureExample projectRoot
      "rejects malformed spaced do syntax without changing the input"
      "StatementSpacingInvalid.hs"

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

idempotentFormattingFromInputExample
  :: FilePath -> String -> FilePath -> FilePath -> Hspec.SpecWith ()
idempotentFormattingFromInputExample
  projectRoot description inputName expectedName = Hspec.it description $ do
    let input = fixturePath projectRoot inputName
        expectedFixture = fixturePath projectRoot expectedName
        output = outputPath projectRoot inputName
    expected <- readFile expectedFixture
    Directory.copyFile input output
    Brittany.mainWith "brittany" (formatterArgs projectRoot output)
    firstPass <- readFile output
    firstPass `Hspec.shouldBe` expected
    Brittany.mainWith "brittany" (formatterArgs projectRoot output)
    secondPass <- readFile output
    secondPass `Hspec.shouldBe` firstPass

strictIdempotentFormattingExample
  :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
strictIdempotentFormattingExample projectRoot description fixtureName =
  Hspec.it description $ do
    let fixture = fixturePath projectRoot fixtureName
        output = outputPath projectRoot fixtureName
    expected <- readFile fixture
    Directory.copyFile fixture output
    Brittany.mainWith "brittany"
      ("--fail-on-fallback" : formatterArgs projectRoot output)
    firstPass <- readFile output
    firstPass `Hspec.shouldBe` expected
    Brittany.mainWith "brittany"
      ("--fail-on-fallback" : formatterArgs projectRoot output)
    secondPass <- readFile output
    secondPass `Hspec.shouldBe` firstPass

strictColumnsIdempotentFormattingExample
  :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
strictColumnsIdempotentFormattingExample projectRoot description fixtureName =
  columnsIdempotentFormattingExampleWith
    ["--fail-on-fallback"] projectRoot description fixtureName

columnsIdempotentFormattingExample
  :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
columnsIdempotentFormattingExample = columnsIdempotentFormattingExampleWith []

columnsIdempotentFormattingExampleWith
  :: [String] -> FilePath -> String -> FilePath -> Hspec.SpecWith ()
columnsIdempotentFormattingExampleWith extraArgs projectRoot description fixtureName =
  Hspec.it description $ do
    let fixture = fixturePath projectRoot fixtureName
        output = outputPath projectRoot fixtureName
        args =
          "--columns" : "80" : extraArgs ++ formatterArgs projectRoot output
    expected <- readFile fixture
    Directory.copyFile fixture output
    Brittany.mainWith "brittany" args
    firstPass <- readFile output
    firstPass `Hspec.shouldBe` expected
    filter ((> 80) . length) (lines firstPass) `Hspec.shouldBe` []
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
