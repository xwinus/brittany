module ConstructorFieldModifierSpec (spec) where

import qualified Data.List as List
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.Process as Process
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "constructor-field modifiers" $ do
  formattingExampleAt 2 projectRoot
    "preserves lazy strict unpack and nounpack record fields"
    "ConstructorFieldModifiersInput.hs"
    "ConstructorFieldModifiersExpected.hs"
  formattingExampleAt 2 projectRoot
    "preserves modifiers across constructor forms and comments"
    "ConstructorFieldModifiersEdgeInput.hs"
    "ConstructorFieldModifiersEdgeExpected.hs"
  formattingExampleAt 4 projectRoot
    "uses configured indentation without losing modifiers"
    "ConstructorFieldModifiersEdgeInput.hs"
    "ConstructorFieldModifiersEdgeIndent4Expected.hs"
  parseFailureExample projectRoot
    "rejects a malformed modifier without changing input"
    "ConstructorFieldModifiersInvalid.hs"
  semanticExample projectRoot
    "keeps an explicitly lazy field lazy under StrictData"
    "ConstructorFieldModifiersSemantic.hs"

formattingExampleAt
  :: Int -> FilePath -> String -> FilePath -> FilePath -> Hspec.SpecWith ()
formattingExampleAt indent projectRoot description inputName expectedName =
  Hspec.it description $ do
    let input = fixturePath projectRoot inputName
        expectedFixture = fixturePath projectRoot expectedName
        output = outputPath projectRoot inputName
        args = formatterArgs projectRoot indent output
    expected <- readFile expectedFixture
    Directory.copyFile input output
    Brittany.mainWith "brittany" args
    firstPass <- readFile output
    firstPass `Hspec.shouldBe` expected
    filter ((> 80) . length) (lines firstPass) `Hspec.shouldBe` []
    assertParses output firstPass
    Brittany.mainWith "brittany" args
    readFile output `Hspec.shouldReturn` firstPass

parseFailureExample :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
parseFailureExample projectRoot description fixtureName = Hspec.it description $ do
  let fixture = fixturePath projectRoot fixtureName
      output = outputPath projectRoot fixtureName
  expected <- readFile fixture
  Directory.copyFile fixture output
  Brittany.mainWith "brittany" (formatterArgs projectRoot 2 output)
    `Hspec.shouldThrow` (== Exit.ExitFailure 60)
  readFile output `Hspec.shouldReturn` expected

semanticExample :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
semanticExample projectRoot description fixtureName = Hspec.it description $ do
  let fixture = fixturePath projectRoot fixtureName
      output = outputPath projectRoot fixtureName
  Directory.copyFile fixture output
  Brittany.mainWith "brittany" (formatterArgs projectRoot 2 output)
  formatted <- readFile output
  formatted `Hspec.shouldSatisfy` List.isInfixOf "lazyField :: ~Int"
  assertParses output formatted
  (exitCode, standardOutput, standardError) <-
    Process.readProcessWithExitCode "runghc" [output] ""
  exitCode `Hspec.shouldBe` Exit.ExitSuccess
  standardOutput `Hspec.shouldBe` "constructed\n"
  standardError `Hspec.shouldBe` ""

assertParses :: FilePath -> String -> IO ()
assertParses output source = do
  parsed <- ParseModule.parseModule ["-haddock"] output
    (const $ pure $ Right ()) source
  case parsed of
    Left parseError -> Hspec.expectationFailure parseError
    Right _ -> pure ()

fixturePath :: FilePath -> FilePath -> FilePath
fixturePath projectRoot fixtureName = FilePath.combine
  (FilePath.combine projectRoot "source/test-suite/fixtures")
  fixtureName

outputPath :: FilePath -> FilePath -> FilePath
outputPath projectRoot fixtureName =
  FilePath.combine (FilePath.combine projectRoot "output") fixtureName

formatterArgs :: FilePath -> Int -> FilePath -> [String]
formatterArgs projectRoot indent input =
  [ "--config-file"
  , FilePath.combine projectRoot "data/brittany.yaml"
  , "--no-user-config"
  , "--columns"
  , "80"
  , "--indent"
  , show indent
  , "--fail-on-fallback"
  , "--write-mode"
  , "inplace"
  , input
  ]
