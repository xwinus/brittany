module SignaturePostDocSpec (spec) where

import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "Haddock-commented type signatures" $ do
  formattingExampleAt 2 projectRoot
    "keeps a final class signature post-doc inside the class"
    "ClassFinalPostDocInput.hs"
    "ClassFinalPostDocExpected.hs"
  formattingExampleAt 2 projectRoot
    "preserves the next declaration's leading documentation"
    "ClassFinalPostDocEdgeInput.hs"
    "ClassFinalPostDocEdgeExpected.hs"
  formattingExampleAt 2 projectRoot
    "formats a Headroom-style signature at configured indentation"
    "SignaturePostDocInput.hs"
    "SignaturePostDocExpected.hs"
  formattingExampleAt 2 projectRoot
    "preserves complex signature post-doc ownership and order"
    "SignaturePostDocEdgeInput.hs"
    "SignaturePostDocEdgeExpected.hs"
  formattingExampleAt 4 projectRoot
    "uses configured four-space indentation for signature post-docs"
    "SignaturePostDocEdgeInput.hs"
    "SignaturePostDocEdgeIndent4Expected.hs"
  parseFailureExample projectRoot
    "rejects malformed documented signatures without changing input"
    "SignaturePostDocInvalid.hs"
  parseFailureExample projectRoot
    "rejects a malformed final class signature without changing input"
    "ClassFinalPostDocInvalid.hs"

formattingExampleAt
  :: Int -> FilePath -> String -> FilePath -> FilePath -> Hspec.SpecWith ()
formattingExampleAt indent projectRoot description inputName expectedName =
  Hspec.it description $ do
    let input = fixturePath projectRoot inputName
        expectedFixture = fixturePath projectRoot expectedName
        output = outputPath projectRoot inputName
        args =
          [ "--columns"
          , "80"
          , "--indent"
          , show indent
          , "--fail-on-fallback"
          ] ++ formatterArgs projectRoot output
    expected <- readFile expectedFixture
    Directory.copyFile input output
    Brittany.mainWith "brittany" args
    firstPass <- readFile output
    firstPass `Hspec.shouldBe` expected
    filter ((> 80) . length) (lines firstPass) `Hspec.shouldBe` []
    parsed <- ParseModule.parseModule ["-haddock"] output
      (const $ pure $ Right ()) firstPass
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError
      Right _ -> pure ()
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
  readFile output `Hspec.shouldReturn` expected

fixturePath :: FilePath -> FilePath -> FilePath
fixturePath projectRoot fixtureName = FilePath.combine
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
