module ComposableDeclarationSpec (spec) where

import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "composable declaration comments" $ do
  formattingExampleAt 2 projectRoot
    "formats documented declaration forms without declaration fallback"
    "ComposableDeclarationsExpected.hs"
  formattingExampleAt 4 projectRoot
    "uses configured indentation across composable declaration forms"
    "ComposableDeclarationsIndent4Expected.hs"
  parseFailureExample projectRoot
    "rejects malformed composable syntax without changing input"

formattingExampleAt :: Int -> FilePath -> String -> FilePath -> Hspec.SpecWith ()
formattingExampleAt indent projectRoot description expectedName =
  Hspec.it description $ do
    let input = fixturePath projectRoot "ComposableDeclarationsInput.hs"
        expectedFixture = fixturePath projectRoot expectedName
        output = FilePath.combine projectRoot
          $ "output/ComposableDeclarations" ++ show indent ++ ".hs"
        args =
          [ "--columns", "80"
          , "--indent", show indent
          , "--fail-on-fallback"
          ] ++ formatterArgs projectRoot output
    expected <- readFile expectedFixture
    Directory.copyFile input output
    Brittany.mainWith "brittany" args
    firstPass <- readFile output
    firstPass `Hspec.shouldBe` expected
    parsed <- ParseModule.parseModule ["-haddock"] output
      (const $ pure $ Right ()) firstPass
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError
      Right _ -> pure ()
    Brittany.mainWith "brittany" args
    readFile output `Hspec.shouldReturn` firstPass

parseFailureExample :: FilePath -> String -> Hspec.SpecWith ()
parseFailureExample projectRoot description = Hspec.it description $ do
  let input = fixturePath projectRoot "ComposableDeclarationsInvalid.hs"
      output = FilePath.combine projectRoot
        "output/ComposableDeclarationsInvalid.hs"
  expected <- readFile input
  Directory.copyFile input output
  Brittany.mainWith "brittany" (formatterArgs projectRoot output)
    `Hspec.shouldThrow` (== Exit.ExitFailure 60)
  readFile output `Hspec.shouldReturn` expected

fixturePath :: FilePath -> FilePath -> FilePath
fixturePath projectRoot fixtureName = FilePath.combine
  (FilePath.combine projectRoot "source/test-suite/fixtures")
  fixtureName

formatterArgs :: FilePath -> FilePath -> [String]
formatterArgs projectRoot input =
  [ "--config-file"
  , FilePath.combine projectRoot "data/brittany.yaml"
  , "--no-user-config"
  , "--write-mode", "inplace"
  , input
  ]
