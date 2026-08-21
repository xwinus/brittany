module RegressionSpec (spec) where

import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "GHC 9.14 regressions" $ do
  derivingViaExample projectRoot "DerivingViaExpected.hs"
  derivingViaExample projectRoot "DerivingViaEdge.hs"

  Hspec.it "rejects malformed DerivingVia syntax" $ do
    let fixture = fixturePath projectRoot "DerivingViaInvalid.hs"
    Brittany.mainWith "brittany" (formatterArgs projectRoot fixture)
      `Hspec.shouldThrow` isParseFailure

derivingViaExample :: FilePath -> FilePath -> Hspec.SpecWith ()
derivingViaExample projectRoot fixtureName =
  Hspec.it ("formats " ++ fixtureName ++ " without crashing") $ do
    let fixture = fixturePath projectRoot fixtureName
        output = FilePath.combine (FilePath.combine projectRoot "output") fixtureName
    expected <- readFile fixture
    Directory.copyFile fixture output
    Brittany.mainWith "brittany" (formatterArgs projectRoot output)
    actual <- readFile output
    actual `Hspec.shouldBe` expected

fixturePath :: FilePath -> FilePath -> FilePath
fixturePath projectRoot fixtureName =
  FilePath.combine
    (FilePath.combine projectRoot "source/test-suite/fixtures")
    fixtureName

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
