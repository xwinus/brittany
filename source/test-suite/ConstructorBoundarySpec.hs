module ConstructorBoundarySpec (spec) where

import qualified Data.Char                               as Char
import qualified Data.Text                               as Text
import           Language.Haskell.Brittany.Internal.CommentPlan
                                                          ( commentPlanFingerprint
                                                          , normalizeCommentPlan
                                                          )
import qualified Language.Haskell.Brittany.Internal.ParseModule
                                                         as ParseModule
import           Language.Haskell.Brittany.Internal.SourceComment.Types
                                                          ( CommentRole
                                                          , SourceCommentSyntax
                                                          )
import qualified Language.Haskell.Brittany.Main          as Brittany
import qualified System.Directory                        as Directory
import qualified System.Exit                             as Exit
import qualified System.FilePath                         as FilePath
import qualified Test.Hspec                              as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "constructor comment boundaries" $ do
  formattingExample projectRoot
                    "keeps leading docs and post-docs with their constructors"
                    "ConstructorBoundaryInput.hs"
                    "ConstructorBoundaryExpected.hs"
  formattingExample projectRoot
                    "handles ordinary comments and mixed constructor forms"
                    "ConstructorHaddockEdge.hs"
                    "ConstructorHaddockEdge.hs"
  narrowFormattingExample projectRoot
                          "derives continuation indentation at a narrow width"
                          "LongPrefixConstructorExpected.hs"
  parseFailureExample projectRoot
                      "leaves a malformed commented declaration unchanged"
                      "ConstructorBoundaryInvalid.hs"

formattingExample
  :: FilePath -> String -> FilePath -> FilePath -> Hspec.SpecWith ()
formattingExample projectRoot description inputName expectedName =
  Hspec.it description $ do
    let input           = fixturePath projectRoot inputName
        expectedFixture = fixturePath projectRoot expectedName
        output          = outputPath projectRoot inputName
        arguments       = formatterArguments projectRoot output
    expected <- readFile expectedFixture
    Directory.copyFile input output
    Brittany.mainWith "brittany" arguments
    firstPass <- readFile output
    firstPass `Hspec.shouldBe` expected
    assertNoOrphanSeparators firstPass
    firstFingerprint <- commentFingerprint output firstPass
    Brittany.mainWith "brittany" arguments
    secondPass <- readFile output
    secondPass `Hspec.shouldBe` firstPass
    secondFingerprint <- commentFingerprint output secondPass
    secondFingerprint `Hspec.shouldBe` firstFingerprint
    Brittany.mainWith "brittany" arguments
    thirdPass <- readFile output
    thirdPass `Hspec.shouldBe` secondPass
    thirdFingerprint <- commentFingerprint output thirdPass
    thirdFingerprint `Hspec.shouldBe` firstFingerprint

parseFailureExample :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
parseFailureExample projectRoot description fixtureName =
  Hspec.it description $ do
    let fixture = fixturePath projectRoot fixtureName
        output  = outputPath projectRoot fixtureName
    expected <- readFile fixture
    Directory.copyFile fixture output
    Brittany.mainWith "brittany" (formatterArguments projectRoot output)
      `Hspec.shouldThrow` (== Exit.ExitFailure 60)
    readFile output `Hspec.shouldReturn` expected

narrowFormattingExample :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
narrowFormattingExample projectRoot description fixtureName =
  Hspec.it description $ do
    let fixture = fixturePath projectRoot fixtureName
        output  = outputPath projectRoot $ "Narrow" ++ fixtureName
        arguments =
          ["--columns", "44", "--indent", "4"]
            ++ formatterBaseArguments projectRoot output
    Directory.copyFile fixture output
    Brittany.mainWith "brittany" arguments
    firstPass <- readFile output
    assertNoOrphanSeparators firstPass
    filter ((> 44) . length) (lines firstPass) `Hspec.shouldBe` []
    firstFingerprint <- commentFingerprint output firstPass
    Brittany.mainWith "brittany" arguments
    secondPass <- readFile output
    secondPass `Hspec.shouldBe` firstPass
    secondFingerprint <- commentFingerprint output secondPass
    secondFingerprint `Hspec.shouldBe` firstFingerprint
    Brittany.mainWith "brittany" arguments
    readFile output `Hspec.shouldReturn` secondPass

assertNoOrphanSeparators :: String -> Hspec.Expectation
assertNoOrphanSeparators source =
  filter isOrphanSeparator (lines source) `Hspec.shouldBe` []
 where
  isOrphanSeparator line = dropWhile Char.isSpace line `elem` ["=", "|"]

commentFingerprint
  :: FilePath
  -> String
  -> IO [(Text.Text, SourceCommentSyntax, CommentRole, String)]
commentFingerprint filename source = do
  parsed <- ParseModule.parseModule ["-haddock"]
                                    filename
                                    (const $ pure $ Right ())
                                    source
  case parsed of
    Left  parseError           -> Hspec.expectationFailure parseError >> pure []
    Right (annotations, _, ()) -> case normalizeCommentPlan annotations of
      Left  planErrors -> Hspec.expectationFailure (show planErrors) >> pure []
      Right plan       -> pure $ commentPlanFingerprint plan

fixturePath :: FilePath -> FilePath -> FilePath
fixturePath projectRoot fixtureName = FilePath.combine
  (FilePath.combine projectRoot "source/test-suite/fixtures")
  fixtureName

outputPath :: FilePath -> FilePath -> FilePath
outputPath projectRoot fixtureName =
  FilePath.combine (FilePath.combine projectRoot "output") fixtureName

formatterArguments :: FilePath -> FilePath -> [String]
formatterArguments projectRoot input =
  ["--columns", "80"] ++ formatterBaseArguments projectRoot input

formatterBaseArguments :: FilePath -> FilePath -> [String]
formatterBaseArguments projectRoot input =
  [ "--config-file"
  , FilePath.combine projectRoot "data/brittany.yaml"
  , "--no-user-config"
  , "--fail-on-fallback"
  , "--write-mode"
  , "inplace"
  , input
  ]
