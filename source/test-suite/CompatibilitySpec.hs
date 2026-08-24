module CompatibilitySpec (spec) where

import qualified CompatibilityMatrix as Matrix
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "GHC 9.14 compatibility matrix" $ do
  loaded <- Hspec.runIO $ loadMatrixAndPragmas projectRoot
  case loaded of
    Left loadError ->
      Hspec.it "loads the compatibility manifest" $
        Hspec.expectationFailure loadError
    Right (matrix, discoveredPragmas) -> do
      Hspec.describe "manifest validation" $ do
        Hspec.it "accepts the complete checked-in manifest" $ do
          validationErrors projectRoot matrix discoveredPragmas
            `Hspec.shouldReturn` []
        Hspec.it "rejects duplicate feature classifications" $ do
          case Matrix.matrixFeatures matrix of
            [] -> Hspec.expectationFailure "matrix has no features"
            firstFeature : _ -> do
              let duplicateMatrix = matrix
                    { Matrix.matrixFeatures =
                        firstFeature : Matrix.matrixFeatures matrix
                    }
              Matrix.validateMatrix duplicateMatrix discoveredPragmas
                `Hspec.shouldContain`
                  ["duplicate feature: " ++ Matrix.featureName firstFeature]
        Hspec.it "rejects cases that reference an unknown feature" $ do
          case Matrix.matrixCases matrix of
            [] -> Hspec.expectationFailure "matrix has no cases"
            firstCase : remainingCases -> do
              let invalidCase = firstCase
                    { Matrix.matrixCaseFeatures = ["UnclassifiedFeature"] }
                  invalidMatrix = matrix
                    { Matrix.matrixCases = invalidCase : remainingCases }
              Matrix.validateMatrix invalidMatrix discoveredPragmas
                `Hspec.shouldContain`
                  [ "case references unknown feature UnclassifiedFeature: "
                    ++ Matrix.matrixCaseName firstCase
                  ]
        Hspec.it "rejects skipped compatibility cases" $ do
          case Matrix.matrixCases matrix of
            [] -> Hspec.expectationFailure "matrix has no cases"
            firstCase : remainingCases -> do
              let skippedCase = firstCase { Matrix.matrixCaseSkipped = True }
                  invalidMatrix = matrix
                    { Matrix.matrixCases = skippedCase : remainingCases }
              Matrix.validateMatrix invalidMatrix discoveredPragmas
                `Hspec.shouldContain`
                  ["case is marked skipped: " ++ Matrix.matrixCaseName firstCase]
        Hspec.it "accepts syntax classifications without LANGUAGE pragmas" $ do
          let syntaxErrors = filter (List.isInfixOf "ModuleHeaders")
                $ Matrix.validateMatrix matrix discoveredPragmas
          syntaxErrors `Hspec.shouldBe` []
        Hspec.it "still requires LANGUAGE pragmas for extension cases" $ do
          let reclassify feature
                | Matrix.featureName feature == "ModuleHeaders" =
                    feature { Matrix.featureKind = Matrix.Extension }
                | otherwise = feature
              invalidMatrix = matrix
                { Matrix.matrixFeatures = reclassify <$> Matrix.matrixFeatures matrix
                }
          Matrix.validateMatrix invalidMatrix discoveredPragmas
            `Hspec.shouldContain`
              [ "case does not enable feature ModuleHeaders in data/Test132.hs"
              ]
        Hspec.it "rejects a syntax classification without coverage" $ do
          let keepsCase matrixCase =
                "ModuleHeaders" `notElem` Matrix.matrixCaseFeatures matrixCase
              invalidMatrix = matrix
                { Matrix.matrixCases = filter keepsCase $ Matrix.matrixCases matrix
                }
          Matrix.validateMatrix invalidMatrix discoveredPragmas
            `Hspec.shouldContain`
              ["feature has no compatibility case: ModuleHeaders"]

      Hspec.describe "classified feature cases" $
        Monad.forM_ (Matrix.matrixCases matrix) $ \matrixCase ->
          Hspec.it (Matrix.matrixCaseName matrixCase) $
            runMatrixCase projectRoot matrixCase

loadMatrixAndPragmas
  :: FilePath
  -> IO (Either String (Matrix.Matrix, [(String, FilePath)]))
loadMatrixAndPragmas projectRoot = do
  decoded <- Matrix.loadMatrix $ FilePath.combine projectRoot "data/compatibility.yaml"
  case decoded of
    Left loadError -> pure $ Left loadError
    Right matrix -> do
      discoveredPragmas <- Matrix.discoverLanguagePragmas projectRoot
      pure $ Right (matrix, discoveredPragmas)

validationErrors
  :: FilePath
  -> Matrix.Matrix
  -> [(String, FilePath)]
  -> IO [String]
validationErrors projectRoot matrix discoveredPragmas = do
  missingFixtures <- Monad.filterM (fmap not . Directory.doesFileExist)
    [FilePath.combine projectRoot $ Matrix.matrixCaseFixture matrixCase
    | matrixCase <- Matrix.matrixCases matrix]
  pure
    $ Matrix.validateMatrix matrix discoveredPragmas
    ++ ["missing compatibility fixture: " ++ path | path <- missingFixtures]

runMatrixCase :: FilePath -> Matrix.MatrixCase -> IO ()
runMatrixCase projectRoot matrixCase = do
  let fixture = FilePath.combine projectRoot $ Matrix.matrixCaseFixture matrixCase
      outputDirectory = FilePath.combine projectRoot "output/compatibility"
      output = FilePath.combine outputDirectory $ FilePath.takeFileName fixture
  Directory.createDirectoryIfMissing True outputDirectory
  expected <- readFile fixture
  Directory.copyFile fixture output
  case Matrix.matrixCaseExpectedResult matrixCase of
    Matrix.Formats -> do
      Brittany.mainWith "brittany" $ formatterArgs projectRoot output
      firstPass <- readFile output
      firstPass `Hspec.shouldBe` expected
      Brittany.mainWith "brittany" $ formatterArgs projectRoot output
      secondPass <- readFile output
      secondPass `Hspec.shouldBe` firstPass
    Matrix.ParseFailure -> do
      Brittany.mainWith "brittany" (formatterArgs projectRoot output)
        `Hspec.shouldThrow` (== Exit.ExitFailure 60)
      actual <- readFile output
      actual `Hspec.shouldBe` expected
    Matrix.FormattingFailure -> do
      Brittany.mainWith "brittany" (formatterArgs projectRoot output)
        `Hspec.shouldThrow` (== Exit.ExitFailure 70)
      actual <- readFile output
      actual `Hspec.shouldBe` expected

formatterArgs :: FilePath -> FilePath -> [String]
formatterArgs projectRoot input =
  [ "--config-file"
  , FilePath.combine projectRoot "data/brittany.yaml"
  , "--no-user-config"
  , "--write-mode"
  , "inplace"
  , input
  ]
