{-# LANGUAGE StandaloneKindSignatures #-}

import qualified Control.Monad as Monad
import qualified CompatibilitySpec
import Data.Kind (Type)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified PreprocessorSpec
import qualified RegressionSpec
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

findProjectRoot :: FilePath -> IO FilePath
findProjectRoot dir = do
  hasCabal <- Directory.doesFileExist (FilePath.combine dir "brittany.cabal")
  hasData <- Directory.doesDirectoryExist (FilePath.combine dir "data")
  if hasCabal || hasData
    then pure dir
    else
      let parent = FilePath.takeDirectory dir
      in if parent == dir then pure dir else findProjectRoot parent

knownCommentFailures :: Set.Set String
knownCommentFailures = Set.empty

main :: IO ()
main = Hspec.hspec $ do
  projectRoot <- Hspec.runIO $ findProjectRoot =<< Directory.getCurrentDirectory
  Hspec.runIO $ Directory.setCurrentDirectory projectRoot
  let dataDir = FilePath.combine projectRoot "data"
      outputDir = FilePath.combine projectRoot "output"
  Hspec.runIO $ Directory.createDirectoryIfMissing True outputDir
  entries <- Hspec.runIO $ Directory.listDirectory dataDir
  Monad.forM_ (List.sort entries) $ \entry ->
    case FilePath.stripExtension "hs" entry of
      Nothing -> pure ()
      Just slug -> Hspec.it slug $ do
        Monad.when (Set.member slug knownCommentFailures)
          $ Hspec.pendingWith "Known GHC 9.14 comment-layout regression"
        let input = FilePath.combine dataDir entry
            output = FilePath.combine outputDir entry
            configFile = FilePath.combine dataDir "brittany.yaml"
        expected <- readFile input
        Directory.copyFile input output
        Brittany.mainWith
          "brittany"
          [ "--config-file"
          , configFile
          , "--no-user-config"
          , "--write-mode"
          , "inplace"
          , output
          ]
        actual <- readFile output
        Literal actual `Hspec.shouldBe` Literal expected

  RegressionSpec.spec projectRoot
  CompatibilitySpec.spec projectRoot
  PreprocessorSpec.spec projectRoot

type Literal :: Type
newtype Literal
  = Literal String
  deriving Eq

instance Show Literal where
  show (Literal value) = value
