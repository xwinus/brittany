{-# LANGUAGE StandaloneKindSignatures #-}

import qualified Control.Monad as Monad
import qualified CompatibilitySpec
import qualified CanonicalSemanticModelSpec
import qualified CommentPlanSpec
import qualified CommentOwnershipSpec
import qualified CompactParenthesizedPatternSpec
import qualified ComposableDeclarationSpec
import qualified ConstructorFieldModifierSpec
import Data.Kind (Type)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified DataDeclSingleConstructorSpec
import qualified ExtractAnnsSpec
import qualified ExactSourceFragmentSpec
import qualified FallbackSpec
import qualified FallbackSweepSpec
import qualified FixitySignatureSpec
import qualified InstanceHeadSpec
import qualified LetStatementBoundarySpec
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified OpaqueSyntaxSpec
import qualified PatternSynonymBindingSpec
import qualified PreambleSpacingSpec
import qualified PreprocessorSpec
import qualified PriorCommentSeparationSpec
import qualified PrefixConstructorIndentationSpec
import qualified RecordFieldRhsIndentSpec
import qualified RegressionSpec
import qualified SemanticFingerprintSpec
import qualified SignaturePostDocSpec
import qualified SiblingBindingAlignmentSpec
import qualified StandaloneDerivingSpec
import qualified StandaloneKindSignatureSpec
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified TemplateHaskellFallbackSpec
import qualified Test.Hspec as Hspec
import qualified TopLevelSpacingSpec
import qualified TransactionalInplaceSpec
import qualified TypeOperatorRecordSpec
import qualified TupleSemanticNameSpec

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
  CanonicalSemanticModelSpec.spec
  SemanticFingerprintSpec.spec
  SignaturePostDocSpec.spec projectRoot
  SiblingBindingAlignmentSpec.spec projectRoot
  StandaloneDerivingSpec.spec projectRoot
  StandaloneKindSignatureSpec.spec projectRoot
  DataDeclSingleConstructorSpec.spec projectRoot
  ExtractAnnsSpec.spec
  ExactSourceFragmentSpec.spec projectRoot
  CompatibilitySpec.spec projectRoot
  CommentPlanSpec.spec
  CommentOwnershipSpec.spec
  CompactParenthesizedPatternSpec.spec projectRoot
  ComposableDeclarationSpec.spec projectRoot
  ConstructorFieldModifierSpec.spec projectRoot
  FallbackSpec.spec projectRoot
  FallbackSweepSpec.spec
  FixitySignatureSpec.spec projectRoot
  InstanceHeadSpec.spec projectRoot
  LetStatementBoundarySpec.spec projectRoot
  OpaqueSyntaxSpec.spec projectRoot
  PatternSynonymBindingSpec.spec projectRoot
  PreambleSpacingSpec.spec projectRoot
  TemplateHaskellFallbackSpec.spec projectRoot
  PreprocessorSpec.spec projectRoot
  PriorCommentSeparationSpec.spec projectRoot
  PrefixConstructorIndentationSpec.spec projectRoot
  RecordFieldRhsIndentSpec.spec projectRoot
  TopLevelSpacingSpec.spec
  TransactionalInplaceSpec.spec projectRoot
  TypeOperatorRecordSpec.spec projectRoot
  TupleSemanticNameSpec.spec projectRoot

type Literal :: Type
newtype Literal
  = Literal String
  deriving Eq

instance Show Literal where
  show (Literal value) = value
