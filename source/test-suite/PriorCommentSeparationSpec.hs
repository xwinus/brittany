module PriorCommentSeparationSpec (spec) where

import qualified Data.Map as Map
import qualified GHC.Data.FastString as FastString
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.BackendUtils
  ( finishPriorCommentLineState
  , priorCommentRequiresLineBoundary
  , priorCommentSourceBoundary
  )
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as EP
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.Types
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "prior-comment lexical separation" $ do
  formattingExampleAt 2 projectRoot
    "separates section and Haddock comments from native declarations"
    "PriorCommentSeparationInput.hs"
    "PriorCommentSeparationExpected.hs"
  formattingExampleAt 2 projectRoot
    "preserves mixed comment boundaries at two-space indentation"
    "PriorCommentSeparationEdgeInput.hs"
    "PriorCommentSeparationEdgeIndent2Expected.hs"
  formattingExampleAt 4 projectRoot
    "preserves nested boundaries at four-space indentation"
    "PriorCommentSeparationEdgeInput.hs"
    "PriorCommentSeparationEdgeIndent4Expected.hs"
  parseFailureExample projectRoot
    "retains malformed input after a separated prior comment"
    "PriorCommentSeparationInvalid.hs"

  Hspec.it "makes a newline pending after an emitted line comment" $ do
    let result = finishPriorCommentLineState $ layoutState $ Left 31
    _lstate_curYOrAddNewline result `Hspec.shouldBe` Right 1
    _lstate_addSepSpace result `Hspec.shouldBe` Just 8
    _lstate_commentCol result `Hspec.shouldBe` Just 8

  Hspec.it "does not reduce an existing pending comment boundary" $ do
    let result = finishPriorCommentLineState $ layoutState $ Right 3
    _lstate_curYOrAddNewline result `Hspec.shouldBe` Right 3

  Hspec.it "classifies lexical line boundaries independently of layout tags" $ do
    priorCommentRequiresLineBoundary "-- ordinary" `Hspec.shouldBe` True
    priorCommentRequiresLineBoundary "-- | Haddock" `Hspec.shouldBe` True
    priorCommentRequiresLineBoundary "#if FLAG" `Hspec.shouldBe` True
    priorCommentRequiresLineBoundary "{- block -}" `Hspec.shouldBe` False

  Hspec.it "derives blank-line separation from source spans" $ do
    let nodeSpan = realSpan 5 1 5 21
        commentSpan = realSpan 3 1 3 34
        annKey = EP.AnnKey [EP.realSpanToSrcSpan nodeSpan] $ EP.CN "DataDecl"
        comment = EP.Comment Nothing
          (EP.realSpanToSrcSpan commentSpan)
          "---------------- DATA ----------------"
    priorCommentSourceBoundary annKey comment `Hspec.shouldBe` 2

formattingExampleAt
  :: Int -> FilePath -> String -> FilePath -> FilePath -> Hspec.SpecWith ()
formattingExampleAt indent projectRoot description inputName expectedName =
  Hspec.it description $ do
    let input = fixturePath projectRoot inputName
        expectedFixture = fixturePath projectRoot expectedName
        output = outputPath projectRoot indent inputName
        args = formatterArgs projectRoot indent output
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

parseFailureExample :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
parseFailureExample projectRoot description fixtureName = Hspec.it description $ do
  let fixture = fixturePath projectRoot fixtureName
      output = outputPath projectRoot 2 fixtureName
  expected <- readFile fixture
  Directory.copyFile fixture output
  Brittany.mainWith "brittany" (formatterArgs projectRoot 2 output)
    `Hspec.shouldThrow` (== Exit.ExitFailure 60)
  readFile output `Hspec.shouldReturn` expected

layoutState :: Either Int Int -> LayoutState
layoutState cursor = LayoutState
  { _lstate_baseYs = [4]
  , _lstate_curYOrAddNewline = cursor
  , _lstate_indLevels = [4]
  , _lstate_indLevelLinger = 4
  , _lstate_comments = Map.empty
  , _lstate_commentCol = Just 8
  , _lstate_addSepSpace = Just 2
  , _lstate_commentNewlines = 0
  }

realSpan :: Int -> Int -> Int -> Int -> SrcLoc.RealSrcSpan
realSpan startLine startColumn endLine endColumn = SrcLoc.mkRealSrcSpan
  (SrcLoc.mkRealSrcLoc testFile startLine startColumn)
  (SrcLoc.mkRealSrcLoc testFile endLine endColumn)
 where
  testFile = FastString.fsLit "PriorCommentSeparationSpec.hs"

fixturePath :: FilePath -> FilePath -> FilePath
fixturePath projectRoot fixtureName = FilePath.combine
  (FilePath.combine projectRoot "source/test-suite/fixtures")
  fixtureName

outputPath :: FilePath -> Int -> FilePath -> FilePath
outputPath projectRoot indent fixtureName = FilePath.combine
  (FilePath.combine projectRoot "output")
  (FilePath.takeBaseName fixtureName ++ "-" ++ show indent ++ ".hs")

formatterArgs :: FilePath -> Int -> FilePath -> [String]
formatterArgs projectRoot indent input =
  [ "--config-file"
  , FilePath.combine projectRoot "data/brittany.yaml"
  , "--no-user-config"
  , "--columns"
  , "80"
  , "--indent"
  , show indent
  , "--write-mode"
  , "inplace"
  , input
  ]
