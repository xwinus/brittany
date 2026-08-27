module RecordFieldRhsIndentSpec (spec) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "record field RHS indentation" $ do
  formattingExample
    40
    2
    ["--fail-on-fallback"]
    projectRoot
    "indents each nested record value beyond its field name"
    "RecordFieldRhsIndentInput.hs"
    "RecordFieldRhsIndentExpected.hs"
  formattingExample
    60
    2
    []
    projectRoot
    "handles field positions, value forms, comments, puns, and wildcards"
    "RecordFieldRhsIndentEdgeInput.hs"
    "RecordFieldRhsIndentEdgeIndent2Expected.hs"
  formattingExample
    60
    4
    []
    projectRoot
    "uses the configured four-space continuation indent"
    "RecordFieldRhsIndentEdgeInput.hs"
    "RecordFieldRhsIndentEdgeIndent4Expected.hs"
  Hspec.it "indents every native broken field RHS by a full continuation level" $ do
    expected <- readFixture projectRoot "RecordFieldRhsIndentExpected.hs"
    edgeIndent2 <- readFixture
      projectRoot "RecordFieldRhsIndentEdgeIndent2Expected.hs"
    edgeIndent4 <- readFixture
      projectRoot "RecordFieldRhsIndentEdgeIndent4Expected.hs"
    assertContinuationIndent 2 expected
    assertContinuationIndent 2 edgeIndent2
    assertContinuationIndent 4 edgeIndent4
  parseFailureExample
    projectRoot
    "rejects malformed nested construction and update syntax without changes"
    "RecordFieldRhsIndentInvalid.hs"

formattingExample
  :: Int
  -> Int
  -> [String]
  -> FilePath
  -> String
  -> FilePath
  -> FilePath
  -> Hspec.SpecWith ()
formattingExample
  columns indent extraArgs projectRoot description inputName expectedName =
    Hspec.it description $ do
      let input = fixturePath projectRoot inputName
          expectedFixture = fixturePath projectRoot expectedName
          output = outputPath projectRoot inputName
          args =
            ["--columns", show columns, "--indent", show indent]
              ++ extraArgs
              ++ formatterArgs projectRoot output
      expected <- readFile expectedFixture
      Directory.copyFile input output
      Brittany.mainWith "brittany" args
      firstPass <- readFile output
      firstPass `Hspec.shouldBe` expected
      filter ((> columns) . length) (lines firstPass) `Hspec.shouldBe` []
      Brittany.mainWith "brittany" args
      secondPass <- readFile output
      secondPass `Hspec.shouldBe` firstPass

assertContinuationIndent :: Int -> String -> Hspec.Expectation
assertContinuationIndent configuredIndent source = do
  let sourceLines = lines source
      candidates = Maybe.mapMaybe
        (uncurry $ continuationCandidate sourceLines configuredIndent)
        (zip [1 ..] sourceLines)
  candidates `Hspec.shouldSatisfy` (not . null)
  concat candidates `Hspec.shouldBe` []

continuationCandidate :: [String] -> Int -> Int -> String -> Maybe [String]
continuationCandidate sourceLines configuredIndent lineNumber line = do
  fieldColumn <- structuralFieldColumn line
  let followingLines = drop lineNumber sourceLines
      rhsLines = dropWhile isBlankOrComment followingLines
      requiredColumn = fieldColumn + configuredIndent
  pure $ case rhsLines of
    [] -> ["missing RHS after line " ++ show lineNumber]
    rhsLine : _
      | leadingSpaces rhsLine >= requiredColumn -> []
      | otherwise ->
          [ "RHS after line "
            ++ show lineNumber
            ++ " starts at column "
            ++ show (leadingSpaces rhsLine)
            ++ ", expected at least "
            ++ show requiredColumn
          ]

structuralFieldColumn :: String -> Maybe Int
structuralFieldColumn line =
  let indentation = leadingSpaces line
      trimmed = drop indentation line
      structuralPrefix = "{ " `List.isPrefixOf` trimmed
        || ", " `List.isPrefixOf` trimmed
      brokenRhs = "=" `List.isSuffixOf` List.dropWhileEnd Char.isSpace trimmed
  in if structuralPrefix && brokenRhs
    then Just $ indentation + 2
    else Nothing

isBlankOrComment :: String -> Bool
isBlankOrComment line =
  let trimmed = dropWhile Char.isSpace line
  in null trimmed || "--" `List.isPrefixOf` trimmed

leadingSpaces :: String -> Int
leadingSpaces = length . takeWhile Char.isSpace

parseFailureExample :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
parseFailureExample projectRoot description fixtureName = Hspec.it description $ do
  let fixture = fixturePath projectRoot fixtureName
      output = outputPath projectRoot fixtureName
  expected <- readFile fixture
  Directory.copyFile fixture output
  Brittany.mainWith "brittany" (formatterArgs projectRoot output)
    `Hspec.shouldThrow` (== Exit.ExitFailure 60)
  actual <- readFile output
  actual `Hspec.shouldBe` expected

readFixture :: FilePath -> FilePath -> IO String
readFixture projectRoot = readFile . fixturePath projectRoot

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
