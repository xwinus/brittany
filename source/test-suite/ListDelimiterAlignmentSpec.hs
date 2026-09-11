{-# LANGUAGE LambdaCase #-}

module ListDelimiterAlignmentSpec (spec) where

import qualified Control.Exception as Exception
import Control.Monad (forM_)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import Data.Semigroup (Last(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Language.Haskell.Brittany
  ( CConfig(..)
  , CErrorHandlingConfig(..)
  , CLayoutConfig(..)
  , Config
  , parsePrintModule
  , staticDefaultConfig
  )
import Language.Haskell.Brittany.Internal.CommentPlan
  ( commentPlanFingerprint
  , normalizeCommentPlan
  )
import Language.Haskell.Brittany.Internal.Config.Types (IndentPolicy(..))
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SemanticFingerprint
  ( compareSemanticSyntax
  )
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "list delimiter alignment" $ do
  Hspec.it "aligns the StandardSuite list in the complete benchmark Scenario module" $ do
    source <- readFile $ projectRoot </> "benchmark/Benchmark/Scenario.hs"
    output <- checkedModule (configWithLayout 80 2 IndentPolicyFree) source
    let standardBranch = unlines $ takeWhile (not . List.isInfixOf "ScalingSuite ->")
          $ dropWhile (not . List.isInfixOf "StandardSuite ->") $ lines output
    assertOuterAlignment "\"alt-parse\"" 6 standardBranch

  Hspec.it "preserves inherited comparison indentation in the complete AltCommentSpec" $ do
    source <- readFile $ projectRoot </> "source/test-suite/AltCommentSpec.hs"
    output <- checkedModule (configWithLayout 80 2 IndentPolicyFree) source
    let comparisons = filter (List.isPrefixOf "==" . dropWhile (== ' ')) $ lines output
    length comparisons `Hspec.shouldBe` 2
    map (length . takeWhile (== ' ')) comparisons `Hspec.shouldBe` [6, 6]

  Hspec.it "retains the enclosing continuation base for a parenthesized do-statement comparison" $ do
    output <- checkedSource 80 2 IndentPolicyFree $ moduleSource
      [ "example = do"
      , "  describe \"comparison\" $ do"
      , "    ((normalizeDocument <$> comments, normalizeDocument remaining)"
      , "      == ([normalizeDocument comment], Pair firstValue secondValue)"
      , "      ) `shouldBe` True"
      ]
    output `Hspec.shouldContain` List.intercalate "\n"
      [ "    ((normalizeDocument <$> comments, normalizeDocument remaining)"
      , "      == ([normalizeDocument comment], Pair firstValue secondValue)"
      , "      )"
      , "      `shouldBe` True"
      ]

  forM_ [40, 80, 100] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ policies $ \policy -> do
      forM_ listContexts $ \(description, suffix) ->
        Hspec.it ("aligns " ++ description ++ layoutDescription columns indent policy) $ do
          output <- checkedSource columns indent policy $ scenarioSource suffix
          assertOuterAlignment "\"first-selected-scenario\"" 5 output
      Hspec.it ("keeps empty and singleton lists compact" ++ layoutDescription columns indent policy) $ do
        output <- checkedSource columns indent policy $ moduleSource
          [ "empty = []"
          , "singleton = [one]"
          , "combined = [] ++ [one]"
          , "small = [one, two]"
          ]
        forM_ ["empty = []", "singleton = [one]", "combined = [] ++ [one]", "small = [one, two]"] $ \line ->
          output `Hspec.shouldContain` line

  forM_ [40, 80, 100] $ \columns -> forM_ [2, 4] $ \indent -> do
    Hspec.it ("aligns delimiters around wrapped elements" ++ layoutDescription columns indent IndentPolicyFree) $ do
      output <- checkedSource columns indent IndentPolicyFree $ moduleSource
        [ "result choice = case choice of"
        , "  Selected ->"
        , "    [ buildFirstScenario firstValue secondValue thirdValue fourthValue fifthValue sixthValue"
        , "    , buildSecondScenario firstValue secondValue thirdValue fourthValue fifthValue sixthValue"
        , "    ] ++ extraScenarios"
        ]
      assertOuterAlignment "buildFirstScenario" 2 output
    Hspec.it ("distinguishes outer delimiters from nested lists and quoted punctuation"
      ++ layoutDescription columns indent IndentPolicyFree) $ do
        output <- checkedSource columns indent IndentPolicyFree $ moduleSource
          [ "result choice = case choice of"
          , "  Selected ->"
          , "    [ wrapNested [\"first [,] marker\", \"another nested value\", \"third nested value\"]"
          , "    , wrapSecond [\"second [,] marker\", \"another nested value\", \"third nested value\"]"
          , "    ] ++ extraScenarios"
          ]
        assertOuterAlignment "wrapNested" 2 output

  forM_ [2, 4] $ \indent -> forM_ commentCases $ \(description, entries) ->
    Hspec.it ("preserves " ++ description ++ " and punctuation at indent " ++ show indent) $ do
      output <- checkedSource 80 indent IndentPolicyFree $ moduleSource $
        ["result choice = case choice of", "  Selected ->"] ++ entries
        ++ ["    ] ++ extraScenarios"]
      assertOuterAlignment "firstScenario" 3 output
      forM_ ["firstScenario", "secondScenario", "thirdScenario"] $ \marker ->
        length (filter (List.isInfixOf marker) $ lines output) `Hspec.shouldBe` 1

  forM_ malformedLists $ \(description, declarations) ->
    Hspec.it ("rejects " ++ description ++ " without replacing inplace input") $ do
      let source = moduleSource declarations
      directory <- Directory.getTemporaryDirectory
      Exception.bracket
        (do
          (path, handle) <- IO.openTempFile directory "brittany-list-delimiter-invalid.hs"
          IO.hPutStr handle source
          IO.hClose handle
          pure path)
        Directory.removeFile
        $ \path -> do
          Brittany.mainWith "brittany"
            [ "--no-user-config", "--write-mode", "inplace"
            , "--werror", "--fail-on-fallback", path
            ] `Hspec.shouldThrow` (== Exit.ExitFailure 60)
          TextIO.readFile path `Hspec.shouldReturn` Text.pack source

checkedModule :: Config -> String -> IO String
checkedModule config source = do
  output <- formatChecked config source
  assertStableAndEquivalent config source output
  pure output

checkedSource :: Int -> Int -> IndentPolicy -> String -> IO String
checkedSource columns indent policy source = do
  output <- checkedModule (configWithLayout columns indent policy) source
  filter ((> columns) . length) (lines output) `Hspec.shouldBe` []
  pure output

assertOuterAlignment :: String -> Int -> String -> IO ()
assertOuterAlignment marker elementCount source = do
  markerOffset <- case List.findIndex (List.isPrefixOf marker) $ List.tails source of
    Just offset -> pure offset
    Nothing -> Hspec.expectationFailure ("missing list marker " ++ marker) >> fail "missing marker"
  opening <- case [offset | (offset, '[') <- punctuation source, offset < markerOffset] of
    [] -> Hspec.expectationFailure "missing outer opening bracket" >> fail "missing opening"
    offsets -> pure $ last offsets
  let delimiters = opening : selectDelimiters 1
        (dropWhile ((<= opening) . fst) $ punctuation source)
      column offset = length $ takeWhile (/= '\n') $ reverse $ take offset source
  length delimiters `Hspec.shouldBe` elementCount + 1
  map column delimiters `Hspec.shouldBe` replicate (elementCount + 1) (column opening)

selectDelimiters :: Int -> [(Int, Char)] -> [Int]
selectDelimiters _ [] = []
selectDelimiters depth ((offset, symbol) : rest) = case symbol of
  '[' -> selectDelimiters (depth + 1) rest
  ']' | depth == 1 -> [offset]
      | otherwise -> selectDelimiters (depth - 1) rest
  ',' | depth == 1 -> offset : selectDelimiters depth rest
  _ -> selectDelimiters depth rest

-- The fixtures contain nested lists, quoted punctuation and delimiter-like comments.
-- Only punctuation outside those strings/comments contributes to list structure.
punctuation :: String -> [(Int, Char)]
punctuation = normal 0
 where
  normal _ [] = []
  normal offset ('"' : rest) = quoted (offset + 1) rest
  normal offset ('-' : '-' : rest) = lineComment (offset + 2) rest
  normal offset ('{' : '-' : rest) = blockComment 1 (offset + 2) rest
  normal offset (char : rest)
    | char `elem` "[]," = (offset, char) : normal (offset + 1) rest
    | otherwise = normal (offset + 1) rest
  quoted _ [] = []
  quoted offset ('\\' : _ : rest) = quoted (offset + 2) rest
  quoted offset ('"' : rest) = normal (offset + 1) rest
  quoted offset (_ : rest) = quoted (offset + 1) rest
  lineComment _ [] = []
  lineComment offset ('\n' : rest) = normal (offset + 1) rest
  lineComment offset (_ : rest) = lineComment (offset + 1) rest
  blockComment :: Int -> Int -> String -> [(Int, Char)]
  blockComment _ _ [] = []
  blockComment depth offset ('{' : '-' : rest) = blockComment (depth + 1) (offset + 2) rest
  blockComment 1 offset ('-' : '}' : rest) = normal (offset + 2) rest
  blockComment depth offset ('-' : '}' : rest) = blockComment (depth - 1) (offset + 2) rest
  blockComment depth offset (_ : rest) = blockComment depth (offset + 1) rest

layoutDescription :: Int -> Int -> IndentPolicy -> String
layoutDescription columns indent policy = " at width " ++ show columns
  ++ ", indent " ++ show indent ++ ", " ++ show policy

policies :: [IndentPolicy]
policies = [IndentPolicyLeft, IndentPolicyMultiple, IndentPolicyFree]

listContexts :: [(String, String)]
listContexts =
  [ ("a standalone list", "")
  , ("a list left operand of append", " ++ extraScenarios")
  , ("a list left operand of another operator", " `append` extraScenarios")
  , ("a list before a flattened append chain",
      " ++ scenarioNames MicroSuite ++ scenarioNames ScalingSuite ++ [\"malformed-parse\"]")
  ]

scenarioSource :: String -> String
scenarioSource suffix = moduleSource
  [ "result choice = case choice of"
  , "  Selected ->"
  , "    [ \"first-selected-scenario\""
  , "    , \"second-selected-scenario\""
  , "    , \"third-selected-scenario\""
  , "    , \"fourth-selected-scenario\""
  , "    , \"fifth-selected-scenario\""
  , "    ]" ++ suffix
  ]

commentCases :: [(String, [String])]
commentCases =
  [ ("comments before a comma",
      [ "    [ firstScenario"
      , "    -- [,] Separator explanation."
      , "    , secondScenario"
      , "    , thirdScenario"
      ])
  , ("comments after a comma",
      [ "    [ firstScenario"
      , "    , -- [,] Next element explanation."
      , "      secondScenario"
      , "    , thirdScenario"
      ])
  , ("inline element comments",
      [ "    [ firstScenario -- First element."
      , "    , secondScenario -- Second element."
      , "    , thirdScenario"
      ])
  , ("block comments at a separator",
      [ "    [ firstScenario"
      , "    , {- [,] Next element explanation. -} secondScenario"
      , "    , thirdScenario"
      ])
  ]

malformedLists :: [(String, [String])]
malformedLists =
  [ ("a missing closing list bracket", ["result = [firstValue, secondValue"])
  , ("a missing element between commas", ["result = [firstValue,, secondValue]"])
  ]

moduleSource :: [String] -> String
moduleSource declarations = unlines $ ["module ListDelimiterAlignment where", ""] ++ declarations

formatChecked :: Config -> String -> IO String
formatChecked config source = parsePrintModule config (Text.pack source) >>= \case
  Left errors -> Hspec.expectationFailure
    ("formatting returned " ++ show (length errors) ++ " errors")
    >> fail "formatting failed"
  Right output -> pure $ Text.unpack output

assertStableAndEquivalent :: Config -> String -> String -> IO ()
assertStableAndEquivalent config original firstPass = do
  (inputAnns, inputParsed, ()) <- parseSource original
  (outputAnns, outputParsed, ()) <- parseSource firstPass
  compareSemanticSyntax inputParsed outputParsed `Hspec.shouldBe` Right Nothing
  case (normalizeCommentPlan inputAnns, normalizeCommentPlan outputAnns) of
    (Right inputPlan, Right outputPlan) ->
      commentPlanFingerprint outputPlan
        `Hspec.shouldBe` commentPlanFingerprint inputPlan
    _ -> Hspec.expectationFailure "input or output has an invalid comment plan"
  secondPass <- formatChecked config firstPass
  thirdPass <- formatChecked config secondPass
  secondPass `Hspec.shouldBe` firstPass
  thirdPass `Hspec.shouldBe` firstPass
 where
  parseSource source = do
    parsed <- ParseModule.parseModule ["-haddock"] "ListDelimiterAlignment.hs"
      (const $ pure $ Right ()) source
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError >> fail parseError
      Right result -> pure result

configWithLayout :: Int -> Int -> IndentPolicy -> Config
configWithLayout columns indent policy = staticDefaultConfig
  { _conf_layout = (_conf_layout staticDefaultConfig)
      { _lconfig_cols = Identity $ Last columns
      , _lconfig_indentAmount = Identity $ Last indent
      , _lconfig_indentPolicy = Identity $ Last policy
      }
  , _conf_errorHandling = (_conf_errorHandling staticDefaultConfig)
      { _econf_Werror = Identity $ Last True
      , _econf_failOnExactSourceFallback = Identity $ Last True
      }
  }
