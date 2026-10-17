{-# LANGUAGE LambdaCase #-}

module TupleOpeningColumnSpec (spec) where

import TupleOpeningColumnFixtures
import qualified Control.Exception as Exception
import Control.Monad (forM_)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import Data.Semigroup (Last(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified GHC
import Language.Haskell.Brittany
  ( CConfig(..), CErrorHandlingConfig(..), CLayoutConfig(..), Config
  , parsePrintModule, staticDefaultConfig
  )
import Language.Haskell.Brittany.Internal.CommentBoundary (canonicalCommentGraph)
import Language.Haskell.Brittany.Internal.CommentPlan
  ( commentPlanFingerprint, normalizeCommentPlan )
import Language.Haskell.Brittany.Internal.Config.Types (IndentPolicy(..))
import Language.Haskell.Brittany.Internal.ExactPrintCompat (Anns)
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SemanticFingerprint (compareSemanticSyntax)
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "tuple opening columns" $ do
  Hspec.it "aligns the reported comparison tuple at its rendered opener" $ do
    output <- checkedSource 80 2 IndentPolicyFree primarySource
    assertGroup "srcSpanEndLine" 0 2 output
    assertWithin 80 output

  Hspec.it "aligns the maintained pattern-comment comparison" $ do
    source <- readFile $ projectRoot
      </> "source/library/Language/Haskell/Brittany/Internal/Layouters/Pattern/Comments.hs"
    output <- checkedSource 80 2 IndentPolicyFree source
    let section = unlines $ takeWhile (not . List.isPrefixOf "patternSourceCommentWithinNodeSpan")
          $ dropWhile (not . List.isPrefixOf "patternSourceCommentPrecedesNode node") $ lines output
    assertGroup "srcSpanEndLine" 0 2 section

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent -> forM_ policies $ \policy -> do
    Hspec.it ("aligns tuple operands of surrounding operators" ++ describeLayout columns indent policy) $ do
      forM_ ["<=", "==", "$", "`Custom.combine`", ":?="] $ \operator -> do
        output <- checkedSource columns indent policy $ standardSource "(" ")" $ " " ++ operator ++ " referenceValue"
        assertGroup "firstTransform" 0 3 output
        assertWithin columns output
    Hspec.it ("retains standalone tuple and list controls" ++ describeLayout columns indent policy) $ do
      forM_ [("(", ")", ""), ("[", "]", " ++ remainingValues")] $ \(open, close, suffix) -> do
        output <- checkedSource columns indent policy $ standardSource open close suffix
        assertGroup "firstTransform" 0 3 output
        assertWithin columns output
      compact <- checkedSource columns indent policy $ moduleSource ["small = (first, second)", "empty = ()"]
      compact `Hspec.shouldContain` "small = (first, second)"
      compact `Hspec.shouldContain` "empty = ()"

  forM_ edgeProfiles $ \(columns, indent, policy) -> forM_ edgeCases $ \(name, source, groups) ->
    Hspec.it ("aligns " ++ name ++ describeLayout columns indent policy) $ do
      output <- checkedSource columns indent policy source
      forM_ groups $ \(marker, ancestor, elements) -> assertGroup marker ancestor elements output
      assertWithin columns output

  forM_ policies $ \policy -> forM_ ["", " `combine` otherValue"] $ \suffix ->
    Hspec.it ("keeps unboxed tuple arguments under their callees, " ++ show policy ++ suffix) $ do
      output <- checkedSource 40 2 policy $ unboxedApplicationSource suffix
      assertGroup "longFirstFunction" 0 2 output
      assertApplicationIndent "longFirstFunction" ["firstArgument", "secondArgument"] output
      assertApplicationIndent "longSecondFunction" ["thirdArgument", "fourthArgument"] output
      assertWithin 40 output

  forM_ policies $ \policy ->
    Hspec.it ("retains the unboxed tuple comment base, " ++ show policy) $ do
      let source = moduleSource
            [ "f =", "  (# right", "    -- Keep the tuple expression comment."
            , "  , left", "  #)"
            ]
      output <- checkedSource 80 2 policy source
      assertGroup "right" 0 2 output
      filter (List.isInfixOf "Keep the tuple expression comment.") (lines output)
        `Hspec.shouldBe` ["    -- Keep the tuple expression comment."]
      assertWithin 80 output

  forM_ ["f = (first, second", "f = (# first, second )"] $ \declaration ->
    Hspec.it ("rejects malformed tuples without inplace replacement: " ++ declaration) $ do
      let source = moduleSource [declaration]
      withSource source $ \path -> do
        Brittany.mainWith "brittany"
          [ "--no-user-config", "--write-mode", "inplace"
          , "--werror", "--fail-on-fallback", path
          ] `Hspec.shouldThrow` (== Exit.ExitFailure 60)
        TextIO.readFile path `Hspec.shouldReturn` Text.pack source

  forM_ [("delimiter identity", "[first, second]"), ("element order", "(second, first)")] $ \(name, expression) ->
    Hspec.it ("detects changed tuple " ++ name) $ do
      (_, before, ()) <- parsedSource $ moduleSource ["f = (first, second)"]
      (_, after, ()) <- parsedSource $ moduleSource ["f = " ++ expression]
      compareSemanticSyntax before after `Hspec.shouldSatisfy` \case
        Right (Just _) -> True
        _ -> False

  forM_ [("loss", ["-- first note"]), ("order", ["-- second note", "-- first note"])] $ \(name, comments) ->
    Hspec.it ("detects tuple comment " ++ name) $ do
      let source notes = moduleSource $ ["f = ( first"] ++ map ("    " ++) notes ++ ["    , second", "    )"]
      assertDifferentCommentGraphs (source ["-- first note", "-- second note"]) (source comments)

assertApplicationIndent :: String -> [String] -> String -> IO ()
assertApplicationIndent function arguments source =
  case filter (List.isInfixOf function) $ lines source of
    [callee] -> do
      let column = length $ takeWhile (not . List.isPrefixOf function) $ List.tails callee
      forM_ arguments $ \argument ->
        case filter (List.isInfixOf argument) $ lines source of
          [row] | row == callee -> pure ()
                | otherwise -> length (takeWhile (== ' ') row) `Hspec.shouldSatisfy` (>= column)
          _ -> Hspec.expectationFailure $ "missing or repeated argument " ++ argument
    _ -> Hspec.expectationFailure $ "missing or repeated callee " ++ function

edgeProfiles :: [(Int, Int, IndentPolicy)]
edgeProfiles =
  [ (40, 2, IndentPolicyFree), (40, 4, IndentPolicyLeft)
  , (40, 4, IndentPolicyMultiple), (80, 2, IndentPolicyLeft)
  , (80, 4, IndentPolicyFree), (80, 2, IndentPolicyMultiple)
  ]

-- Select the smallest enclosing group, or one of its ancestors, and compare
-- only leading rows: compact tuple sections may share a row with their opener.
assertGroup :: String -> Int -> Int -> String -> IO ()
assertGroup marker ancestor elements source = do
  markerOffset <- case List.findIndex (List.isPrefixOf marker) $ List.tails source of
    Nothing -> Hspec.expectationFailure ("missing " ++ marker) >> fail "missing marker"
    Just offset -> pure offset
  let tokens = punctuation source
      candidates = reverse
        [ opening : selectDelimiters 1 (dropWhile ((<= opening) . fst) tokens)
        | (opening, symbol) <- tokens
        , symbol `elem` "([", opening < markerOffset
        , let delimiters = selectDelimiters 1 $ dropWhile ((<= opening) . fst) tokens
        , not (null delimiters), last delimiters > markerOffset
        ]
  case drop ancestor candidates of
    delimiters@(opening : _) : _ -> do
      length delimiters `Hspec.shouldBe` elements + 1
      let rowPrefix offset = takeWhile (/= '\n') $ reverse $ take offset source
          column = length . rowPrefix
          leading offset = all (== ' ') $ rowPrefix offset
      map column (filter leading $ tail delimiters)
        `Hspec.shouldBe` replicate (length $ filter leading $ tail delimiters) (column opening)
    _ -> Hspec.expectationFailure "missing enclosing delimiter group"

selectDelimiters :: Int -> [(Int, Char)] -> [Int]
selectDelimiters _ [] = []
selectDelimiters depth ((offset, symbol) : rest)
  | symbol `elem` "([" = selectDelimiters (depth + 1) rest
  | symbol `elem` ")]" = if depth == 1 then [offset] else selectDelimiters (depth - 1) rest
  | symbol == ',' && depth == 1 = offset : selectDelimiters depth rest
  | otherwise = selectDelimiters depth rest

-- Delimiter-like punctuation in comments and strings must not affect the check.
-- For unboxed tuples the closing token starts at # rather than its final ).
punctuation :: String -> [(Int, Char)]
punctuation = normal 0
 where
  normal _ [] = []
  normal offset ('"' : rest) = quoted (offset + 1) rest
  normal offset ('-' : '-' : rest) = lineComment (offset + 2) rest
  normal offset ('{' : '-' : rest) = blockComment 1 (offset + 2) rest
  normal offset ('#' : ')' : rest) = (offset, ')') : normal (offset + 2) rest
  normal offset (char : rest)
    | char `elem` "([])," = (offset, char) : normal (offset + 1) rest
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

assertWithin :: Int -> String -> IO ()
assertWithin columns source = filter ((> columns) . length) (lines source) `Hspec.shouldBe` []

assertDifferentCommentGraphs :: String -> String -> IO ()
assertDifferentCommentGraphs original mutated = do
  before@(_, beforeParsed, ()) <- parsedSource original
  after@(_, afterParsed, ()) <- parsedSource mutated
  compareSemanticSyntax beforeParsed afterParsed `Hspec.shouldBe` Right Nothing
  assertChangedGraph before after

assertChangedGraph
  :: (Anns, GHC.ParsedSource, ()) -> (Anns, GHC.ParsedSource, ()) -> IO ()
assertChangedGraph (beforeAnns, beforeParsed, ()) (afterAnns, afterParsed, ()) =
  case (normalizeCommentPlan beforeAnns, normalizeCommentPlan afterAnns) of
    (Right before, Right after) ->
      canonicalCommentGraph afterParsed after
        `Hspec.shouldNotBe` canonicalCommentGraph beforeParsed before
    _ -> Hspec.expectationFailure "invalid plan in comment mutation control"

checkedSource :: Int -> Int -> IndentPolicy -> String -> IO String
checkedSource columns indent policy source = do
  let config = configWithLayout columns indent policy
  first <- formatChecked config source
  (inputAnns, inputParsed, ()) <- parsedSource source
  (outputAnns, outputParsed, ()) <- parsedSource first
  compareSemanticSyntax inputParsed outputParsed `Hspec.shouldBe` Right Nothing
  case (normalizeCommentPlan inputAnns, normalizeCommentPlan outputAnns) of
    (Right inputPlan, Right outputPlan) -> do
      commentPlanFingerprint outputPlan `Hspec.shouldBe` commentPlanFingerprint inputPlan
      canonicalCommentGraph outputParsed outputPlan
        `Hspec.shouldBe` canonicalCommentGraph inputParsed inputPlan
    _ -> Hspec.expectationFailure "input or output has an invalid comment plan"
  second <- formatChecked config first
  third <- formatChecked config second
  second `Hspec.shouldBe` first
  third `Hspec.shouldBe` first
  pure first

parsedSource :: String -> IO (Anns, GHC.ParsedSource, ())
parsedSource source = do
  parsed <- ParseModule.parseModule ["-haddock"] "TupleOpeningColumn.hs"
    (const $ pure $ Right ()) source
  case parsed of
    Left failure -> Hspec.expectationFailure failure >> fail failure
    Right result -> pure result

formatChecked :: Config -> String -> IO String
formatChecked config source = parsePrintModule config (Text.pack source) >>= \case
  Left errors -> Hspec.expectationFailure
    ("formatting returned " ++ show (length errors) ++ " errors") >> fail "formatting failed"
  Right output -> pure $ Text.unpack output

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

policies :: [IndentPolicy]
policies = [IndentPolicyLeft, IndentPolicyMultiple, IndentPolicyFree]

describeLayout :: Int -> Int -> IndentPolicy -> String
describeLayout columns indent policy =
  " at width " ++ show columns ++ ", indent " ++ show indent ++ ", " ++ show policy

withSource :: String -> (FilePath -> IO a) -> IO a
withSource source action = do
  directory <- Directory.getTemporaryDirectory
  Exception.bracket
    (do
      (path, handle) <- IO.openTempFile directory "brittany-tuple-opening-column.hs"
      IO.hPutStr handle source
      IO.hClose handle
      pure path)
    Directory.removeFile
    action
