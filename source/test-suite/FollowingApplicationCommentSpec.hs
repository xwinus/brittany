{-# LANGUAGE LambdaCase #-}

module FollowingApplicationCommentSpec (spec) where

import FollowingApplicationCommentFixtures
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
spec projectRoot = Hspec.describe "completed applications before comments" $ do
  Hspec.it "keeps the reported completed call and following run intact" $ do
    output <- checkedSource 80 2 IndentPolicyFree primarySource
    assertCompact "docNodeAnnKW lmod Nothing docEmpty" output
    assertOrdered (["docNodeAnnKW lmod Nothing docEmpty"] ++ reportedComments ++ [", next"]) output
    assertAligned reportedComments output
    assertWithin 80 output

  Hspec.it "keeps the maintained Module pseudo-node call compact" $ do
    source <- readFile $ projectRoot
      </> "source/library/Language/Haskell/Brittany/Internal/Layouters/Module.hs"
    output <- checkedSource 80 2 IndentPolicyFree source
    let section = unlines $ takeWhile (not . List.isInfixOf "docNodeMoveToKWDP lmod AnnModule")
          $ dropWhile (not . List.isInfixOf "let allowSingleLine") $ lines output
    assertCompact "docNodeAnnKW lmod Nothing docEmpty" section
    assertOrdered (["docNodeAnnKW lmod Nothing docEmpty"] ++ reportedComments) section
    assertAligned reportedComments section

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent -> forM_ policies $ \policy ->
    Hspec.it ("keeps a fitting list-element call compact" ++ describeLayout columns indent policy) $ do
      output <- checkedSource columns indent policy listSource
      assertCompact "build first second third" output
      assertOrdered ["build first second third", "-- after the call", "-- continuation", ", next"] output
      assertAligned ["-- after the call", "-- continuation"] output
      assertWithin columns output

  forM_ policies $ \policy -> forM_ contextCases $ \(name, source, markers) ->
    Hspec.it ("preserves a completed call in " ++ name ++ ", " ++ show policy) $ do
      output <- checkedSource 80 2 policy source
      assertCompact "build first second third" output
      assertOrdered markers output
      if name == "blank runs"
        then do
          assertBlankBetween "-- first run" "-- second run" output
          assertAligned ["-- first run", "-- second run"] output
        else pure ()
      assertWithin 80 output

  forM_ policies $ \policy -> forM_ protectedRuns $ \(inline, source, offsets) ->
    Hspec.it ("preserves protected following-run offsets with inline seed=" ++ show inline
      ++ ", " ++ show policy) $ do
      output <- checkedSource 80 2 policy source
      let markers = ["-- Example:", "-- > first step", "-- > nested step"]
      rows <- mapM (`uniqueLine` output) markers
      case zipWith markerColumn markers rows of
        first : rest -> map (subtract first) (first : rest) `Hspec.shouldBe` offsets
        [] -> Hspec.expectationFailure "missing protected comment run"
      seed <- uniqueLine "-- Example:" output
      if inline then seed `Hspec.shouldContain` "third -- Example:"
        else dropWhile (== ' ') seed `Hspec.shouldBe` "-- Example:"
      assertCompact "build first second third" output
      assertOrdered (["build first second third"] ++ markers ++ [", next"]) output
      assertWithin 80 output

  forM_ policies $ \policy ->
    Hspec.it ("aligns each nested closing parenthesis with its opener, " ++ show policy) $ do
      output <- checkedSource 80 2 policy nestedParenthesesSource
      assertCompact "combine first second third" output
      assertOrdered ["(", "(", "combine first second third", "-- after the call", ")", ")"] output
      let columnsOf character =
            [column | row <- lines output, (column, token) <- zip [0 :: Int ..] row, token == character]
          opening = columnsOf '('
          closing = columnsOf ')'
      length opening `Hspec.shouldBe` 2
      closing `Hspec.shouldBe` reverse opening
      assertWithin 80 output

  forM_ policies $ \policy ->
    forM_ protectedPrefixCases $ \(name, source, prefix, expectedColumns, suffix) ->
      Hspec.it ("preserves existing protected-run geometry for " ++ name ++ ", " ++ show policy) $ do
        output <- checkedSource 80 4 policy source
        rows <- mapM (`uniqueLine` output) prefix
        zipWith markerColumn prefix rows `Hspec.shouldBe` expectedColumns
        case suffix of
          [first, second] -> do
            assertBlankBetween (last prefix) first output
            firstRow <- uniqueLine first output
            secondRow <- uniqueLine second output
            markerColumn second secondRow - markerColumn first firstRow `Hspec.shouldBe` 2
          [] -> pure ()
          _ -> Hspec.expectationFailure "invalid protected-prefix fixture"
        assertCompact "combine first second third" output
        assertOrdered (["combine first second third"] ++ prefix ++ suffix ++ [", next"]) output
        assertWithin 80 output

  forM_ policies $ \policy ->
    forM_ recordGeometryCases $ \(name, indent, source, comments, nextField) ->
      Hspec.it ("retains record comment geometry in " ++ name ++ ", " ++ show policy) $ do
        output <- checkedSource 80 indent policy source
        assertCompact "combine first second third" output
        assertAligned (comments ++ [nextField]) output
        assertOrdered (["combine first second third"] ++ comments ++ [nextField]) output
        case comments of
          [first, second] -> assertBlankBetween first second output
          _ -> pure ()
        assertWithin 80 output

  forM_ profiles $ \(columns, indent, policy) -> do
    forM_ betweenCases $ \(name, source, markers) ->
      Hspec.it ("retains a comment after the " ++ name ++ " argument" ++ describeLayout columns indent policy) $ do
        output <- checkedSource columns indent policy source
        assertOrdered markers output
        assertWithin columns output
    forM_ [0, 1] $ \extra ->
      Hspec.it ("respects a completed call's width boundary +" ++ show extra ++ describeLayout columns indent policy) $ do
        output <- checkedSource columns indent policy $ boundarySource columns indent extra
        let call = boundaryCall columns indent extra
        if extra == 0
          then do
            row <- uniqueLine call output
            length row `Hspec.shouldBe` columns
          else output `Hspec.shouldNotContain` call
        let lastArgument = drop (length "build first second ") call
        assertOrdered ["build", "first", "second", lastArgument, "-- after the call", ", next"] output
        assertWithin columns output

  forM_ policies $ \policy ->
    Hspec.it ("wraps a genuinely oversized call before its comment, " ++ show policy) $ do
      output <- checkedSource 40 2 policy longSource
      output `Hspec.shouldNotContain` "build longFirstArgument longSecondArgument longThirdArgument"
      assertOrdered ["build", "longFirstArgument", "longSecondArgument", "longThirdArgument"
        , "-- after the call", ", next"] output
      assertWithin 40 output

  forM_
    [ ["f = [build first (second -- missing parenthesis", "    , next]"]
    , ["f = [build first second -- missing list close"]
    ] $ \declarations ->
      Hspec.it ("rejects malformed applications without inplace replacement: " ++ show declarations) $ do
        let source = moduleSource declarations
        withSource source $ \path -> do
          Brittany.mainWith "brittany"
            [ "--no-user-config", "--write-mode", "inplace"
            , "--werror", "--fail-on-fallback", path
            ] `Hspec.shouldThrow` (== Exit.ExitFailure 60)
          TextIO.readFile path `Hspec.shouldReturn` Text.pack source

  forM_
    [ ("loss", ["-- first note"])
    , ("duplication", ["-- first note", "-- second note", "-- second note"])
    , ("order", ["-- second note", "-- first note"])
    ] $ \(name, comments) ->
      Hspec.it ("detects following comment " ++ name) $ do
        let source notes = moduleSource $ ["f = [build first second third"]
              ++ map ("    " ++) notes ++ ["    , next]"]
        assertDifferentCommentGraphs (source ["-- first note", "-- second note"]) (source comments)

  forM_ ["build second first third", "build first (second third)"] $ \changed ->
    Hspec.it ("detects changed application structure: " ++ changed) $ do
      (_, before, ()) <- parsedSource $ moduleSource ["f = build first second third"]
      (_, after, ()) <- parsedSource $ moduleSource ["f = " ++ changed]
      compareSemanticSyntax before after `Hspec.shouldSatisfy` \case
        Right (Just _) -> True
        _ -> False

profiles :: [(Int, Int, IndentPolicy)]
profiles =
  [ (40, 2, IndentPolicyFree), (40, 4, IndentPolicyLeft)
  , (40, 4, IndentPolicyMultiple), (80, 2, IndentPolicyLeft)
  , (80, 4, IndentPolicyFree), (80, 2, IndentPolicyMultiple)
  ]

assertCompact :: String -> String -> IO ()
assertCompact call output = uniqueLine call output >> pure ()

assertAligned :: [String] -> String -> IO ()
assertAligned markers output = do
  rows <- mapM (`uniqueLine` output) markers
  case zipWith markerColumn markers rows of
    [] -> Hspec.expectationFailure "missing comment run"
    first : rest -> rest `Hspec.shouldBe` replicate (length rest) first

assertBlankBetween :: String -> String -> String -> IO ()
assertBlankBetween first second source = do
  let between = takeWhile (not . List.isInfixOf second)
        $ drop 1 $ dropWhile (not . List.isInfixOf first) $ lines source
  between `Hspec.shouldSatisfy` any (null . words)

assertOrdered :: [String] -> String -> IO ()
assertOrdered [] _ = pure ()
assertOrdered (marker : remaining) source =
  case List.findIndex (List.isPrefixOf marker) $ List.tails source of
    Nothing -> Hspec.expectationFailure $ "missing or misplaced marker: " ++ marker
    Just offset -> assertOrdered remaining $ drop (offset + length marker) source

uniqueLine :: String -> String -> IO String
uniqueLine marker source = case filter (List.isInfixOf marker) $ lines source of
  [row] -> pure row
  found -> Hspec.expectationFailure ("expected one " ++ marker ++ " row: " ++ show found)
    >> fail "missing or repeated marker"

markerColumn :: String -> String -> Int
markerColumn marker row = length $ takeWhile (not . List.isPrefixOf marker) $ List.tails row

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
  parsed <- ParseModule.parseModule ["-haddock"] "FollowingApplications.hs"
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
      (path, handle) <- IO.openTempFile directory "brittany-following-application-comment.hs"
      IO.hPutStr handle source
      IO.hClose handle
      pure path)
    Directory.removeFile
    action
