{-# LANGUAGE LambdaCase #-}

module CommentedInfixChainSpec (spec) where

import CommentedInfixChainFixtures
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
spec projectRoot = Hspec.describe "commented infix chain bases" $ do
  Hspec.it "aligns the reported composition and application operators" $ do
    output <- checkedSource 80 2 IndentPolicyFree compositionSource
    assertShared [". specialNub", ". filter hasOkColCount", ". preFilterLimit", "$ values"] output
    assertWithin 80 output

  Hspec.it "aligns the reported application operators inside a list" $ do
    output <- checkedSource 80 2 IndentPolicyFree applicationSource
    assertShared ["$ docAddBaseY", "$ docSeq"] output
    assertWithin 80 output

  Hspec.it "aligns the maintained Expr application chain" $ do
    source <- readFile $ projectRoot
      </> "source/library/Language/Haskell/Brittany/Internal/Layouters/Expr.hs"
    output <- checkedSource 80 2 IndentPolicyFree source
    let section = unlines $ takeWhile (not . List.isInfixOf "-- func")
          $ dropWhile (not . List.isInfixOf "-- this is most likely superfluous") $ lines output
    assertShared ["$ docAddBaseY", "$ docSeq"] section

  Hspec.it "aligns the maintained Alt composition chain" $ do
    source <- readFile $ projectRoot
      </> "source/library/Language/Haskell/Brittany/Internal/Transformations/Alt.hs"
    output <- checkedSource 80 2 IndentPolicyFree source
    let section = unlines $ takeWhile (not . List.isInfixOf "MaximumSpacingWidthBeforePruning")
          $ dropWhile (not . List.isInfixOf "-- prune so we always consider") $ lines output
    assertShared [". specialNub", ". filter hasOkColCount", ". preFilterLimit", "$ values"] section

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ policies $ \policy -> forM_ [False, True] $ \inline ->
      Hspec.it ("shares a continuation base around " ++ (if inline then "inline-seeded" else "own-line")
        ++ " comments" ++ describeLayout columns indent policy) $ do
        forM_ ["$", ".", "++"] $ \operator -> do
          output <- checkedSource columns indent policy $ chainSource operator inline
          assertShared (map ((operator ++ " ") ++) ["firstPart", "secondPart", "finalPart"]) output
          assertOrdered ["initialValue", "-- first note", "-- continuation", operator ++ " firstPart"
            , "-- middle note", operator ++ " secondPart", operator ++ " finalPart"] output
          assertWithin columns output

  forM_ policies $ \policy -> forM_ edgeCases $ \(name, source, groups) ->
    Hspec.it ("preserves " ++ name ++ ", " ++ show policy) $ do
      output <- checkedSource 80 2 policy source
      mapM_ (`assertShared` output) groups
      assertWithin 80 output
      case name of
        "blank-separated comment runs" -> do
          assertBlankBetween "-- first run" "-- second run" output
          assertOrdered ["firstPart", "-- first run", "-- second run", "++ secondPart"
            , "-- middle note", "++ thirdPart"] output
        "block comments between operators" ->
          assertOrdered ["firstPart", "{- first block -}", "++ secondPart"
            , "{- second block -}", "++ thirdPart"] output
        _ -> pure ()

  forM_ policies $ \policy -> forM_ blockEdgeCases $ \(name, marker, source) ->
    Hspec.it ("keeps " ++ name ++ " valid at indent 4, " ++ show policy) $ do
      output <- checkedSource 80 4 policy source
      assertShared [". secondFunction", marker] output
      assertOrdered ["firstFunction", "-- C1", ". secondFunction", marker, "-- C2"] output
      assertWithin 80 output

  forM_ policies $ \policy -> do
    Hspec.it ("aligns a homogeneous chain with two inline comments, " ++ show policy) $ do
      output <- checkedSource 80 2 policy inlineCompositionSource
      assertShared [". secondFunction", ". thirdFunction", ". fourthFunction"] output
      assertOrdered ["firstFunction", "-- C1", ". secondFunction", "-- C2", ". thirdFunction"] output
      assertWithin 80 output
    forM_ scopedHeaderCases $ \(name, header, markers, source) ->
      Hspec.it ("preserves a fitting header around " ++ name ++ ", " ++ show policy) $ do
        output <- checkedSource 80 2 policy source
        _ <- uniqueLine header output
        assertOrdered markers output
        assertWithin 80 output

  forM_ rhsProfiles $ \(columns, indent, policy) ->
    forM_ rhsBaseCases $ \(shape, finalOperator, source) ->
      Hspec.it ("keeps a commented " ++ shape ++ " RHS base" ++ describeLayout columns indent policy) $ do
        output <- checkedSource columns indent policy source
        _ <- uniqueLine "outer $ -- C1" output
        assertOrdered ["$ -- C1", finalOperator ++ " -- C2"] output
        assertRhsBase shape output
        assertWithin columns output

  forM_ policies $ \policy ->
    Hspec.it ("keeps an operator block comment before its next-line operand, " ++ show policy) $ do
      output <- checkedSource 80 4 policy operatorBlockSource
      assertOrdered ["$", "{- C1 -}", "[ buildResult", "| predicate", "]", "++", "-- C3"] output
      assertWithin 80 output

  forM_
    [ ["f = initialValue -- retained", "  $ (firstPart ."]
    , ["f = firstPart ++ -- retained", "  )"]
    ] $ \declarations ->
      Hspec.it ("rejects malformed commented chains without inplace replacement: " ++ show declarations) $ do
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
    , ("reordering", ["-- second note", "-- first note"])
    ] $ \(name, comments) ->
      Hspec.it ("detects infix comment " ++ name) $ do
        let source notes = moduleSource $
              ["f = firstPart"] ++ map ("    " ++) notes ++ ["  . secondPart . thirdPart"]
        assertDifferentCommentGraphs (source ["-- first note", "-- second note"]) (source comments)

  forM_
    [ ("parenthesis scope", "f = firstPart $ (secondPart $ thirdPart)", "f = (firstPart $ secondPart) $ thirdPart")
    , ("operator order", "f = firstPart . secondPart $ thirdPart", "f = firstPart $ secondPart . thirdPart")
    ] $ \(name, original, mutated) ->
      Hspec.it ("detects a mutation of " ++ name) $ do
        (_, before, ()) <- parsedSource $ moduleSource [original]
        (_, after, ()) <- parsedSource $ moduleSource [mutated]
        compareSemanticSyntax before after `Hspec.shouldSatisfy` \case
          Right (Just _) -> True
          _ -> False

assertShared :: [String] -> String -> IO ()
assertShared markers output = do
  rows <- mapM (`uniqueLine` output) markers
  let columns = zipWith markerColumn markers rows
  columns `Hspec.shouldSatisfy` (not . null)
  columns `Hspec.shouldBe` replicate (length columns) (head columns)

rhsProfiles :: [(Int, Int, IndentPolicy)]
rhsProfiles =
  [ (40, 2, IndentPolicyFree), (40, 4, IndentPolicyLeft)
  , (40, 4, IndentPolicyMultiple), (80, 2, IndentPolicyLeft)
  , (80, 4, IndentPolicyFree), (80, 2, IndentPolicyMultiple)
  ]

assertRhsBase :: String -> String -> IO ()
assertRhsBase shape output = do
  let rows = takeWhile (not . List.isInfixOf "-- C2") $ lines output
      rhs = unlines rows
      indentation = length . takeWhile (== ' ')
      assertClosing opening closing = do
        let matches = filter ((== [closing]) . dropWhile (== ' ')) rows
        case reverse matches of
          row : _ -> indentation row `Hspec.shouldBe` opening
          [] -> pure ()
      assertArguments = do
        callee <- uniqueLine "buildResult" rhs
        let calleeColumn = markerColumn "buildResult" callee
            arguments = filter (\row -> row /= callee && any (`List.isInfixOf` row)
              ["(Constructor", "firstArgument", "secondArgument"]) rows
        mapM_ (\row -> indentation row `Hspec.shouldSatisfy` (>= calleeColumn)) arguments
  case shape of
    "list comprehension" -> do
      opening <- uniqueLine "[ buildResult" rhs
      qualifier <- uniqueLine "| predicate" rhs
      let column = markerColumn "[ buildResult" opening
      markerColumn "| predicate" qualifier `Hspec.shouldBe` column
      assertClosing column ']'
      assertArguments
    "tuple" -> do
      opening <- uniqueLine "buildResult" rhs
      let column = markerColumn "(" opening
          separators = filter (List.isInfixOf ", otherValue") rows
      mapM_ (\row -> if row == opening then pure ()
        else markerColumn ", otherValue" row `Hspec.shouldBe` column) separators
      assertClosing column ')'
      assertArguments
    "application" -> assertArguments
    _ -> do
      let (marker, bodyMarkers) = if shape == "do block"
            then ("(do", ["firstAction", "secondAction"])
            else ("(case", ["Just x", "Nothing"])
      opening <- uniqueLine marker rhs
      body <- mapM (`uniqueLine` rhs) bodyMarkers
      let column = markerColumn marker opening
      mapM_ (\row -> indentation row `Hspec.shouldSatisfy` (> column)) body
      assertClosing column ')'

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

assertWithin :: Int -> String -> IO ()
assertWithin columns source = filter ((> columns) . length) (lines source) `Hspec.shouldBe` []

markerColumn :: String -> String -> Int
markerColumn marker row = length $ takeWhile (not . List.isPrefixOf marker) $ List.tails row

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
  parsed <- ParseModule.parseModule ["-haddock"] "CommentedInfixChains.hs"
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
      (path, handle) <- IO.openTempFile directory "brittany-commented-infix-chain.hs"
      IO.hPutStr handle source
      IO.hClose handle
      pure path)
    Directory.removeFile
    action
