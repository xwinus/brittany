{-# LANGUAGE LambdaCase #-}

module BindArrowCommentSpec (spec) where

import BindArrowCommentFixtures
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
spec projectRoot = Hspec.describe "bind arrow comment ownership" $ do
  Hspec.it "keeps the maintained Pattern explanation after its binding arrow" $ do
    source <- readFile $ projectRoot
      </> "source/library/Language/Haskell/Brittany/Internal/Layouters/Pattern.hs"
    output <- checkedSource 80 2 IndentPolicyFree source
    let section = unlines $ dropWhile (not . List.isInfixOf "xN' <-") $ lines output
    section `Hspec.shouldSatisfy` (not . null)
    assertOrdered (["xN' <-"] ++ maintainedComments ++ ["docAddBaseY"]) section
    assertAligned maintainedComments output

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ policies $ \policy -> forM_ [False, True] $ \inline ->
      Hspec.it ("keeps a post-arrow " ++ (if inline then "inline" else "own-line")
        ++ " run" ++ describeLayout columns indent policy) $ do
        output <- checkedSource columns indent policy $ runSource inline
        output `Hspec.shouldContain` "f = do\n"
        output `Hspec.shouldContain` "x <-"
        assertOrdered (["x <-"] ++ ordinaryComments ++ ["performAction", "pure x"]) output
        assertAligned ordinaryComments output
        commentRow <- uniqueLine (head ordinaryComments) output
        actionRow <- uniqueLine "performAction" output
        markerColumn "performAction" actionRow
          `Hspec.shouldBe` markerColumn (head ordinaryComments) commentRow
        if inline
          then do
            row <- uniqueLine (head ordinaryComments) output
            row `Hspec.shouldContain` "<-"
          else forM_ ordinaryComments $ \marker -> do
            row <- uniqueLine marker output
            dropWhile (== ' ') row `Hspec.shouldBe` marker
        assertWithin columns output

  forM_ policies $ \policy -> do
    forM_ edgeCases $ \(name, columns, source, sequenceMarkers) ->
      Hspec.it ("preserves " ++ name ++ describeLayout columns 2 policy) $ do
        output <- checkedSource columns 2 policy source
        assertOrdered sequenceMarkers output
        assertWithin columns output
        case name of
          "blank-separated post-arrow runs" ->
            assertBlankBetween "-- first run" "-- second run" output
          "a block comment before the arrow" -> do
            row <- uniqueLine "{- binder block -}" output
            row `Hspec.shouldContain` "x"
            row `Hspec.shouldContain` "<-"
            row `Hspec.shouldContain` "performAction"
          "a compact post-arrow block comment" -> do
            row <- uniqueLine "{- action note -}" output
            row `Hspec.shouldContain` "x <-"
            row `Hspec.shouldContain` "performAction"
          "an own-line block followed by an inline action" -> do
            row <- uniqueLine "{- action block -}" output
            row `Hspec.shouldContain` "performAction"
          "a post-doc marker after the arrow" -> do
            assertAligned ["-- ^ action documentation", "-- continuation"] output
            row <- uniqueLine "-- ^ action documentation" output
            row `Hspec.shouldContain` "<-"
          "a dedented run after a multiline pattern" -> do
            assertAligned ["-- action", "-- continuation"] output
            row <- uniqueLine "-- action" output
            action <- uniqueLine "performAction" output
            markerColumn "performAction" action
              `Hspec.shouldBe` markerColumn "-- action" row
            let binding = unlines $ takeWhile
                  (not . List.isInfixOf "pure firstValue") $ lines output
            first <- uniqueLine "firstValue" binding
            lastPattern <- uniqueLine "fourthValue" binding
            first `Hspec.shouldNotBe` lastPattern
          _ -> pure ()
    forM_ widthCases $ \(name, columns, indent, source) ->
      Hspec.it ("fits a post-arrow comment run after a " ++ name
        ++ describeLayout columns indent policy) $ do
        output <- checkedSource columns indent policy source
        assertOrdered ["<-", "-- action", "-- continuation", "action\n", "pure"] output
        assertAligned ["-- action", "-- continuation"] output
        assertWithin columns output
    Hspec.it ("wraps a long action after a pre-arrow block " ++ show policy) $ do
      output <- checkedSource 40 2 policy $ moduleSource
        [ "f = do"
        , "  x {- binder explanation -} <- longAction firstArgument secondArgument"
        , "  pure x"
        ]
      assertOrdered ["{- binder explanation -}", "<-", "longAction", "firstArgument"
        , "secondArgument", "pure x"] output
      assertWithin 40 output
    Hspec.it ("separates consecutive inline blocks on both arrow sides " ++ show policy) $ do
      output <- checkedSource 80 2 policy $ moduleSource
        [ "f = do"
        , "  x {- binder one -} {- binder two -} <- {- action one -} {- action two -} action"
        , "  pure x"
        ]
      output `Hspec.shouldContain` "{- binder one -} {- binder two -}"
      output `Hspec.shouldContain` "{- action one -} {- action two -}"
      assertOrdered ["{- binder one -}", "{- binder two -}", "<-"
        , "{- action one -}", "{- action two -}", "action\n", "pure x"] output
      assertWithin 80 output
    Hspec.it ("preserves protected relative indentation " ++ show policy) $ do
      output <- checkedSource 80 2 policy protectedSource
      let markers = ["-- Example:", "-- > first step", "-- > second step"]
      rows <- mapM (`uniqueLine` output) markers
      let positions = zipWith markerColumn markers rows
      map (subtract $ head positions) positions `Hspec.shouldBe` [0, 2, 2]
      assertOrdered (["<-"] ++ markers ++ ["performAction"]) output
      assertWithin 80 output

  forM_
    [ ["f = do", "  x <- -- missing action", "  pure x"]
    , ["f = [x | x <- -- missing generator", "  ]"]
    ] $ \declarations ->
      Hspec.it ("rejects malformed bindings without inplace replacement: " ++ show declarations) $ do
        let source = moduleSource declarations
        withSource source $ \path -> do
          Brittany.mainWith "brittany"
            [ "--no-user-config", "--write-mode", "inplace"
            , "--werror", "--fail-on-fallback", path
            ] `Hspec.shouldThrow` (== Exit.ExitFailure 60)
          TextIO.readFile path `Hspec.shouldReturn` Text.pack source

  forM_
    [ ("loss", ["-- first"])
    , ("duplication", ["-- first", "-- second", "-- second"])
    , ("reordering", ["-- second", "-- first"])
    ] $ \(name, comments) ->
      Hspec.it ("detects post-arrow comment " ++ name) $ do
        let source notes = moduleSource $
              ["f = do", "  x <-"] ++ map ("    " ++) notes
              ++ ["    performAction", "  pure x"]
        assertDifferentCommentGraphs (source ["-- first", "-- second"]) (source comments)

  forM_
    [ ("before the arrow", ["f = do", "  x -- boundary note", "    <- performAction", "  pure x"])
    , ("after the following statement", ["f = do", "  x <- performAction", "  pure x -- boundary note"])
    ] $ \(name, declarations) ->
      Hspec.it ("detects reassociation of a post-arrow comment " ++ name) $ do
        let original = moduleSource
              ["f = do", "  x <- -- boundary note", "    performAction", "  pure x"]
        assertDifferentCommentGraphs original $ moduleSource declarations

  Hspec.it "detects reassociation across adjacent generator arrows" $ do
    let original = moduleSource
          [ "f = [(x, y) | x <- -- generator boundary"
          , "  sourceValues, y <- otherValues]"
          ]
        mutated = moduleSource
          [ "f = [(x, y) | x <- sourceValues, y <- -- generator boundary"
          , "  otherValues]"
          ]
    assertDifferentCommentGraphs original mutated

  Hspec.it "detects a changed action despite identical post-arrow comments" $ do
    (_, before, ()) <- parsedSource $ runSource True
    let mutated = moduleSource
          ["f = do", "  x <- -- explain the action", "       -- continuation"
          , "       differentAction", "  pure x"]
    (_, after, ()) <- parsedSource mutated
    compareSemanticSyntax before after `Hspec.shouldNotBe` Right Nothing

assertOrdered :: [String] -> String -> IO ()
assertOrdered [] _ = pure ()
assertOrdered (marker : remaining) source =
  case List.findIndex (List.isPrefixOf marker) $ List.tails source of
    Nothing -> Hspec.expectationFailure $ "missing or misplaced marker: " ++ marker
    Just offset -> assertOrdered remaining $ drop (offset + length marker) source

assertAligned :: [String] -> String -> IO ()
assertAligned markers source = do
  rows <- mapM (`uniqueLine` source) markers
  let columns = zipWith markerColumn markers rows
  columns `Hspec.shouldSatisfy` all (== head columns)

uniqueLine :: String -> String -> IO String
uniqueLine marker source = case filter (List.isInfixOf marker) $ lines source of
  [row] -> pure row
  found -> Hspec.expectationFailure ("expected one " ++ marker ++ " row: " ++ show found)
    >> fail "missing or repeated marker"

markerColumn :: String -> String -> Int
markerColumn marker row = length $ takeWhile (not . List.isPrefixOf marker) $ List.tails row

assertBlankBetween :: String -> String -> String -> IO ()
assertBlankBetween first second source = do
  let between = takeWhile (not . List.isInfixOf second)
        $ drop 1 $ dropWhile (not . List.isInfixOf first) $ lines source
  between `Hspec.shouldSatisfy` any (null . words)

assertWithin :: Int -> String -> IO ()
assertWithin columns source = filter ((> columns) . length) (lines source) `Hspec.shouldBe` []

assertDifferentCommentGraphs :: String -> String -> IO ()
assertDifferentCommentGraphs original mutated = do
  (beforeAnns, beforeParsed, ()) <- parsedSource original
  (afterAnns, afterParsed, ()) <- parsedSource mutated
  compareSemanticSyntax beforeParsed afterParsed `Hspec.shouldBe` Right Nothing
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
  parsed <- ParseModule.parseModule ["-haddock"] "BindArrowComments.hs"
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
      (path, handle) <- IO.openTempFile directory "brittany-bind-arrow-comment.hs"
      IO.hPutStr handle source
      IO.hClose handle
      pure path)
    Directory.removeFile
    action
