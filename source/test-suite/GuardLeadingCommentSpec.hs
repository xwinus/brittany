{-# LANGUAGE LambdaCase #-}

module GuardLeadingCommentSpec (spec) where

import GuardLeadingCommentFixtures
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
spec projectRoot = Hspec.describe "guard-leading comment anchors" $ do
  Hspec.it "aligns the maintained symbolic-operator explanation with its guard" $ do
    source <- readFile $ projectRoot
      </> "source/library/Language/Haskell/Brittany/Internal/ExtractAnns.hs"
    output <- checkedSource 80 2 IndentPolicyFree source
    assertGuardOffset "-- Must not be a symbolic operator" "| null rest || not" 0 output

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent -> forM_ policies $ \policy ->
    Hspec.it ("moves the reported where-block comment with its guard" ++ describeLayout columns indent policy) $ do
      output <- checkedSource columns indent policy primarySource
      assertGuardOffset "-- explain the guard" "| x > 0" 0 output
      assertOrdered ["g x\n", "-- explain the guard", "| x > 0"] output
      assertWithin columns output

  forM_ edgeProfiles $ \(columns, indent, policy) -> forM_ edgeCases $ \(name, source, targets) ->
    Hspec.it ("preserves " ++ name ++ describeLayout columns indent policy) $ do
      output <- checkedSource columns indent policy source
      forM_ targets $ \(comment, guard, offset) -> do
        assertGuardOffset comment guard offset output
        assertOrdered [comment, guard] output
      case name of
        "blank runs" -> assertBlankBetween "-- first run" "-- second run" output
        "subsequent line" -> do
          assertGuardRow "| x > 0" ["|", "x", ">", "0", "=", "x"] output
          assertGuardRow "| otherwise" ["|", "otherwise", "=", "0"] output
        "let" -> assertGuardRow "| y > 0" ["|", "y", ">", "0", "=", "y"] output
        _ -> pure ()
      assertWithin columns output

  forM_ policies $ \policy ->
    Hspec.it ("keeps short guarded clauses on single rows, " ++ show policy) $ do
      output <- checkedSource 80 2 policy shortGuardSource
      comment <- uniqueLine "-- explain the guard" output
      output `Hspec.shouldContain` ("f x\n" ++ comment)
      assertGuardOffset "-- explain the guard" "| x > 0" 0 output
      assertGuardRow "| x > 0" ["|", "x", ">", "0", "=", "x"] output
      assertGuardRow "| otherwise" ["|", "otherwise", "=", "0"] output
      assertWithin 80 output

  forM_ edgeProfiles $ \(columns, indent, policy) ->
    Hspec.it ("preserves one blank line before a guard" ++ describeLayout columns indent policy) $ do
      output <- checkedSource columns indent policy blankBeforeGuardSource
      assertGuardOffset "-- explain the guard" "| x > 0" 0 output
      let between = takeWhile (not . List.isInfixOf "| x > 0")
            $ drop 1 $ dropWhile (not . List.isInfixOf "-- explain the guard") $ lines output
      length between `Hspec.shouldBe` 1
      between `Hspec.shouldSatisfy` all (null . words)
      assertGuardRow "| x > 0" ["|", "x", ">", "0", "=", "x"] output
      assertWithin columns output

  forM_ policies $ \policy -> forM_ suffixCases $ \(name, source, prefix, targets) ->
    Hspec.it ("keeps a separated guard suffix after " ++ name ++ ", " ++ show policy) $ do
      output <- checkedSource 80 2 policy source
      output `Hspec.shouldContain` prefix
      assertOrdered (prefix : map fst targets ++ ["| x > 0"]) output
      forM_ targets $ \(comment, offset) -> assertGuardOffset comment "| x > 0" offset output
      assertGuardRow "| x > 0" ["|", "x", ">", "0", "=", "x"] output
      assertWithin 80 output

  forM_ policies $ \policy -> do
    Hspec.it ("preserves unrelated inline trailing comments, " ++ show policy) $ do
      output <- checkedSource 80 2 policy trailingSource
      row <- uniqueLine "-- result trailing" output
      row `Hspec.shouldBe` "f x = x -- result trailing"
    Hspec.it ("preserves unrelated Haddock ownership, " ++ show policy) $ do
      output <- checkedSource 80 2 policy haddockSource
      row <- uniqueLine "-- | Function documentation." output
      row `Hspec.shouldBe` "-- | Function documentation."
      assertOrdered ["-- | Function documentation.", "f x", "| x > 0"] output
    Hspec.it ("preserves an unrelated source-relative diagram, " ++ show policy) $ do
      output <- checkedSource 80 2 policy diagramSource
      filter (List.isInfixOf "--") (lines output) `Hspec.shouldBe`
        [ "    -- Example:", "    --   +---+", "    --   | x |", "    --   +---+" ]

  forM_
    [ ["f x", "  -- missing predicate", "  | = x"]
    , ["f x", "  -- missing result", "  | x > 0 ="]
    ] $ \declarations ->
      Hspec.it ("rejects malformed guards without inplace replacement: " ++ show declarations) $ do
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
      Hspec.it ("detects guard comment " ++ name) $ do
        let source notes = moduleSource $ ["f x"] ++ map ("  " ++) notes
              ++ ["  | x > 0 = x", "  | otherwise = 0"]
        assertDifferentCommentGraphs (source ["-- first note", "-- second note"]) (source comments)

  Hspec.it "detects a guard-condition semantic mutation" $ do
    (_, before, ()) <- parsedSource $ moduleSource ["f x | x > 0 = x | otherwise = 0"]
    (_, after, ()) <- parsedSource $ moduleSource ["f x | x < 0 = x | otherwise = 0"]
    compareSemanticSyntax before after `Hspec.shouldSatisfy` \case
      Right (Just _) -> True
      _ -> False

  forM_ boundaryMutants $ \(name, original, changed) ->
    Hspec.it ("detects " ++ name) $
      assertDifferentCommentGraphs (moduleSource original) (moduleSource changed)

edgeProfiles :: [(Int, Int, IndentPolicy)]
edgeProfiles =
  [ (40, 2, IndentPolicyFree), (40, 4, IndentPolicyLeft)
  , (40, 4, IndentPolicyMultiple), (80, 2, IndentPolicyLeft)
  , (80, 4, IndentPolicyFree), (80, 2, IndentPolicyMultiple)
  ]

assertGuardRow :: String -> [String] -> String -> IO ()
assertGuardRow marker expected source = do
  row <- uniqueLine marker source
  words row `Hspec.shouldBe` expected

assertGuardOffset :: String -> String -> Int -> String -> IO ()
assertGuardOffset comment guard offset source = do
  commentRow <- uniqueLine comment source
  guardRow <- uniqueLine guard source
  markerColumn comment commentRow `Hspec.shouldBe` markerColumn guard guardRow + offset

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
  parsed <- ParseModule.parseModule ["-haddock"] "GuardLeadingComments.hs"
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
      (path, handle) <- IO.openTempFile directory "brittany-guard-leading-comment.hs"
      IO.hPutStr handle source
      IO.hClose handle
      pure path)
    Directory.removeFile
    action
