{-# LANGUAGE LambdaCase #-}

module LambdaCaseCommentSpec (spec) where

import qualified Control.Exception as Exception
import Control.Monad (forM_)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import Data.Semigroup (Last(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import LambdaCaseCommentFixtures
import Language.Haskell.Brittany
  ( CConfig(..), CErrorHandlingConfig(..), CLayoutConfig(..), Config
  , parsePrintModule, staticDefaultConfig
  )
import Language.Haskell.Brittany.Internal.CommentPlan
  ( commentPlanFingerprint, normalizeCommentPlan )
import Language.Haskell.Brittany.Internal.Config.Types (IndentPolicy(..))
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SemanticFingerprint (compareSemanticSyntax)
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "lambda-case comment alignment" $ do
  Hspec.it "aligns all four ordinary explanations in the complete Floating module" $ do
    source <- readFile $ projectRoot
      </> "source/library/Language/Haskell/Brittany/Internal/Transformations/Floating.hs"
    output <- checkedSource 80 2 IndentPolicyFree source
    let stepFull = unlines $ dropWhile
          (not . List.isPrefixOf ["stepFull"] . words) $ lines output
    stepFull `Hspec.shouldSatisfy` (not . null)
    forM_
      [ ("-- AddIndent floats into Lines.", "BDAddBaseY indent (BDLines lines)")
      , ("-- AddIndent floats into last column", "BDAddBaseY indent (BDCols sig cols)")
      , ("-- merge AddIndent and Par", "BDAddBaseY ind1 (BDPar ind2 line indented)")
      , ("-- prior floating in", "BDAnnotationPrior priorMode annKey1 (BDPar ind line indented)")
      ] $ \(comment, branch) -> assertAlignment comment branch stepFull

  forM_ [40, 80, 100] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ policies $ \policy -> do
      let context = layoutDescription columns indent policy
      forM_ ordinaryCases $ \(name, alternatives, alignments) ->
        Hspec.it ("aligns " ++ name ++ context) $ do
          output <- checkedSource columns indent policy $ shiftedSource alternatives
          forM_ alignments $ \(comment, branch) -> assertAlignment comment branch output
      Hspec.it ("preserves normal-case alignment" ++ context) $ do
        let source = moduleSource
              [ "outer value = stepFull"
              , " where"
              , "  stepFull = -- trace"
              , "             case value of"
              , "    -- First normal explanation."
              , "    True -> 1"
              , "    -- Later normal explanation."
              , "    False -> 0"
              ]
        output <- checkedSource columns indent policy source
        assertAlignment "-- First normal explanation." "True ->" output
        assertAlignment "-- Later normal explanation." "False ->" output
      Hspec.it ("preserves already aligned lambda-case comments" ++ context) $ do
        output <- checkedSource columns indent policy $ moduleSource
          [ "choose = \\case"
          , "  -- First aligned explanation."
          , "  True -> 1"
          , "  -- Later aligned explanation."
          , "  False -> 0"
          ]
        assertAlignment "-- First aligned explanation." "True ->" output
        assertAlignment "-- Later aligned explanation." "False ->" output

  forM_ [2, 4] $ \indent -> forM_ policies $ \policy -> do
    let context = layoutDescription 80 indent policy
    forM_ protectedRuns $ \(name, comments) ->
      Hspec.it ("preserves " ++ name ++ context) $ do
        let source = shiftedSource $ ["    True -> 1"] ++ comments ++ ["    False -> 0"]
            sourceComments = filter (not . null) comments
        output <- checkedSource 80 indent policy source
        let outputComments = standaloneComments output
        map stripIndent outputComments `Hspec.shouldBe` map stripIndent sourceComments
        assertCommentOwnership "True ->" "False ->" outputComments output
        if name == "separated ordinary and protected runs"
          then assertAlignment "-- Later ordinary explanation." "False ->" output
          else do
            relativeColumns outputComments `Hspec.shouldBe` relativeColumns sourceComments
            outputComments `Hspec.shouldBe` sourceComments
    forM_ (filter (\(name, _) -> name `notElem`
      ["deliberately indented prose"]) protectedRuns)
      $ \(name, comments) ->
        Hspec.it ("preserves the established first-alternative layout for " ++ name ++ context) $ do
          output <- checkedSource 80 indent policy $ shiftedSource $
            comments ++ ["    True -> 1", "    False -> 0"]
          let outputComments = standaloneComments output
          map stripIndent outputComments `Hspec.shouldBe` map stripIndent (filter (not . null) comments)
          branch <- uniqueLine "True ->" output
          forM_ outputComments $ \comment -> do
            indentation comment `Hspec.shouldBe` indentation branch
            assertAlignment (stripIndent comment) "True ->" output
    Hspec.it ("preserves inline-seeded continuations" ++ context) $ do
      output <- checkedSource 80 indent policy $ shiftedSource
        [ "    True -> 1 -- seed note"
        , "              -- continuation note"
        , "    False -> 0"
        ]
      seed <- uniqueLine "-- seed note" output
      continuation <- uniqueLine "-- continuation note" output
      markerColumn "-- seed note" seed
        `Hspec.shouldBe` markerColumn "-- continuation note" continuation
      assertCommentOwnership "True ->" "False ->" [continuation] output

  forM_
    [ ("a missing first body", ["choose = \\case", "  -- Explanation.", "  True ->"])
    , ("a malformed later pattern", ["choose = \\case", "  True -> 1", "  -- Explanation.", "  (False, -> 0"])
    ] $ \(name, declarations) ->
      Hspec.it ("rejects " ++ name ++ " without replacing inplace input") $ do
        let source = moduleSource declarations
        directory <- Directory.getTemporaryDirectory
        Exception.bracket
          (do
            (path, handle) <- IO.openTempFile directory "brittany-lambda-case-invalid.hs"
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

assertAlignment :: String -> String -> String -> IO ()
assertAlignment commentMarker branchMarker output = do
  comment <- uniqueLine commentMarker output
  let following = drop 1 $ dropWhile (/= comment) $ lines output
  case filter (List.isInfixOf branchMarker) following of
    branch : _ -> indentation comment `Hspec.shouldBe` indentation branch
    [] -> Hspec.expectationFailure $ "missing following branch: " ++ branchMarker

assertCommentOwnership :: String -> String -> [String] -> String -> IO ()
assertCommentOwnership before after comments output = do
  beforeLine <- uniqueLine before output
  afterLine <- uniqueLine after output
  let positions = zip (lines output) [0 :: Int ..]
      position line = lookup line positions
  forM_ comments $ \comment -> do
    position comment `Hspec.shouldSatisfy` (> position beforeLine)
    position comment `Hspec.shouldSatisfy` (< position afterLine)

uniqueLine :: String -> String -> IO String
uniqueLine marker output = case filter (List.isInfixOf marker) $ lines output of
  [line] -> pure line
  matches -> Hspec.expectationFailure
    ("expected one line containing " ++ show marker ++ ", found " ++ show matches)
    >> fail "missing or repeated marker"

stripIndent :: String -> String
stripIndent = dropWhile (== ' ')

indentation :: String -> Int
indentation = length . takeWhile (== ' ')

standaloneComments :: String -> [String]
standaloneComments = filter (List.isPrefixOf "--" . stripIndent) . lines

relativeColumns :: [String] -> [Int]
relativeColumns [] = []
relativeColumns comments@(first : _) = map ((subtract $ indentation first) . indentation) comments

markerColumn :: String -> String -> Int
markerColumn marker line = length $ takeWhile (not . List.isPrefixOf marker) $ List.tails line

policies :: [IndentPolicy]
policies = [IndentPolicyLeft, IndentPolicyMultiple, IndentPolicyFree]

layoutDescription :: Int -> Int -> IndentPolicy -> String
layoutDescription columns indent policy = " at width " ++ show columns
  ++ ", indent " ++ show indent ++ ", " ++ show policy

checkedSource :: Int -> Int -> IndentPolicy -> String -> IO String
checkedSource columns indent policy original = do
  let config = configWithLayout columns indent policy
  first <- formatChecked config original
  (inputAnns, inputParsed, ()) <- parseSource original
  (outputAnns, outputParsed, ()) <- parseSource first
  compareSemanticSyntax inputParsed outputParsed `Hspec.shouldBe` Right Nothing
  case (normalizeCommentPlan inputAnns, normalizeCommentPlan outputAnns) of
    (Right inputPlan, Right outputPlan) ->
      commentPlanFingerprint outputPlan `Hspec.shouldBe` commentPlanFingerprint inputPlan
    _ -> Hspec.expectationFailure "input or output has an invalid comment plan"
  second <- formatChecked config first
  third <- formatChecked config second
  second `Hspec.shouldBe` first
  third `Hspec.shouldBe` first
  pure first
 where
  parseSource source = do
    parsed <- ParseModule.parseModule ["-haddock"] "LambdaCaseComments.hs"
      (const $ pure $ Right ()) source
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError >> fail parseError
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
