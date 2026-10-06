{-# LANGUAGE LambdaCase #-}

module SourceFragmentBoundarySpec (spec) where

import qualified Control.Exception as Exception
import Control.Monad (forM_)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import Data.Semigroup (Last(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import SourceFragmentBoundaryFixtures
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
spec projectRoot = Hspec.describe "source-fragment line boundaries" $ do
  Hspec.it "keeps the complete Backend animousAct condition compact after its source comment" $ do
    source <- readFile $ projectRoot
      </> "source/library/Language/Haskell/Brittany/Internal/Backend.hs"
    output <- checkedSource 80 2 IndentPolicyFree source
    let binding = unlines $ takeWhile (not . List.isPrefixOf "case alignMode" . stripIndent)
          $ dropWhile (not . List.isPrefixOf ["animousAct"] . words) $ lines output
    binding `Hspec.shouldSatisfy` (not . null)
    assertFittingCondition 80 condition binding
    binding `Hspec.shouldContain` "-- trace (\"animousAct fixedPosXs=\""
    binding `Hspec.shouldContain` "-- per-item check if there is overflowing."

  forM_ [40, 80, 100] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ policies $ \policy -> do
      let context = layoutDescription columns indent policy
          predicate = if columns == 40 then narrowCondition else condition
      forM_ [("long", longComment), ("short", "-- Short note.")] $ \(name, comment) ->
        Hspec.it ("resets the expression cursor after a " ++ name ++ " separator comment" ++ context) $ do
          output <- checkedSource columns indent policy $ bindingSource comment predicate
          assertFittingCondition columns predicate output
          assertBeforeCondition comment output
      Hspec.it ("preserves a single-line block comment inside the condition" ++ context) $ do
        let predicateWithComment = "{- Condition note. -} " ++ predicate
        output <- checkedSource columns indent policy $ bindingSource "" predicateWithComment
        assertFittingCondition columns predicate output
        output `Hspec.shouldContain` "{- Condition note. -}"

  forM_ [2, 4] $ \indent -> forM_ policies $ \policy -> do
    let context = layoutDescription 80 indent policy
    forM_ nestedSources $ \(name, source) ->
      Hspec.it ("honors the rendered condition column in " ++ name ++ context) $ do
        output <- checkedSource 80 indent policy source
        assertFittingCondition 80 condition output
        assertBeforeCondition longComment output
    Hspec.it ("still wraps a genuinely over-width condition" ++ context) $ do
      output <- checkedSource 40 indent policy $ bindingSource longComment overwideCondition
      output `Hspec.shouldNotContain` ("if " ++ overwideCondition)
      filter (not . List.isInfixOf "--") (lines output)
        `Hspec.shouldSatisfy` all ((<= 40) . length)
      assertBeforeCondition longComment output

  forM_
    [ ( "a missing condition operand"
      , ["f = do", "  let result = " ++ longComment, "        if value + then yes else no", "  result"]
      )
    , ( "an unfinished commented branch"
      , ["f value = case value of", "  Just selected -> " ++ longComment, "    if selected then", "  Nothing -> fallback"]
      )
    ] $ \(name, declarations) ->
      Hspec.it ("rejects " ++ name ++ " without replacing inplace input") $ do
        let source = moduleSource declarations
        directory <- Directory.getTemporaryDirectory
        Exception.bracket
          (do
            (path, handle) <- IO.openTempFile directory "brittany-source-boundary-invalid.hs"
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

assertFittingCondition :: Int -> String -> String -> IO ()
assertFittingCondition columns predicate output = do
  line <- conditionLine output
  let prefix = length $ takeWhile (not . List.isPrefixOf "if ") $ List.tails line
  if prefix + length "if " + length predicate <= columns
    then line `Hspec.shouldContain` ("if " ++ predicate)
    else pure ()

assertBeforeCondition :: String -> String -> IO ()
assertBeforeCondition comment output = do
  line <- conditionLine output
  let preceding = takeWhile (/= line) $ lines output
  length (filter (List.isInfixOf comment) $ lines output) `Hspec.shouldBe` 1
  preceding `Hspec.shouldSatisfy` any (List.isInfixOf comment)

conditionLine :: String -> IO String
conditionLine output = case filter isCondition $ lines output of
  [line] -> pure line
  matches -> Hspec.expectationFailure ("expected one condition line, found " ++ show matches)
    >> fail "missing or repeated condition"
 where
  isCondition line = "if" `elem` words (takeCommentFree line)
  takeCommentFree = takeWhilePrefix
  takeWhilePrefix [] = []
  takeWhilePrefix ('-' : '-' : _) = []
  takeWhilePrefix (char : rest) = char : takeWhilePrefix rest

stripIndent :: String -> String
stripIndent = dropWhile (== ' ')

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
    parsed <- ParseModule.parseModule ["-haddock"] "SourceFragmentBoundary.hs"
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
