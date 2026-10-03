{-# LANGUAGE LambdaCase #-}

module StructuralCaseDoSpec (spec) where

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
import StructuralCaseDoFixtures
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "structural case do attachment" $ do
  Hspec.it "attaches do and lowers statement indentation in the reported constructor" $ do
    output <- checkedSource 40 2 IndentPolicyFree reducedSource
    lines output `Hspec.shouldBe` lines reducedExpected

  Hspec.it "attaches do to the maintained CommentIRSpec list pattern" $ do
    source <- readFile $ projectRoot </> "source/test-suite/CommentIRSpec.hs"
    output <- checkedModule (configWithLayout 80 2 IndentPolicyFree) source
    let occurrence = unlines $ takeWhile (not . List.isInfixOf "_ ->")
          $ dropWhile (not . List.isInfixOf "case unwrapBriDocNumbered lowered of") $ lines output
    closing <- uniqueLine "] ->" occurrence
    dropWhile (== ' ') closing `Hspec.shouldBe` "] -> do"
    statement <- uniqueLine "ownerKey' `Hspec.shouldBe` ownerKey" occurrence
    leadingSpaces statement `Hspec.shouldBe` leadingSpaces closing + 2
    filter ((> 80) . length) (lines occurrence) `Hspec.shouldBe` []

  forM_ [40, 80, 100] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ policies $ \policy -> do
      forM_ patternCases $ \(description, patternText) ->
        Hspec.it ("attaches a structural " ++ description ++ " head" ++ layoutDescription columns indent policy) $ do
          output <- checkedSource columns indent policy $ caseSource False patternText
          assertAttachedBody indent "firstAction" "secondAction" output
          alternative <- uniqueLine "_ -> fallback" output
          leadingSpaces alternative `Hspec.shouldBe` indent

      Hspec.it ("attaches a structural lambda-case head" ++ layoutDescription columns indent policy) $ do
        output <- checkedSource columns indent policy $ caseSource True $ snd $ head patternCases
        assertAttachedBody indent "firstAction" "secondAction" output

      Hspec.it ("preserves nested do and later case scopes" ++ layoutDescription columns indent policy) $ do
        output <- checkedSource columns indent policy $ nestedSource $ snd $ head patternCases
        headLine <- uniqueLine "-> do" output
        result <- uniqueLine "result <- do" output
        inner <- uniqueLine "innerAction" output
        later <- uniqueLine "afterInnerCase" output
        outer <- uniqueLine "afterOuterCase" output
        leadingSpaces result `Hspec.shouldBe` leadingSpaces headLine + indent
        leadingSpaces inner `Hspec.shouldBe` leadingSpaces result + indent
        leadingSpaces later `Hspec.shouldBe` leadingSpaces result
        leadingSpaces outer `Hspec.shouldBe` indent

      Hspec.it ("retains compact case and lambda-case heads" ++ layoutDescription columns indent policy) $ do
        output <- checkedSource columns indent policy $ moduleSource
          [ "match value = case value of"
          , "  Just selected -> do"
          , "    consume selected"
          , "    finish"
          , "  Nothing -> fallback"
          , "other = \\case"
          , "  Box item -> do"
          , "    consume item"
          , "    complete"
          ]
        output `Hspec.shouldContain` "Just selected -> do"
        output `Hspec.shouldContain` "Box item -> do"

      forM_ [0, 1] $ \extra ->
        Hspec.it ((if extra == 0 then "attaches an exact-fit keyword" else "detaches a one-column-over keyword")
            ++ layoutDescription columns indent policy) $ do
          output <- checkedSource columns indent policy $ boundarySource columns indent extra
          lastArgument <- uniqueLine (boundaryArgument columns indent extra) output
          if extra == 0 then do
            lastArgument `Hspec.shouldSatisfy` List.isSuffixOf " -> do"
            length lastArgument `Hspec.shouldBe` columns
          else do
            lastArgument `Hspec.shouldNotSatisfy` List.isInfixOf "do"
            keyword <- uniqueLine "do" output
            dropWhile (== ' ') keyword `Hspec.shouldBe` "do"

  forM_ [2, 4] $ \indent -> forM_ commentCases $ \(description, declarations) ->
    Hspec.it ("preserves " ++ description ++ " at indent " ++ show indent) $ do
      output <- checkedModule (configWithLayout 80 indent IndentPolicyFree) $ moduleSource $
        ["match value = case value of"] ++ declarations ++ ["  _ -> fallback"]
      forM_ ["firstAction", "secondAction"] $ \marker -> do
        _ <- uniqueLine marker output
        pure ()
      forM_ (filter (List.isInfixOf "--") $ lines output) $ \line ->
        case List.find (List.isPrefixOf "--") $ List.tails line of
          Just comment -> comment `Hspec.shouldNotSatisfy` List.isInfixOf " do"
          Nothing -> Hspec.expectationFailure "missing comment marker"

  forM_ [2, 4] $ \indent ->
    Hspec.it ("keeps infix do operands block-relative at indent " ++ show indent) $ do
      output <- checkedSource 80 indent IndentPolicyFree $ moduleSource
        [ "example = do"
        , "  initial `LongOperator.cleanup` do"
        , "    firstAction"
        , "    secondAction"
        , "  afterCleanup"
        ]
      first <- uniqueLine "firstAction" output
      second <- uniqueLine "secondAction" output
      later <- uniqueLine "afterCleanup" output
      leadingSpaces first `Hspec.shouldBe` 2 * indent
      leadingSpaces second `Hspec.shouldBe` 2 * indent
      leadingSpaces later `Hspec.shouldBe` indent

  Hspec.it "attaches do even when a later statement is indivisibly wider than the limit" $ do
    let longAction = "action" ++ replicate 50 'x'
    output <- checkedModule (configWithLayout 40 2 IndentPolicyFree) $ moduleSource
      [ "match value = case value of"
      , "  " ++ constructorPattern ++ " -> do"
      , "    " ++ longAction
      , "    finish"
      , "  _ -> fallback"
      ]
    assertAttachedBody 2 longAction "finish" output
    headLine <- uniqueLine "-> do" output
    length headLine `Hspec.shouldSatisfy` (<= 40)

  forM_ malformedCases $ \(description, declarations) ->
    Hspec.it ("rejects " ++ description ++ " without replacing inplace input") $ do
      let source = moduleSource declarations
      directory <- Directory.getTemporaryDirectory
      Exception.bracket
        (do
          (path, handle) <- IO.openTempFile directory "brittany-structural-do-invalid.hs"
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

assertAttachedBody :: Int -> String -> String -> String -> IO ()
assertAttachedBody indent firstMarker secondMarker output = do
  headLine <- uniqueLine "-> do" output
  first <- uniqueLine firstMarker output
  second <- uniqueLine secondMarker output
  leadingSpaces first `Hspec.shouldBe` leadingSpaces headLine + indent
  leadingSpaces second `Hspec.shouldBe` leadingSpaces first

uniqueLine :: String -> String -> IO String
uniqueLine marker output = case filter (List.isInfixOf marker) $ lines output of
  [line] -> pure line
  matches -> Hspec.expectationFailure ("expected one " ++ marker ++ " line, found " ++ show matches)
    >> fail "missing or repeated line"

leadingSpaces :: String -> Int
leadingSpaces = length . takeWhile (== ' ')

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

layoutDescription :: Int -> Int -> IndentPolicy -> String
layoutDescription columns indent policy = " at width " ++ show columns
  ++ ", indent " ++ show indent ++ ", " ++ show policy

policies :: [IndentPolicy]
policies = [IndentPolicyLeft, IndentPolicyMultiple, IndentPolicyFree]

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
    parsed <- ParseModule.parseModule ["-haddock"] "StructuralCaseBody.hs"
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
