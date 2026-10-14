{-# LANGUAGE LambdaCase #-}

module CohesiveOperatorChainSpec (spec) where

import qualified Control.Exception as Exception
import Control.Monad (forM_)
import CohesiveOperatorChainFixtures
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
import Language.Haskell.Brittany.Internal.CommentPlan
  ( commentPlanFingerprint, normalizeCommentPlan )
import Language.Haskell.Brittany.Internal.Config.Types (IndentPolicy(..))
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.ExactPrintCompat (Anns)
import Language.Haskell.Brittany.Internal.SemanticFingerprint (compareSemanticSyntax)
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Timeout as Timeout
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "cohesive mixed operator chains" $ do
  forM_ maintainedCases $ \(name, path, units) ->
    Hspec.it ("keeps all audited groups cohesive in " ++ name) $ do
      source <- readFile $ projectRoot </> path
      output <- checkedSource 80 2 IndentPolicyFree source
      forM_ units $ \unit -> assertCohesive unit output
      if "SemanticFingerprint.Ghc" `List.isInfixOf` name
        then assertBooleanTier "all ignoredRepresentation typeArguments" output
        else pure ()

  forM_ [40, 60, 80, 100] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ policies $ \policy -> do
      let context = layoutDescription columns indent policy
      forM_ cohesiveCases $ \(name, declarations, units) ->
        Hspec.it ("keeps fitting " ++ name ++ context) $ do
          output <- checkedSource columns indent policy $ moduleSource declarations
          forM_ units $ \unit -> assertCohesive unit output
          assertWithinColumns columns output
      Hspec.it ("shows conjunction nesting within disjunction" ++ context) $ do
        output <- checkedSource columns indent policy $ booleanSource columns
        assertBooleanTier "all allowed types" output
        assertWithinColumns columns output

  forM_ [2, 4] $ \indent -> forM_ policies $ \policy -> do
    let context = layoutDescription 80 indent policy
    forM_ ["<~>", "`contains`", "`List.elem`", "Q.<+>", ".:?=", "++"] $ \operator ->
      Hspec.it ("preserves opaque operator tokens without discarding neighboring known groups for "
        ++ operator ++ context) $ do
        output <- checkedSource 80 indent policy $ moduleSource
          [ "result = name " ++ operator ++ " matrixCaseFeatures matrixCase"
            ++ " && matrixCaseExpectedResult matrixCase == Formats"
          ]
        output `Hspec.shouldContain` operator
        assertCohesive "matrixCaseExpectedResult matrixCase == Formats" output
        assertWithinColumns 80 output
    forM_ [0, 1] $ \excess ->
      Hspec.it ("reserves the outer operator at an exact arithmetic boundary plus "
        ++ show excess ++ context) $ do
        let suffix = " + finalValue"
            subject = 'x' : replicate (80 - 2 * indent - 2 - length suffix - 1 + excess) 'x'
            unit = subject ++ suffix
        output <- checkedSource 80 indent policy $ moduleSource
          ["result = processConfiguration selectedConfiguration $ " ++ unit]
        if excess == 0
          then map length (filter (List.isInfixOf $ "$ " ++ unit) $ lines output)
            `Hspec.shouldBe` [80]
          else output `Hspec.shouldNotContain` ("$ " ++ unit)
        assertWithinColumns 80 output

  forM_ [2, 4] $ \indent -> forM_ policies $ \policy ->
    forM_ ["Custom.%%%", "Prelude.=="] $ \operator ->
      Hspec.it ("avoids extra indentation within an opaque qualified group after "
        ++ operator ++ layoutDescription 40 indent policy) $ do
        output <- checkedSource 40 indent policy $ moduleSource
          [ "result a b c = a == expected && b " ++ operator
            ++ " longUnknownOperand && c == other"
          ]
        output `Hspec.shouldContain` operator
        forM_ ["a == expected", "c == other"] $ \unit -> assertCohesive unit output
        assertWithinColumns 40 output

  forM_ [2, 4] $ \indent -> forM_ commentCases $ \(name, declarations, comments) ->
    Hspec.it ("preserves " ++ name ++ " at indent " ++ show indent) $ do
      output <- checkedSource 60 indent IndentPolicyFree $ moduleSource declarations
      forM_ comments $ \comment ->
        length (filter (List.isInfixOf comment) $ lines output) `Hspec.shouldBe` 1
      forM_ ["(a == A)", "(b /= B)", "(c == C)"] $
        \unit -> assertCohesive unit output
      assertWithinColumns 60 output

  forM_ policies $ \policy -> do
    forM_ shadowedCases $ \(name, declarations) ->
      Hspec.it ("preserves parsed semantics for " ++ name ++ show policy) $ do
        output <- checkedSource 60 2 policy $ moduleSource declarations
        assertWithinColumns 60 output
    Hspec.it ("preserves nested explicit grouping " ++ show policy) $ do
      output <- checkedSource 60 2 policy $ moduleSource
        [ "result = processConfiguration selectedConfiguration"
        , "  $ ((first + second) * (third - fourth)) + finalValue"
        ]
      forM_ ["(first + second)", "(third - fourth)"] $ \unit -> assertCohesive unit output
      assertWithinColumns 60 output
    Hspec.it ("keeps fitting complete chains compact " ++ show policy) $ do
      output <- checkedSource 80 2 policy $ moduleSource
        [ "arithmetic = 1 + 2 * 3"
        , "booleans = a || b && c"
        , "alternatives = f <$> x <|> g <$> y"
        ]
      forM_ ["arithmetic = 1 + 2 * 3", "booleans = a || b && c", "alternatives = f <$> x <|> g <$> y"] $
        \unit -> assertCohesive unit output

  forM_ policies $ \policy -> do
    Hspec.it ("keeps a fitting function application intact before an oversized literal " ++ show policy) $ do
      let literal = show $ replicate 100 'x'
      output <- checkedSource 40 4 policy $ moduleSource
        [ "f x a b = firstFunction x $ firstPredicate a == expectedValue"
          ++ " && b == " ++ literal
        ]
      map (dropWhile (== ' ')) (lines output) `Hspec.shouldContain` ["firstFunction x"]
      case filter (List.isInfixOf literal) $ lines output of
        [row] -> do
          length (takeWhile (== ' ') row) `Hspec.shouldSatisfy` (<= 16)
          length row `Hspec.shouldSatisfy` (<= 121)
        _ -> Hspec.expectationFailure "expected exactly one complete oversized literal"
      filter (\row -> length row > 40 && not (literal `List.isInfixOf` row))
        (lines output) `Hspec.shouldBe` []
    Hspec.it ("does not add avoidable indentation inside a final case operand " ++ show policy) $ do
      output <- checkedSource 40 4 policy $ moduleSource
        [ "f x a b = firstFunction x $ firstPredicate a == expectedValue && case b of"
        , "  Just y -> veryLongPredicate y == expectedResult"
        , "  Nothing -> False"
        ]
      map (dropWhile (== ' ')) (lines output) `Hspec.shouldContain` ["firstFunction x"]
      forM_ ["firstPredicate a", "expectedValue", "veryLongPredicate y", "expectedResult"] $
        \unit -> assertCohesive unit output
      assertWithinColumns 40 output

  forM_ policies $ \policy ->
    Hspec.it ("preserves the let header policy with a grouped RHS under bounded search " ++ show policy) $ do
      output <- checkedSource 80 2 policy $ moduleSource
        [ "example = do"
        , "  let localComments = List.nubBy"
        , "        (\\left right -> sourceCommentKey left == sourceCommentKey right)"
        , "        $ outerComments ++ nestedComments"
        , "  pure localComments"
        ]
      if policy == IndentPolicyFree
        then output `Hspec.shouldContain` "let localComments ="
        else output `Hspec.shouldContain` "let\n    localComments ="
      assertCohesive "outerComments ++ nestedComments" output
      assertWithinColumns 80 output

  forM_ policies $ \policy -> forM_ ["`Hspec.shouldBe`", "`shouldBe`"] $ \operator ->
    Hspec.it ("retains conservative continuation columns for an assertion after <$> with "
      ++ operator ++ show policy) $ do
      output <- checkedSource 60 2 policy $ moduleSource
        [ "result = alignmentGroupRows <$> alignmentPlanGroups plan "
          ++ operator ++ " [take 2 rows, take 1 $ drop 2 rows, drop 3 rows]"
        ]
      case (filter (List.isInfixOf "<$>") $ lines output,
            filter (List.isInfixOf operator) $ lines output) of
        ([mapping], [assertion]) -> do
          let column token = length . takeWhile (not . List.isPrefixOf token) . List.tails
          column operator assertion `Hspec.shouldSatisfy` (<= column "<$>" mapping)
        _ -> Hspec.expectationFailure "expected one mapping and one assertion operator"
      assertWithinColumns 60 output

  Hspec.it "formats twenty applicative alternative groups within a bounded three-pass budget" $ do
    let units = ["parse" ++ show index ++ " <$> input" ++ show index | index <- [1 :: Int .. 20]]
        source = moduleSource ["result = " ++ List.intercalate " <|> " units]
    result <- Timeout.timeout 20000000 $ checkedSource 80 2 IndentPolicyFree source
    case result of
      Nothing -> Hspec.expectationFailure "twenty alternative groups exceeded the 20-second budget"
      Just output -> do
        forM_ units $ \unit -> assertCohesive unit output
        assertWithinColumns 80 output

  forM_ ["result = transform $ first +", "result = first <$> input <|>", "result = first == First && ("] $
    \declaration -> Hspec.it ("rejects malformed input without inplace replacement: " ++ declaration) $ do
      let source = moduleSource [declaration]
      directory <- Directory.getTemporaryDirectory
      Exception.bracket
        (do
          (path, handle) <- IO.openTempFile directory "brittany-cohesive-chain-invalid.hs"
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

  forM_
    [ ("operator order", "result = first + second * third", "result = first * second + third")
    , ("explicit parentheses", "result = (first + second) * third", "result = first + (second * third)")
    ] $ \(name, before, after) -> Hspec.it ("detects a semantic mutation of " ++ name) $ do
      input <- parsedSource $ moduleSource [before]
      output <- parsedSource $ moduleSource [after]
      case (input, output) of
        ((_, inputParsed, ()), (_, outputParsed, ())) ->
          compareSemanticSyntax inputParsed outputParsed `Hspec.shouldNotBe` Right Nothing

  forM_ [[], ["-- retained comment", "-- retained comment"]] $ \changedComments ->
    Hspec.it ("detects comment loss or duplication: " ++ show (length changedComments)) $ do
      (inputAnns, _, ()) <- parsedSource $ moduleSource ["-- retained comment", "result = first + second * third"]
      (outputAnns, _, ()) <- parsedSource $ moduleSource $ changedComments ++ ["result = first + second * third"]
      case (normalizeCommentPlan inputAnns, normalizeCommentPlan outputAnns) of
        (Right inputPlan, Right outputPlan) ->
          commentPlanFingerprint outputPlan `Hspec.shouldNotBe` commentPlanFingerprint inputPlan
        _ -> Hspec.expectationFailure "invalid comment plan in validator mutation control"

assertBooleanTier :: String -> String -> IO ()
assertBooleanTier finalUnit output = case filter (List.isInfixOf finalUnit) $ lines output of
  [row] | "||" `List.isInfixOf` row && "&&" `List.isInfixOf` row -> pure ()
  [row] -> do
    let preceding = takeWhile (/= row) $ lines output
        disjunctions = filter (List.isInfixOf "||") preceding
    case reverse disjunctions of
      outer : _ -> indentation row `Hspec.shouldSatisfy` (> indentation outer)
      [] -> Hspec.expectationFailure "expected a disjunction before its conjunction operand"
  _ -> Hspec.expectationFailure "expected exactly one final conjunction operand"
 where
  indentation = length . takeWhile (== ' ')

assertCohesive :: String -> String -> IO ()
assertCohesive unit output = filter (List.isInfixOf unit) (lines output)
  `Hspec.shouldSatisfy` (not . null)

assertWithinColumns :: Int -> String -> IO ()
assertWithinColumns columns output = filter ((> columns) . length) (lines output)
  `Hspec.shouldBe` []

policies :: [IndentPolicy]
policies = [IndentPolicyLeft, IndentPolicyMultiple, IndentPolicyFree]

layoutDescription :: Int -> Int -> IndentPolicy -> String
layoutDescription columns indent policy = " at width " ++ show columns
  ++ ", indent " ++ show indent ++ ", " ++ show policy

checkedSource :: Int -> Int -> IndentPolicy -> String -> IO String
checkedSource columns indent policy original = do
  let config = configWithLayout columns indent policy
  first <- formatChecked config original
  (inputAnns, inputParsed, ()) <- parsedSource original
  (outputAnns, outputParsed, ()) <- parsedSource first
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

parsedSource :: String -> IO (Anns, GHC.ParsedSource, ())
parsedSource source = do
  parsed <- ParseModule.parseModule ["-haddock"] "CohesiveOperatorChain.hs"
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
