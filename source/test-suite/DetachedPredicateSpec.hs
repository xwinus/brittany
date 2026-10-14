{-# LANGUAGE LambdaCase #-}

module DetachedPredicateSpec (spec) where

import qualified Control.Exception as Exception
import Control.Monad (forM_)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import Data.Semigroup (Last(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified GHC
import Language.Haskell.Brittany.Internal.ExactPrintCompat (Anns)
import DetachedPredicateFixtures
import Language.Haskell.Brittany
  ( CConfig(..), CErrorHandlingConfig(..), CLayoutConfig(..), Config
  , parsePrintModule, staticDefaultConfig
  )
import Language.Haskell.Brittany.Internal.CommentPlan
  ( commentPlanFingerprint, normalizeCommentPlan )
import Language.Haskell.Brittany.Internal.Config.Types (AltChooser(..), IndentPolicy(..))
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SemanticFingerprint (compareSemanticSyntax)
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "compact detached predicates" $ do
  Hspec.it "detaches the reported complete section at width sixty" $ do
    let predicate = "(<= AnnotationIndex.indexNodeCount annotationIndex)"
    output <- checkedSource 60 2 IndentPolicyFree $ moduleSource
      [ "f spans annotationIndex = Map.size spans `Hspec.shouldSatisfy` " ++ predicate ]
    assertDetached "`Hspec.shouldSatisfy`" predicate output
    assertWithinColumns 60 output

  forM_ maintainedCases $ \(path, columns, predicate) ->
    Hspec.it ("keeps the audited predicate compact in " ++ path) $ do
      source <- readFile $ projectRoot </> path
      output <- checkedSource columns 2 IndentPolicyFree source
      assertCompactPredicate columns predicate output

  forM_ [40, 60, 80, 100] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ policies $ \policy -> do
      let context = layoutDescription columns indent policy
      forM_ predicateShapes $ \shape ->
        Hspec.it ("detaches a fitting " ++ shape ++ context) $ do
          let predicate = predicateOfSize shape $ columns - 3 * indent - 1
          output <- checkedSource columns indent policy $
            assertionSource False "`Hspec.shouldSatisfy`" predicate
          assertDetached "`Hspec.shouldSatisfy`" predicate output
          assertWithinColumns columns output
      Hspec.it ("retains a fitting attachment after a short operator" ++ context) $ do
        let predicate = "(<= count values)"
        output <- checkedSource columns indent policy $ assertionSource False "==" predicate
        assertCompactPredicate columns ("== " ++ predicate) output
        assertWithinColumns columns output

  forM_ policies $ \policy -> do
    Hspec.it ("preserves the fitting multiline left-section attachment at width61 " ++ show policy) $ do
      output <- checkedSource 61 4 policy $ moduleSource
        [ "f spans annotationIndex = Map.size spans `Hspec.shouldSatisfy`"
          ++ " (AnnotationIndex.indexNodeCount annotationIndex <=)"
        ]
      row <- uniqueLine "`Hspec.shouldSatisfy`" output
      row `Hspec.shouldContain` "(AnnotationIndex.indexNodeCount"
      assertWithinColumns 61 output
    Hspec.it ("retains attachment for a noncompact predicate containing case blocks " ++ show policy) $ do
      output <- checkedSource 80 2 policy noncompactBlockSource
      output `Hspec.shouldContain` "&& ((case placementOwner placement of"
      map (dropWhile (== ' ')) (lines output) `Hspec.shouldNotContain` ["&&"]
      assertWithinColumns 80 output

  forM_ policies $ \policy -> forM_ [False, True] $ \nested ->
    Hspec.it ("detaches an explicit type application with nested parentheses "
      ++ show nested ++ ", " ++ show policy) $ do
      let typeName = 'T' : replicate 50 'x'
          application = "(predicate @" ++ typeName ++ ")"
          predicate = if nested then "(" ++ application ++ ")" else application
          source = "{-# LANGUAGE TypeApplications #-}\n"
            ++ assertionSource False "`Hspec.shouldSatisfy`" predicate
      output <- checkedSource 80 2 policy source
      assertDetached "`Hspec.shouldSatisfy`" predicate output
      assertWithinColumns 80 output

  forM_ [2, 4] $ \indent -> forM_ policies $ \policy -> do
    let context = layoutDescription 80 indent policy
    forM_ ["`Hspec.shouldSatisfy`", "`shouldSatisfy`", "<~~~~~~~~>"] $ \operator ->
      Hspec.it ("detaches predicates throughout flattened chains after " ++ operator ++ context) $ do
        let predicate = predicateOfSize "right section" $ 80 - 3 * indent - 1
        output <- checkedSource 80 indent policy $ assertionSource True operator predicate
        assertCompactPredicate 80 predicate output
        lines output `Hspec.shouldSatisfy` any ((== compact predicate) . compact)
        assertWithinColumns 80 output
    forM_ [0, 1] $ \excess ->
      Hspec.it ("accounts for the operator width at the attachment boundary plus " ++ show excess ++ context) $ do
        let operator = "`Hspec.shouldSatisfy`"
            predicate = predicateOfSize "right section" $ 80 - 2 * indent - length operator - 1 + excess
        output <- checkedSource 80 indent policy $ assertionSource False operator predicate
        assertCompactPredicate 80 predicate output
        if excess == 0
          then do
            row <- uniqueLine operator output
            row `Hspec.shouldContain` predicate
            length row `Hspec.shouldBe` 80
          else assertDetached operator predicate output
        assertWithinColumns 80 output
    forM_ oversizedPredicates $ \predicate ->
      Hspec.it ("still wraps a genuinely oversized predicate " ++ predicate
        ++ layoutDescription 60 indent policy) $ do
        output <- checkedSource 60 indent policy $ assertionSource False "`shouldSatisfy`" predicate
        lines output `Hspec.shouldSatisfy` all (not . List.isInfixOf (compact predicate) . compact)
        assertWithinColumns 60 output
    Hspec.it ("keeps attached and detached choices available to the shallow chooser" ++ context) $ do
      let config = configWithLayout 80 indent policy
          shallow = config { _conf_layout = (_conf_layout config)
            { _lconfig_altChooser = Identity $ Last AltChooserShallowBest } }
      forM_ [20, 80 - 4 * indent - 1] $ \size -> do
        let predicate = predicateOfSize "right section" size
        output <- checkedModule shallow $ assertionSource False "`shouldSatisfy`" predicate
        assertCompactPredicate 80 predicate output
        assertWithinColumns 80 output

  forM_ [2, 4] $ \indent -> forM_ commentCases $ \(name, declarations, marker) ->
    Hspec.it ("preserves " ++ name ++ " at indent " ++ show indent) $ do
      output <- checkedSource 80 indent IndentPolicyFree $ moduleSource $ "example = do" : declarations
      _ <- uniqueLine marker output
      assertWithinColumns 80 output

  forM_ policies $ \policy -> do
    Hspec.it ("preserves existing literal, list, lambda and do-block operands " ++ show policy) $ do
      forM_ unchangedOperandCases $ \(_, operand) -> do
        output <- checkedSource 80 2 policy $ assertionSource False "`shouldSatisfy`" operand
        assertWithinColumns 80 output
    Hspec.it ("detaches a fitting predicate in a nested do/case block " ++ show policy) $ do
      let predicate = "(<= AnnotationIndex.indexNodeCount annotationIndex)"
      output <- checkedSource 80 2 policy $ moduleSource
        [ "example = do"
        , "  when selected $ do"
        , "    case result of"
        , "      Right spans -> do"
        , "        Map.size spans `Hspec.shouldSatisfy` " ++ predicate
        , "        nextAction"
        , "      Left err -> fail err"
        , "  afterCase"
        ]
      assertDetached "`Hspec.shouldSatisfy`" predicate output
      assertWithinColumns 80 output

  forM_
    [ "example = value `shouldSatisfy` (<=) selected ("
    , "example = value `shouldSatisfy` (selected +"
    , "example = value `shouldSatisfy` (predicate .) ("
    ] $ \declaration ->
      Hspec.it ("rejects malformed predicates without inplace replacement: " ++ declaration) $ do
        let source = moduleSource [declaration]
        directory <- Directory.getTemporaryDirectory
        Exception.bracket
          (do
            (path, handle) <- IO.openTempFile directory "brittany-detached-predicate-invalid.hs"
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
    [ ("section direction", "result = (left +)", "result = (+ left)")
    , ("composition order", "result = (first . second)", "result = (second . first)")
    ] $ \(name, original, mutated) ->
      Hspec.it ("detects a semantic mutation of " ++ name) $ do
        (_, before, ()) <- parsedSource $ moduleSource [original]
        (_, after, ()) <- parsedSource $ moduleSource [mutated]
        compareSemanticSyntax before after `Hspec.shouldNotBe` Right Nothing

  forM_ [[], ["-- retained", "-- retained"]] $ \comments ->
    Hspec.it ("detects predicate comment loss or duplication " ++ show (length comments)) $ do
      (before, _, ()) <- parsedSource $ moduleSource ["-- retained", "result = (<= value)"]
      (after, _, ()) <- parsedSource $ moduleSource $ comments ++ ["result = (<= value)"]
      case (normalizeCommentPlan before, normalizeCommentPlan after) of
        (Right original, Right mutated) ->
          commentPlanFingerprint mutated `Hspec.shouldNotBe` commentPlanFingerprint original
        _ -> Hspec.expectationFailure "invalid comment plan in mutation control"

assertDetached :: String -> String -> String -> IO ()
assertDetached operator predicate output = do
  operatorRow <- uniqueLine operator output
  operatorRow `Hspec.shouldNotContain` "("
  lines output `Hspec.shouldSatisfy` any ((== compact predicate) . compact)

assertCompactPredicate :: Int -> String -> String -> IO ()
assertCompactPredicate columns predicate output = do
  let matching = filter (List.isInfixOf (compact predicate) . compact) $ lines output
  matching `Hspec.shouldSatisfy` (not . null)
  matching `Hspec.shouldSatisfy` all ((<= columns) . length)

assertWithinColumns :: Int -> String -> IO ()
assertWithinColumns columns output = filter ((> columns) . length) (lines output) `Hspec.shouldBe` []

uniqueLine :: String -> String -> IO String
uniqueLine marker output = case filter (List.isInfixOf marker) $ lines output of
  [row] -> pure row
  rows -> Hspec.expectationFailure ("expected one " ++ marker ++ " row, got " ++ show rows)
    >> fail "missing or repeated marker"

compact :: String -> String
compact = filter (/= ' ')

policies :: [IndentPolicy]
policies = [IndentPolicyLeft, IndentPolicyMultiple, IndentPolicyFree]

layoutDescription :: Int -> Int -> IndentPolicy -> String
layoutDescription columns indent policy = " at width " ++ show columns
  ++ ", indent " ++ show indent ++ ", " ++ show policy

checkedSource :: Int -> Int -> IndentPolicy -> String -> IO String
checkedSource columns indent policy = checkedModule $ configWithLayout columns indent policy

checkedModule :: Config -> String -> IO String
checkedModule config original = do
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
  parsed <- ParseModule.parseModule ["-haddock"] "DetachedPredicate.hs"
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
