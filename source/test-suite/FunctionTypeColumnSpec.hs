{-# LANGUAGE LambdaCase #-}

module FunctionTypeColumnSpec (spec) where

import qualified Control.Exception as Exception
import Control.Monad (forM_)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import Data.Semigroup (Last(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import FunctionTypeColumnFixtures
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
spec projectRoot = Hspec.describe "function type component columns" $ do
  forM_
    [ ("ParseModule.hs", "parseModuleWithMetricsAndContext", parserResult)
    , ("Layouters/Decl.hs", "layoutPatternBindFinal", tupleList)
    ] $ \(path, name, component) ->
      Hspec.it ("keeps the fitting maintained component in " ++ name ++ " compact") $ do
        source <- readFile $ projectRoot
          </> "source/library/Language/Haskell/Brittany/Internal" </> path
        output <- checkedSource 80 2 IndentPolicyFree source
        signature <- signatureBlock name output
        assertCompactComponent 80 component signature
        if name == "layoutPatternBindFinal"
          then do
            assertCommentAtArrowColumn
              "-- ^ AnnKey for the node that contains the AnnWhere position annotation" signature
            lines signature `Hspec.shouldSatisfy` all ((<= 80) . length)
          else pure ()

  forM_ [40, 80, 100] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ policies $ \policy -> forM_ (componentsAt columns) $ \(kind, component) ->
      forM_ [False, True] $ \intermediate ->
        Hspec.it ("keeps a fitting " ++ kind ++ (if intermediate then " parameter" else " result")
          ++ " independent of preceding arrow count" ++ layoutDescription columns indent policy) $ do
          forM_ [1, 6, 12] $ \preceding -> do
            output <- checkedSource columns indent policy $
              signatureSource preceding intermediate component
            assertCompactComponent columns component output
            assertAlignedArrows output

  forM_ [2, 4] $ \indent -> forM_ policies $ \policy -> do
    let context = layoutDescription 80 indent policy
    forM_ contextualSignatures $ \(name, signature, comments) ->
      Hspec.it ("preserves fitting components with " ++ name ++ context) $ do
        output <- checkedSource 80 indent policy $ moduleSource
          ["f :: " ++ signature, "f = undefined"]
        assertCompactComponent 80 parserResult output
        forM_ comments $ \comment -> output `Hspec.shouldContain` comment
    Hspec.it ("retains wrapping for a genuinely over-width component" ++ context) $ do
      output <- checkedSource 40 indent policy $ signatureSource 6 False overwideComponent
      lines output `Hspec.shouldSatisfy`
        all (not . List.isInfixOf (compact overwideComponent) . compact)
      lines output `Hspec.shouldSatisfy` all ((<= 40) . length)
      assertAlignedArrows output

  forM_ [2, 4] $ \indent -> forM_ nearBudgetSources $ \(name, source, fits) ->
    Hspec.it ("measures a near-budget result under " ++ name ++ " at indent " ++ show indent) $ do
      let nestedAtFour = indent == 4 && "a nested" `List.isPrefixOf` name
          component = if nestedAtFour
            then replaceText ", value," ", val," nearBudgetComponent
            else nearBudgetComponent
          input = replaceText nearBudgetComponent component source
      output <- checkedSource 80 indent IndentPolicyFree input
      if fits
        then assertCompactComponent 80 component output
        else do
          output `Hspec.shouldContain` "SomeExtremelyLongKindName"
          output `Hspec.shouldContain` "AnotherLongKindName"

  forM_ wrappedForallSources $ \(name, source) ->
    Hspec.it ("keeps a fitting result after multiline forall binders in " ++ name) $ do
      output <- checkedSource 80 2 IndentPolicyFree source
      assertCompactComponent 80 nearBudgetComponent output
      forM_ ["first", "second", "third"] $ \binder ->
        output `Hspec.shouldContain` binder
      forM_ ["SomeExtremelyLongKindName", "AnotherExtremelyLongKindName", "ThirdExtremelyLongKindName"] $
        \kind -> output `Hspec.shouldContain` kind

  forM_ [2, 4] $ \indent -> forM_ policies $ \policy -> do
    let context = layoutDescription 80 indent policy
    forM_ [0, 3, 6, 10] $ \depth ->
      Hspec.it ("keeps an own-line argument post-doc at the arrow column after "
        ++ show depth ++ " preceding arrows" ++ context) $ do
        let comment = "-- ^ Explains the preceding parameter."
            types = replicate depth "A" ++ ["Maybe (Owner, [Document])"]
            signature = "f\n  :: " ++ List.intercalate "\n  -> " types
              ++ "\n     " ++ comment ++ "\n  -> Bool\n  -> Result"
        output <- checkedSource 80 indent policy $ moduleSource [signature, "f = undefined"]
        assertCommentAtArrowColumn comment output
        lines output `Hspec.shouldSatisfy` all ((<= 80) . length)
    Hspec.it ("preserves a genuinely long inline argument post-doc" ++ context) $ do
      let comment = "-- ^ This longer parameter documentation remains inline."
          signature = "f :: A -> " ++ nearBudgetComponent ++ " " ++ comment ++ "\n  -> Result"
      output <- checkedSource 80 indent policy $ moduleSource [signature, "f = undefined"]
      case filter (List.isInfixOf comment) $ lines output of
        [line] -> beforeComment (dropWhile (== ' ') line)
          `Hspec.shouldSatisfy` (not . null)
        _ -> Hspec.expectationFailure "missing or repeated inline parameter documentation"

  forM_
    [ ( "a nested callback"
      , [ "f :: A -> A -> A -> A -> A -> A -> (Input -> Middle"
        , "       -- ^ Nested argument."
        , "       -> Result) -> Output"
        , "f = undefined"
        ]
      , "-- ^ Nested argument."
      )
    , ( "a function type synonym"
      , [ "type Callback = A -> A -> A -> A -> A -> A -> Middle"
        , "       -- ^ Synonym argument."
        , "       -> Result"
        ]
      , "-- ^ Synonym argument."
      )
    ] $ \(name, declarations, comment) ->
      Hspec.it ("retains the actual local arrow base for documentation in " ++ name) $ do
        output <- checkedSource 80 2 IndentPolicyFree $ moduleSource declarations
        assertCommentAtArrowColumn comment output

  forM_
    [ ("a missing arrow result", "f :: A ->")
    , ("an unfinished tuple-list parameter", "f :: A -> [(Input, Output] -> Result")
    ] $ \(name, signature) ->
      Hspec.it ("rejects " ++ name ++ " without replacing inplace input") $ do
        let source = moduleSource [signature, "f = undefined"]
        directory <- Directory.getTemporaryDirectory
        Exception.bracket
          (do
            (path, handle) <- IO.openTempFile directory "brittany-function-type-invalid.hs"
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

assertCommentAtArrowColumn :: String -> String -> IO ()
assertCommentAtArrowColumn marker output = do
  let comments = filter (List.isInfixOf marker) $ lines output
      arrows = filter (List.isPrefixOf "->" . dropWhile (== ' '))
        $ drop 1 $ dropWhile (not . List.isInfixOf marker) $ lines output
      indentation = length . takeWhile (== ' ')
  case (comments, arrows) of
    ([comment], arrow : _) -> indentation comment `Hspec.shouldBe` indentation arrow
    _ -> Hspec.expectationFailure "missing unique post-doc or following arrow rows"

assertCompactComponent :: Int -> String -> String -> IO ()
assertCompactComponent columns component output = do
  let matches = filter (List.isInfixOf (compact component) . compact) $ lines output
  case matches of
    [line] -> length (beforeComment line) `Hspec.shouldSatisfy` (<= columns)
    _ -> Hspec.expectationFailure $
      "expected one compact component " ++ show component ++ " at its rendered column, found "
        ++ show matches ++ " in:\n" ++ output

beforeComment :: String -> String
beforeComment [] = []
beforeComment ('-' : '-' : _) = []
beforeComment (char : rest) = char : beforeComment rest

assertAlignedArrows :: String -> IO ()
assertAlignedArrows output = case
  [length $ takeWhile (== ' ') line | line <- lines output, "->" `List.isPrefixOf` dropWhile (== ' ') line] of
    [] -> pure ()
    first : remaining -> remaining `Hspec.shouldSatisfy` all (== first)

signatureBlock :: String -> String -> IO String
signatureBlock name output = case dropWhile (not . startsName) $ lines output of
  [] -> Hspec.expectationFailure ("missing signature for " ++ name) >> fail "missing signature"
  header : following -> pure $ unlines $ header : takeWhile (not . startsName) following
 where
  startsName line = name `List.isPrefixOf` line
    && [name] `List.isPrefixOf` words line

replaceText :: String -> String -> String -> String
replaceText old new = Text.unpack . Text.replace (Text.pack old) (Text.pack new) . Text.pack

compact :: String -> String
compact = filter (/= ' ')

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
    parsed <- ParseModule.parseModule ["-haddock"] "FunctionTypeColumns.hs"
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
