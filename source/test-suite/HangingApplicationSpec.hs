{-# LANGUAGE LambdaCase #-}

module HangingApplicationSpec (spec) where

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
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "hanging application layout" $ do
  Hspec.it "breaks the children bind before its complete RHS in SemanticFingerprint.Ghc" $ do
    source <- readFile $ projectRoot
      </> "source/library/Language/Haskell/Brittany/Internal/SemanticFingerprint/Ghc.hs"
    output <- checkedModule source
    assertCompleteChildrenBind output

  Hspec.it "avoids narrow argument columns in the complete transactional test module" $ do
    source <- readFile $ projectRoot </> "source/test-suite/TransactionalInplaceSpec.hs"
    output <- checkedModule source
    let calls = filter (List.isInfixOf "Transaction.operationRenameFile") $ lines output
        applications = filter (not . List.isInfixOf "=") calls
    length applications `Hspec.shouldBe` 2
    forM_ applications $ \call ->
      assertLocalArguments 2 call ["defaults", "source", "target"] output

  Hspec.it "avoids narrow argument columns in the complete benchmark scenario module" $ do
    source <- readFile $ projectRoot </> "benchmark/Benchmark/Scenario.hs"
    output <- checkedModule source
    call <- uniqueLine "(errors, output) <- pPrintModuleWithSourceMeasured" output
    assertLocalArguments 2 call
      ["metrics", "originalSource", "moduleConfig", "perItemConfig", "annotations", "parsedModule"] output

  forM_ [2, 4] $ \indent ->
    Hspec.it ("keeps the children RHS together after a bind break at indent " ++ show indent) $ do
      output <- checkedSource 80 indent IndentPolicyFree $ moduleSource
        [ "example value = case value of"
        , "  Box item -> do"
        , "    children <- " ++ childrenRhs
        , "    pure children"
        ]
      assertCompleteChildrenBind output

  forM_ [40, 80, 100] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ policies $ \policy -> forM_ applicationCases $ \(description, declarations) ->
      Hspec.it (description ++ " at width " ++ show columns ++ ", indent "
        ++ show indent ++ ", " ++ show policy) $ do
        _ <- checkedSource columns indent policy $ moduleSource declarations
        pure ()

  forM_ [2, 4] $ \indent -> forM_ policies $ \policy ->
    Hspec.it ("keeps fitting applications compact at indent " ++ show indent
      ++ " with " ++ show policy) $ do
      output <- checkedSource 80 indent policy $ moduleSource
        [ "result = combine left right"
        , "action = do"
        , "  value <- combine left right"
        , "  pure value"
        ]
      output `Hspec.shouldContain` "result = combine left right"
      output `Hspec.shouldContain` "value <- combine left right"

  forM_ [2, 4] $ \indent ->
    Hspec.it ("retains useful moderate argument alignment at indent " ++ show indent) $ do
      output <- checkedSource 40 indent IndentPolicyFree $ moduleSource
        ["result = combine firstArgument secondArgument thirdArgument"]
      lines output `Hspec.shouldContain`
        [ "result = combine firstArgument"
        , "                 secondArgument"
        , "                 thirdArgument"
        ]

  forM_ [2, 4] $ \indent ->
    Hspec.it ("breaks after a long function head before three short arguments at indent " ++ show indent) $ do
      let functionName = "lakjsdlajsdljasdlkjasldjasldjasldjalsdjlaskjd"
      output <- checkedSource 80 indent IndentPolicyFree $ moduleSource
        ["func = " ++ functionName ++ " firstArgument secondArgument thirdArgument"]
      call <- uniqueLine functionName output
      let arguments = ["firstArgument", "secondArgument", "thirdArgument"]
      forM_ arguments $ \argument -> do
        argumentLine <- uniqueLine argument output
        dropWhile (== ' ') argumentLine `Hspec.shouldBe` argument
      assertLocalArguments indent call arguments output

  forM_ [2, 4] $ \indent -> forM_ commentCases $ \(description, columns, declarations) ->
    Hspec.it ("preserves " ++ description ++ " through a bind break at indent " ++ show indent) $ do
      output <- checkedSource columns indent IndentPolicyFree $ moduleSource declarations
      _ <- uniqueLine "argument note" output
      pure ()

  forM_ [2, 4] $ \indent ->
    Hspec.it ("does not widen an externally owned block suffix at indent " ++ show indent) $ do
      let prefix = replicate indent ' ' ++ "value <- combine "
          source = moduleSource
            [ "result = do"
            , prefix ++ "firstValue"
            , replicate (length prefix) ' ' ++ "secondValue {- argument note -}"
            , replicate indent ' ' ++ "pure value"
            ]
          config = configWithLayout 40 indent IndentPolicyFree
      output <- formatChecked config source
      -- This stable input already exceeds 40 columns. Permit improvements,
      -- but do not widen its externally emitted suffix through RHS compaction.
      maximum (map length $ lines output)
        `Hspec.shouldSatisfy` (<= maximum (map length $ lines source))
      assertStableAndEquivalent config source output

  forM_ [2, 4] $ \indent ->
    Hspec.it ("keeps the issue 206 nested tuple lambda intact at indent " ++ show indent) $ do
      output <- checkedSource 80 indent IndentPolicyFree $ moduleSource
        [ "example = do"
        , "  forM_ outerValues $ \\outer ->"
        , "    forM_ allSelectedCases $ \\(keyword, body, marker) -> do"
        , "      run keyword"
        , "      run marker"
        ]
      tupleLine <- uniqueLine "(keyword, body, marker) ->" output
      indentation tupleLine `Hspec.shouldSatisfy` (<= 6 * indent)

  forM_ malformedApplications $ \(description, declarations) ->
    Hspec.it ("rejects " ++ description ++ " without replacing inplace input") $ do
      let source = moduleSource declarations
      directory <- Directory.getTemporaryDirectory
      Exception.bracket
        (do
          (path, handle) <- IO.openTempFile directory "brittany-hanging-application-invalid.hs"
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

checkedModule :: String -> IO String
checkedModule source = do
  let config = configWithLayout 80 2 IndentPolicyFree
  output <- formatChecked config source
  assertStableAndEquivalent config source output
  pure output

checkedSource :: Int -> Int -> IndentPolicy -> String -> IO String
checkedSource columns indent policy source = do
  let config = configWithLayout columns indent policy
  output <- formatChecked config source
  filter ((> columns) . length) (lines output) `Hspec.shouldBe` []
  assertStableAndEquivalent config source output
  pure output

assertCompleteChildrenBind :: String -> IO ()
assertCompleteChildrenBind output = do
  binding <- uniqueLine "children <-" output
  dropWhile (== ' ') binding `Hspec.shouldBe` "children <-"
  rhs <- uniqueLine childrenRhs output
  indentation rhs `Hspec.shouldSatisfy` (> indentation binding)
  length rhs `Hspec.shouldSatisfy` (<= 80)

assertLocalArguments :: Int -> String -> [String] -> String -> IO ()
assertLocalArguments indent call arguments output = do
  let following = drop 1 $ dropWhile (/= call) $ lines output
      argumentLines = takeWhile (\line -> dropWhile (== ' ') line `elem` arguments) following
  -- Whole-call compaction is valid too; only detached argument rows need a bound.
  forM_ argumentLines $ \line -> do
    indentation line `Hspec.shouldSatisfy` (<= indentation call + indent)
    length line `Hspec.shouldSatisfy` (<= 80)
  length call `Hspec.shouldSatisfy` (<= 80)

uniqueLine :: String -> String -> IO String
uniqueLine marker output = case filter (List.isInfixOf marker) $ lines output of
  [line] -> pure line
  matches -> Hspec.expectationFailure
    ("expected one line containing " ++ show marker ++ ", found " ++ show matches)
    >> fail "missing or duplicated marker"

indentation :: String -> Int
indentation = length . takeWhile (== ' ')

childrenRhs :: String
childrenRhs = "sequence $ zipWith projectChild childNames $ Data.gmapQ Box value"

policies :: [IndentPolicy]
policies = [IndentPolicyLeft, IndentPolicyMultiple, IndentPolicyFree]

applicationCases :: [(String, [String])]
applicationCases =
  [ ("wraps a bind with short arguments",
      [ "result = do"
      , "  selectedResult <- applySelectedValues first second third fourth fifth"
      , "  pure selectedResult"
      ])
  , ("wraps long arguments without losing their application",
      [ "result = assembleSelectedValues firstSelectedArgument secondSelectedArgument thirdSelectedArgument"
      ])
  , ("wraps a nested application and operator chain",
      [ "result = do"
      , "  selectedResult <- collect $ map transformValue $ applySelectedValues first second third fourth"
      , "  pure selectedResult"
      ])
  ]

commentCases :: [(String, Int, [String])]
commentCases =
  [ ("an internal line comment", 40,
      [ "result = do"
      , "  value <- combine firstValue -- argument note"
      , "    secondValue"
      , "  pure value"
      ])
  , ("a trailing block comment", 80,
      [ "result = do"
      , "  value <- combine firstValue secondValue {- argument note -}"
      , "  pure value"
      ])
  ]

malformedApplications :: [(String, [String])]
malformedApplications =
  [ ("a bind without an RHS", ["result = do", "  value <-"])
  , ("an unclosed application argument", ["result = do", "  value <- combine (firstValue", "  pure value"])
  ]

moduleSource :: [String] -> String
moduleSource declarations = unlines $ ["module HangingApplications where", ""] ++ declarations

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
    parsed <- ParseModule.parseModule ["-haddock"] "HangingApplications.hs"
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
