{-# LANGUAGE LambdaCase #-}

module ExpressionCommentBoundarySpec (spec) where

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
import Language.Haskell.Brittany.Internal.Config.Types (AltChooser(..))
import Language.Haskell.Brittany.Internal.CommentPlan
  ( commentPlanFingerprint
  , normalizeCommentPlan
  )
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
spec projectRoot = Hspec.describe "standalone expression comment boundaries" $ do
  forM_ [40, 80, 100] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ expressionShapes $ \(shape, headExpression, follower) ->
      forM_ commentForms $ \(kind, comments) ->
        Hspec.it (shape ++ " resumes structurally after " ++ kind
          ++ " at width " ++ show columns ++ " and indent " ++ show indent) $ do
          output <- checkedSource columns indent $ moduleSource $
            ["example = " ++ headExpression]
            ++ map ("  " ++) comments ++ ["  " ++ follower]
          assertFollower indent "veryLongFunctionName" follower output
          assertCommentOrder comments follower output

  forM_ [2, 4] $ \indent -> forM_ expressionShapes $ \(shape, headExpression, follower) ->
    forM_ [False, True] $ \nested ->
      Hspec.it (shape ++ " keeps a structural boundary in "
        ++ (if nested then "a nested expression" else "a do statement")
        ++ " at indent " ++ show indent) $ do
        let declarations = if nested
              then [ "example = outerFunction $ do"
                   , "  result <- middleFunction $ " ++ headExpression
                   , "    -- boundary note"
                   , "    " ++ follower
                   , "  pure result"
                   ]
              else [ "example = do"
                   , "  result <- " ++ headExpression
                   , "    -- boundary note"
                   , "    " ++ follower
                   , "  pure result"
                   ]
        output <- checkedSource 80 indent $ moduleSource declarations
        assertFollower indent "veryLongFunctionName" follower output
        assertCommentOrder ["-- boundary note"] follower output

  Hspec.it "resumes the pattern-synonym alternative structurally in the complete Decl module" $ do
    let config = configWithLayout 80 2
    source <- readFile $ projectRoot </>
      "source/library/Language/Haskell/Brittany/Internal/Layouters/Decl.hs"
    output <- formatChecked config source
    assertStableAndEquivalent config source output
    let preceding = takeWhile (not . List.isInfixOf "-- pattern .. where") $ lines output
        following = drop 1 $ dropWhile (not . List.isInfixOf "-- pattern .. where") $ lines output
    anchor <- lastMatching "addAlternativeCond" preceding
    resumed <- firstMatching "docAddBaseY BrIndentRegular" following
    indentation resumed `Hspec.shouldSatisfy` (<= indentation anchor + 4)

  Hspec.it "resumes the commented-out source example structurally in the complete Utils module" $ do
    let config = configWithLayout 80 2
    source <- readFile $ projectRoot </>
      "source/library/Language/Haskell/Brittany/Internal/Utils.hs"
    output <- formatChecked config source
    assertStableAndEquivalent config source output
    let isExample = List.isInfixOf "-- - $"
        preceding = takeWhile (not . isExample) $ lines output
        following = drop 1 $ dropWhile isNotExample $ lines output
        isNotExample = not . isExample
    anchor <- lastMatching "simpleLayouter" preceding
    resumed <- firstMatching "$" following
    indentation resumed `Hspec.shouldSatisfy` (<= indentation anchor + 4)
    resumed `Hspec.shouldSatisfy` List.isPrefixOf "$" . dropWhile (== ' ')

  forM_ [2, 4] $ \indent -> do
    Hspec.it ("keeps uncommented expressions compact at indent " ++ show indent) $ do
      output <- checkedSource 80 indent $ moduleSource
        [ "example = veryLongFunctionName $ anotherFunction value"
        , "application = veryLongFunctionName anotherValue"
        ]
      output `Hspec.shouldContain` "example = veryLongFunctionName $ anotherFunction value"
      output `Hspec.shouldContain` "application = veryLongFunctionName anotherValue"
    Hspec.it ("preserves inline-seeded continuation alignment at indent " ++ show indent) $ do
      let seed = "  then select value -- seed note"
          continued = replicate (length $ beforeMarker "-- seed note" seed) ' '
            ++ "-- continued note"
      output <- checkedSource 80 indent $ moduleSource
        ["example = if condition", seed, continued, "  else fallback"]
      first <- uniqueLine "-- seed note" output
      second <- uniqueLine "-- continued note" output
      length (beforeMarker "-- seed note" first)
        `Hspec.shouldBe` length (beforeMarker "-- continued note" second)
    Hspec.it ("preserves a protected run and blank separation at indent " ++ show indent) $ do
      let comments = ["-- > originalValue $ nextValue", "--   /\\ -> preserved"]
      output <- checkedSource 80 indent $ moduleSource $
        ["example = veryLongFunctionName $"]
        ++ map ("  " ++) comments ++ ["", "  -- final note", "  anotherFunction value"]
      assertCommentOrder (comments ++ ["-- final note"]) "anotherFunction value" output
      let between = takeWhile (not . List.isInfixOf "-- final note")
            $ drop 1 $ dropWhile (not . List.isInfixOf "--   /\\ -> preserved") $ lines output
      between `Hspec.shouldSatisfy` any null
      assertFollower indent "veryLongFunctionName" "anotherFunction value" output

  forM_ [("line", "-- boundary note"), ("block", "{- boundary note -}")] $
    \(kind, comment) ->
      Hspec.it ("keeps an interrupted expression valid with the Quick chooser and a " ++ kind ++ " comment") $ do
        let baseConfig = configWithLayout 80 2
            config = baseConfig
              { _conf_layout = (_conf_layout baseConfig)
                  { _lconfig_altChooser = Identity $ Last AltChooserSimpleQuick }
              }
            source = moduleSource
              [ "example = veryLongFunctionName $"
              , "  " ++ comment
              , "  anotherFunction value"
              ]
        output <- formatChecked config source
        assertStableAndEquivalent config source output
        assertCommentOrder [comment] "anotherFunction value" output
        assertFollower 2 "veryLongFunctionName" "anotherFunction value" output
        resumed <- uniqueLine "anotherFunction" output
        indentation resumed `Hspec.shouldSatisfy` (> 0)
        filter ((> 80) . length) (lines output) `Hspec.shouldBe` []

  forM_ malformedExpressions $ \(description, declarations) ->
    Hspec.it ("rejects " ++ description ++ " without replacing inplace input") $ do
      let source = moduleSource declarations
      directory <- Directory.getTemporaryDirectory
      Exception.bracket
        (do
          (path, handle) <- IO.openTempFile directory "brittany-comment-boundary-invalid.hs"
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

checkedSource :: Int -> Int -> String -> IO String
checkedSource columns indent source = do
  let config = configWithLayout columns indent
  output <- formatChecked config source
  filter ((> columns) . length) (lines output) `Hspec.shouldBe` []
  assertStableAndEquivalent config source output
  pure output

-- The bound follows the expression's rendered context, not an absolute column.
-- It permits an operator and its operand to occupy separate structural levels.
assertFollower :: Int -> String -> String -> String -> IO ()
assertFollower indent anchorMarker follower output = do
  anchor <- uniqueLine anchorMarker output
  let marker = if "$ " `List.isPrefixOf` follower then "anotherFunction" else head $ words follower
  resumed <- uniqueLine marker output
  indentation resumed `Hspec.shouldSatisfy` (<= indentation anchor + 2 * indent)

assertCommentOrder :: [String] -> String -> String -> IO ()
assertCommentOrder comments follower output = do
  forM_ comments $ \comment -> do
    _ <- uniqueLine comment output
    pure ()
  let codeMarker = head $ words $ if "$ " `List.isPrefixOf` follower then drop 2 follower else follower
      positions = map (length . (`beforeMarker` output)) (comments ++ [codeMarker])
  positions `Hspec.shouldBe` List.sort positions
  length (List.nub positions) `Hspec.shouldBe` length positions

indentation :: String -> Int
indentation = length . takeWhile (== ' ')

uniqueLine :: String -> String -> IO String
uniqueLine marker output = case filter (List.isInfixOf marker) $ lines output of
  [line] -> pure line
  matches -> Hspec.expectationFailure
    ("expected one line containing " ++ show marker ++ ", found " ++ show matches)
    >> fail "missing or duplicated marker"

firstMatching :: String -> [String] -> IO String
firstMatching marker rows = case filter (List.isInfixOf marker) rows of
  line : _ -> pure line
  [] -> Hspec.expectationFailure ("missing " ++ marker) >> fail "missing marker"

lastMatching :: String -> [String] -> IO String
lastMatching marker = firstMatching marker . reverse

beforeMarker :: String -> String -> String
beforeMarker marker line = case
  [ prefix | (prefix, suffix) <- zip (List.inits line) (List.tails line)
           , marker `List.isPrefixOf` suffix ] of
    prefix : _ -> prefix
    [] -> line

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
    parsed <- ParseModule.parseModule ["-haddock"] "ExpressionCommentBoundary.hs"
      (const $ pure $ Right ()) source
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError >> fail parseError
      Right result -> pure result

configWithLayout :: Int -> Int -> Config
configWithLayout columns indent = staticDefaultConfig
  { _conf_layout = (_conf_layout staticDefaultConfig)
      { _lconfig_cols = Identity $ Last columns
      , _lconfig_indentAmount = Identity $ Last indent
      }
  , _conf_errorHandling = (_conf_errorHandling staticDefaultConfig)
      { _econf_Werror = Identity $ Last True
      , _econf_failOnExactSourceFallback = Identity $ Last True
      }
  }

moduleSource :: [String] -> String
moduleSource declarations = unlines $ ["module ExpressionCommentBoundary where", ""] ++ declarations

expressionShapes :: [(String, String, String)]
expressionShapes =
  [ ("after an operator", "veryLongFunctionName $", "anotherFunction value")
  , ("after the left operand", "veryLongFunctionName", "$ anotherFunction value")
  , ("inside an application", "veryLongFunctionName", "anotherValue")
  ]

commentForms :: [(String, [String])]
commentForms =
  [ ("a line comment", ["-- boundary note"])
  , ("a block comment", ["{- boundary note -}"])
  , ("a multiline block comment", ["{- boundary note", "   continued note -}"])
  , ("source-sensitive code comments", ["-- > originalValue $ nextValue", "--   /\\ -> preserved"])
  ]

malformedExpressions :: [(String, [String])]
malformedExpressions =
  [ ("a missing operator operand", ["example = value $", "  -- missing operand"])
  , ("an unclosed application", ["example = function (", "  {- boundary note -}", "  value"])
  ]
