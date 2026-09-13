{-# LANGUAGE LambdaCase #-}

module InfixLambdaIndentationSpec (spec) where

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
spec projectRoot = Hspec.describe "ordinary infix lambda indentation" $ do
  Hspec.it "uses ten spaces for the reported application body at width80 and indent2" $ do
    output <- checkedSource 80 2 $ moduleSource
      [ "example = do"
      , "  when condition $ do"
      , "    when condition $ do"
      , "      filter ((> 40) . length) (lines output)"
      , "        `Hspec.shouldSatisfy` \\overflow ->"
      , "          checkSelectedValue firstArgument secondArgument"
      ]
    body <- uniqueLine "checkSelectedValue" output
    leadingSpaces body `Hspec.shouldBe` 10
    dropWhile (== ' ') body `Hspec.shouldBe` longBody
    assertCompactHeader "overflow" output

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ [0, 1] $ \depth ->
      forM_ (if columns == 40 then ["`Q.f`", "`f`", "<~>"]
        else ["`Hspec.shouldSatisfy`", "`shouldSatisfy`", "<~>"]) $ \operator ->
      forM_ ["value", "first second", "(first, second)"] $ \binder ->
        Hspec.it ("keeps " ++ binder ++ " structural after " ++ operator
          ++ " at width " ++ show columns ++ ", indent " ++ show indent
          ++ ", do depth " ++ show depth) $ do
          let body = if columns == 40 then "check value" else longBody
          output <- checkedSource columns indent $ nestedSource depth operator binder body
          assertCompactHeader binder output
          assertBodyIndent indent binder body output

  forM_ ["InfixRhsWrappingSpec.hs", "NonListInfixRhsSpec.hs"] $ \name ->
    Hspec.it ("removes the captured body column in complete " ++ name) $ do
      source <- readFile $ projectRoot </> "source/test-suite" </> name
      let config = configWithLayout 80 2
      output <- formatChecked config source
      assertStableAndEquivalent config source output
      let rest = dropWhile (not . List.isInfixOf "`Hspec.shouldSatisfy` \\overflow ->") $ lines output
      case rest of
        header : body : _ -> do
          body `Hspec.shouldSatisfy` List.isInfixOf "length overflow"
          leadingSpaces body `Hspec.shouldBe` leadingSpaces header + 2
          length body `Hspec.shouldSatisfy` (<= 80)
        _ -> Hspec.expectationFailure "missing maintained infix-lambda occurrence"

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent -> do
    forM_ [False, True] $ \parenthesized ->
      Hspec.it ((if parenthesized then "preserves a parenthesized lambda" else "preserves a standalone lambda")
        ++ " at width " ++ show columns ++ " and indent " ++ show indent) $ do
        let body = if columns == 40 then "check value" else longBody
            lambda = "\\value -> " ++ body
            expression = if parenthesized then "selectedValues `apply` (" ++ lambda ++ ")" else lambda
        output <- checkedSource columns indent $ moduleSource ["example = " ++ expression]
        assertCompactHeader "value" output
    Hspec.it ("retains fitting inline lambdas at width " ++ show columns ++ " and indent " ++ show indent) $ do
      output <- checkedSource columns indent $ moduleSource
        ["result = xs <~> \\x -> use x", "other = xs `f` \\(x, y) -> x"]
      output `Hspec.shouldContain` "result = xs <~> \\x -> use x"
      output `Hspec.shouldContain` "other = xs `f` \\(x, y) -> x"
    forM_ [False, True] $ \caseBody ->
      Hspec.it ((if caseBody then "retains a case lambda body" else "retains a do lambda body")
        ++ " at width " ++ show columns ++ " and indent " ++ show indent) $ do
        let declarations =
              ["example = do", "  selectedValues `inspect` \\value -> " ++ if caseBody then "case value of" else "do"]
              ++ if caseBody
                then ["    Just found -> check found", "    Nothing -> fallback"]
                else ["    verify first", "    verify second"]
            marker = if caseBody then "Just found" else "verify first"
        output <- checkedSource columns indent $ moduleSource declarations
        header <- lambdaHeader "value" output
        first <- uniqueLine marker output
        leadingSpaces first `Hspec.shouldSatisfy` (> leadingSpaces header)
        leadingSpaces first `Hspec.shouldSatisfy` (<= leadingSpaces header + 2 * indent)

  forM_ [2, 4] $ \indent -> forM_ commentCases $ \(name, declarations, marker) ->
    Hspec.it ("preserves " ++ name ++ " at indent " ++ show indent) $ do
      output <- checkedSource 80 indent $ moduleSource $ "example = do" : declarations
      _ <- uniqueLine marker output
      assertCompactHeader "value" output
      assertBodyIndent indent "value" longBody output

  forM_ malformedLambdas $ \(name, declarations) ->
    Hspec.it ("rejects " ++ name ++ " without replacing inplace input") $ do
      let source = moduleSource declarations
      directory <- Directory.getTemporaryDirectory
      Exception.bracket
        (do
          (path, handle) <- IO.openTempFile directory "brittany-infix-lambda-invalid.hs"
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

assertCompactHeader :: String -> String -> IO ()
assertCompactHeader binder output = lambdaHeader binder output >> pure ()

lambdaHeader :: String -> String -> IO String
lambdaHeader binder output = case
  filter (List.isInfixOf (compact $ "\\" ++ binder ++ " ->") . compact) $ lines output of
    [header] -> pure header
    matches -> Hspec.expectationFailure ("expected one compact lambda header, found " ++ show matches)
      >> fail "missing lambda header"
 where
  compact = filter (/= ' ')

assertBodyIndent :: Int -> String -> String -> String -> IO ()
assertBodyIndent indent binder body output = do
  header <- lambdaHeader binder output
  bodyLine <- uniqueLine body output
  if bodyLine == header
    then pure ()
    else do
      leadingSpaces bodyLine `Hspec.shouldSatisfy` (> leadingSpaces header)
      leadingSpaces bodyLine `Hspec.shouldSatisfy` (<= leadingSpaces header + indent)

uniqueLine :: String -> String -> IO String
uniqueLine marker output = case filter (List.isInfixOf marker) $ lines output of
  [line] -> pure line
  matches -> Hspec.expectationFailure ("expected one " ++ marker ++ " line, found " ++ show matches)
    >> fail "missing or repeated line"

leadingSpaces :: String -> Int
leadingSpaces = length . takeWhile (== ' ')

longBody :: String
longBody = "checkSelectedValue firstArgument secondArgument"

nestedSource :: Int -> String -> String -> String -> String
nestedSource depth operator binder body = moduleSource $
  ["example = do"]
  ++ [replicate (2 * level) ' ' ++ "when condition $ do" | level <- [1 .. depth]]
  ++ [ replicate (2 * (depth + 1)) ' ' ++ "selectedValues"
     , replicate (2 * (depth + 2)) ' ' ++ operator ++ " \\" ++ binder ++ " ->"
     , replicate (2 * (depth + 3)) ' ' ++ body
     ]

moduleSource :: [String] -> String
moduleSource declarations = unlines $ ["module InfixLambdaIndentation where", ""] ++ declarations

commentCases :: [(String, [String], String)]
commentCases =
  [ ("operator block", ["  selectedValues `Hspec.shouldSatisfy` {- lambda note -}", "    \\value ->", "      checkSelectedValue firstArgument secondArgument"], "{- lambda note -}")
  , ("arrow line", ["  selectedValues `Hspec.shouldSatisfy` \\value -> -- arrow note", "    checkSelectedValue firstArgument secondArgument"], "-- arrow note")
  , ("body block", ["  selectedValues `Hspec.shouldSatisfy` \\value ->", "    {- body note -}", "    checkSelectedValue firstArgument secondArgument"], "{- body note -}")
  , ("body line", ["  selectedValues `Hspec.shouldSatisfy` \\value ->", "    -- body note", "    checkSelectedValue firstArgument secondArgument"], "-- body note")
  ]

malformedLambdas :: [(String, [String])]
malformedLambdas =
  [ ("a missing lambda body", ["example = values `inspect` \\value ->"])
  , ("an unfinished tuple binder", ["example = values <~> \\(first, -> first"])
  ]

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
    parsed <- ParseModule.parseModule ["-haddock"] "InfixLambdaIndentation.hs"
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

