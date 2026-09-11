{-# LANGUAGE LambdaCase #-}

module InlineBranchCommentSpec (spec) where

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
spec projectRoot = Hspec.describe "inline branch comments" $ do
  Hspec.it "keeps Unicode star beside its expression in the complete Type module" $ do
    source <- readFile $ projectRoot
      </> "source/library/Language/Haskell/Brittany/Internal/Layouters/Type.hs"
    let config = configWithLayout 80 2
    output <- formatChecked config source
    commentLine <- uniqueLine "-- Unicode star" output
    beforeMarker "-- Unicode star" commentLine `Hspec.shouldContain` "Text.pack"
    commentLine `Hspec.shouldContain` "\\x2605"
    length commentLine `Hspec.shouldSatisfy` (<= 80)
    assertStableAndEquivalent config source output

  forM_ [40, 80, 100] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ [False, True] $ \elseBranch -> forM_ commentForms $ \(kind, comment) ->
      Hspec.it ("keeps a fitting " ++ branchName elseBranch ++ " " ++ kind
        ++ " comment after its expression at width " ++ show columns
        ++ " and indent " ++ show indent) $ do
        output <- checkedSource columns indent $ moduleSource $
          branchSource elseBranch ("keep x " ++ comment)
        commentLine <- uniqueLine comment output
        beforeMarker comment commentLine `Hspec.shouldContain` "keep x"

  forM_ [40, 80, 100] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ [False, True] $ \elseBranch ->
      Hspec.it ("keeps a nested wrapped " ++ branchName elseBranch
        ++ " comment with its expression at width " ++ show columns
        ++ " and indent " ++ show indent) $ do
        let expression = "buildSelectedResult firstArgument secondArgument thirdArgument fourthArgument"
            annotated = expression ++ " -- note"
            declarations =
              [ "result = if outerCondition"
              , "  then if condition"
              , "    then " ++ if elseBranch then "fallback" else annotated
              , "    else " ++ if elseBranch then annotated else "fallback"
              , "  else fallback"
              ]
        output <- checkedSource columns indent $ moduleSource declarations
        commentLine <- uniqueLine "-- note" output
        beforeMarker "-- note" commentLine
          `Hspec.shouldSatisfy` \prefix -> any (`List.isInfixOf` prefix)
            ["buildSelectedResult", "firstArgument", "secondArgument", "thirdArgument", "fourthArgument"]
        assertBrokenBranchIndentation output

  forM_ [2, 4] $ \indent -> do
    Hspec.it ("classifies both same-line branch comments by expression position at indent " ++ show indent) $ do
      output <- checkedSource 100 indent $ moduleSource
        ["result = if condition then firstValue {- then note -} else secondValue {- else note -}"]
      thenLine <- uniqueLine "{- then note -}" output
      elseLine <- uniqueLine "{- else note -}" output
      beforeMarker "{- then note -}" thenLine `Hspec.shouldContain` "firstValue"
      beforeMarker "{- else note -}" elseLine `Hspec.shouldContain` "secondValue"
      length (beforeMarker "{- then note -}" output)
        `Hspec.shouldSatisfy` (< length (beforeMarker "secondValue" output))
    Hspec.it ("keeps explicit leading comments before their branch expressions at indent " ++ show indent) $ do
      output <- checkedSource 80 indent $ moduleSource
        [ "result = if condition"
        , "  then -- leading then note"
        , "    firstValue"
        , "  else -- leading else note"
        , "    secondValue"
        ]
      _ <- uniqueLine "-- leading then note" output
      _ <- uniqueLine "-- leading else note" output
      length (beforeMarker "-- leading then note" output)
        `Hspec.shouldSatisfy` (< length (beforeMarker "firstValue" output))
      length (beforeMarker "-- leading else note" output)
        `Hspec.shouldSatisfy` (< length (beforeMarker "secondValue" output))
    Hspec.it ("keeps an internal argument comment with firstValue at indent " ++ show indent) $ do
      output <- checkedSource 80 indent $ moduleSource
        [ "result = if condition"
        , "  then combine firstValue -- argument note"
        , "    secondValue"
        , "  else fallback"
        ]
      commentLine <- uniqueLine "-- argument note" output
      beforeMarker "-- argument note" commentLine `Hspec.shouldContain` "firstValue"
      length (beforeMarker "-- argument note" output)
        `Hspec.shouldSatisfy` (< length (beforeMarker "secondValue" output))
    Hspec.it ("keeps distinct neighboring comments with their branches at indent " ++ show indent) $ do
      output <- checkedSource 80 indent $ moduleSource
        [ "result = if condition"
        , "  then firstValue -- then note"
        , "  else secondValue -- else note"
        ]
      thenLine <- uniqueLine "-- then note" output
      elseLine <- uniqueLine "-- else note" output
      beforeMarker "-- then note" thenLine `Hspec.shouldContain` "firstValue"
      beforeMarker "-- else note" elseLine `Hspec.shouldContain` "secondValue"
    Hspec.it ("retains a fitting uncommented conditional at indent " ++ show indent) $ do
      output <- checkedSource 80 indent $ moduleSource
        ["result = if condition then firstValue else secondValue"]
      output `Hspec.shouldContain` "result = if condition then firstValue else secondValue"

  forM_ [2, 4] $ \indent -> forM_ [False, True] $ \elseBranch ->
    Hspec.it ("retains an inline-seeded " ++ branchName elseBranch
      ++ " continuation at indent " ++ show indent) $ do
      let keyword = branchName elseBranch
          seedLine = "  " ++ keyword ++ " select value -- seed note"
          continuation = replicate (length $ beforeMarker "-- seed note" seedLine) ' '
            ++ "-- continuation note"
          declarations = ["result = if condition"] ++ if elseBranch
            then ["  then fallback", seedLine, continuation]
            else [seedLine, continuation, "  else fallback"]
      output <- checkedSource 80 indent $ moduleSource declarations
      seed <- uniqueLine "-- seed note" output
      continued <- uniqueLine "-- continuation note" output
      beforeMarker "-- seed note" seed `Hspec.shouldContain` "select value"
      length (beforeMarker "-- seed note" seed)
        `Hspec.shouldBe` length (beforeMarker "-- continuation note" continued)

  forM_ malformedBranches $ \(description, declarations) ->
    Hspec.it ("rejects " ++ description ++ " without replacing inplace input") $ do
      let source = moduleSource declarations
      directory <- Directory.getTemporaryDirectory
      Exception.bracket
        (do
          (path, handle) <- IO.openTempFile directory "brittany-inline-branch-invalid.hs"
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

uniqueLine :: String -> String -> IO String
uniqueLine marker output = case filter (List.isInfixOf marker) $ lines output of
  [line] -> pure line
  matches -> Hspec.expectationFailure
    ("expected one line containing " ++ show marker ++ ", found " ++ show matches)
    >> fail "missing or duplicated marker"

beforeMarker :: String -> String -> String
beforeMarker marker line = case
  [ prefix | (prefix, suffix) <- zip (List.inits line) (List.tails line)
           , marker `List.isPrefixOf` suffix ] of
    prefix : _ -> prefix
    [] -> line

assertBrokenBranchIndentation :: String -> IO ()
assertBrokenBranchIndentation output = forM_ (List.tails $ lines output) $ \case
  keywordLine : rest
    | dropWhile (== ' ') keywordLine `elem` ["then", "else"] ->
      case filter (not . null . dropWhile (== ' ')) rest of
        bodyLine : _ -> indentation bodyLine `Hspec.shouldSatisfy` (> indentation keywordLine)
        [] -> Hspec.expectationFailure "branch keyword has no body"
  _ -> pure ()
 where
  indentation = length . takeWhile (== ' ')

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
    parsed <- ParseModule.parseModule ["-haddock"] "InlineBranchComments.hs"
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
moduleSource declarations = unlines $ ["module InlineBranchComments where", ""] ++ declarations

branchName :: Bool -> String
branchName elseBranch = if elseBranch then "else" else "then"

branchSource :: Bool -> String -> [String]
branchSource elseBranch expression =
  [ "result = if condition"
  , "  then " ++ if elseBranch then "fallback" else expression
  , "  else " ++ if elseBranch then expression else "fallback"
  ]

commentForms :: [(String, String)]
commentForms = [("line", "-- branch note"), ("block", "{- branch note -}")]

malformedBranches :: [(String, [String])]
malformedBranches =
  [ ("a missing then expression",
      ["result = if condition", "  then -- branch note", "  else fallback"])
  , ("a missing else expression",
      ["result = if condition", "  then firstValue", "  else -- branch note"])
  ]
