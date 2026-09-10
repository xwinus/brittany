{-# LANGUAGE LambdaCase #-}

module NestedLambdaPatternSpec (spec) where

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
spec projectRoot = Hspec.describe "nested lambda patterns" $ do
  Hspec.it "keeps the reported tuple and arrow compact in the complete maintained module" $ do
    source <- readFile $ projectRoot </>
      "source/test-suite/InfixBlockIndentationSpec.hs"
    let config = configWithLayout 80 2
    output <- formatChecked config source
    let matching = compactPatternLines ["keyword", "body", "marker"] output
    length matching `Hspec.shouldSatisfy` (>= 2)
    map leadingSpaces matching `Hspec.shouldSatisfy` all (<= 16)
    map length matching `Hspec.shouldSatisfy` all (<= 80)
    assertStableAndEquivalent config source output

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ [1, 2] $ \depth -> forM_ [["key", "value"], ["key", "body", "mark"]] $ \names ->
      Hspec.it
        ("keeps a " ++ show (length names) ++ "-tuple compact at width "
          ++ show columns ++ ", indent " ++ show indent ++ ", depth " ++ show depth) $ do
          output <- checkWithinColumns columns indent $
            nestedSource depth "forM_ cases" "$" names
          assertCompactPattern names (min (columns `div` 2) ((depth + 3) * indent)) output

  forM_ ["$", "`Q.apply`", "<+>", "`Qualified.apply`"] $ \operator ->
    Hspec.it ("keeps the tuple compact with enclosing operator " ++ operator) $ do
      let names = ["firstValue", "secondValue", "thirdValue"]
      output <- checkWithinColumns 80 2 $
        nestedSource 2 "visit allSelectedCases" operator names
      assertCompactPattern names 16 output

  forM_ [2, 4] $ \indent ->
    Hspec.it ("retains a fitting compact application at indent " ++ show indent) $ do
      output <- checkWithinColumns 80 indent $ moduleSource
        ["example = forM_ xs $ \\(x, y) -> pure x"]
      output `Hspec.shouldContain` "example = forM_ xs $ \\(x, y) -> pure x"

  forM_ [False, True] $ \blockArgument ->
    Hspec.it ("allows a genuinely long tuple pattern to wrap as "
      ++ if blockArgument then "a block argument" else "an operator RHS") $ do
      let names = ["firstLongPatternComponent", "secondLongPatternComponent", "thirdLongPatternComponent"]
          call = if blockArgument then "consume " else "forM_ xs $ "
      output <- checkWithinColumns 40 2 $ moduleSource
        ["example = " ++ call ++ "\\" ++ tuplePattern names ++ " -> pure firstLongPatternComponent"]
      compactPatternLines names output `Hspec.shouldBe` []
      forM_ names $ \name -> output `Hspec.shouldContain` name

  forM_
    [ ("lambda", ["    forM_ cases $ {- lambda note -}", "      \\(key, value) -> do"])
    , ("comma", ["    forM_ cases $ \\(key, -- comma note", "      value) -> do"])
    , ("arrow", ["    forM_ cases $ \\(key, value) -> -- arrow note", "      do"])
    ] $ \(boundary, lambdaLines) ->
      Hspec.it ("preserves the comment at the " ++ boundary ++ " boundary") $ do
        output <- checkWithinColumns 80 2 $ moduleSource
          (["example = do", "  forM_ outerValues $ \\outer ->"]
            ++ lambdaLines ++ ["        run key", "        run value"])
        output `Hspec.shouldContain` (boundary ++ " note")

  Hspec.it "preserves an indivisible long pattern token without looping" $ do
    let token = 'p' : replicate 90 'x'
        source = moduleSource ["example = forM_ xs $ \\" ++ token ++ " -> pure ()"]
        config = configWithLayout 40 2
    output <- formatChecked config source
    output `Hspec.shouldContain` token
    filter ((> 40) . length) (lines output) `Hspec.shouldSatisfy`
      all (List.isInfixOf token)
    assertStableAndEquivalent config source output

  Hspec.it "rejects a malformed lambda without replacing the inplace input" $ do
    let source = moduleSource ["example = forM_ xs $ \\(key, value) ->"]
    directory <- Directory.getTemporaryDirectory
    Exception.bracket
      (do
        (path, handle) <- IO.openTempFile directory "brittany-nested-lambda-invalid.hs"
        IO.hPutStr handle source
        IO.hClose handle
        pure path)
      Directory.removeFile
      $ \path -> do
        Brittany.mainWith "brittany"
          [ "--no-user-config"
          , "--write-mode", "inplace"
          , "--werror"
          , "--fail-on-fallback"
          , path
          ] `Hspec.shouldThrow` (== Exit.ExitFailure 60)
        TextIO.readFile path `Hspec.shouldReturn` Text.pack source

checkWithinColumns :: Int -> Int -> String -> IO String
checkWithinColumns columns indent source = do
  let config = configWithLayout columns indent
  output <- formatChecked config source
  filter ((> columns) . length) (lines output) `Hspec.shouldBe` []
  assertStableAndEquivalent config source output
  pure output

assertCompactPattern :: [String] -> Int -> String -> IO ()
assertCompactPattern names maximumIndent output = do
  let matching = compactPatternLines names output
  length matching `Hspec.shouldBe` 1
  map leadingSpaces matching `Hspec.shouldSatisfy` all (<= maximumIndent)

compactPatternLines :: [String] -> String -> [String]
compactPatternLines names = filter (List.isInfixOf marker . filter (/= ' ')) . lines
 where
  marker = "\\(" ++ List.intercalate "," names ++ ")->"

nestedSource :: Int -> String -> String -> [String] -> String
nestedSource depth innerCall operator names = moduleSource $
  ["example = do"]
  ++ [replicate (2 * level) ' ' ++ "forM_ outerValues $ \\outer" ++ show level ++ " ->"
     | level <- [1 .. depth]]
  ++ [ replicate (2 * (depth + 1)) ' ' ++ innerCall ++ " " ++ operator
         ++ " \\" ++ tuplePattern names ++ " -> do"
     , replicate (2 * (depth + 2)) ' ' ++ "run " ++ head names
     , replicate (2 * (depth + 2)) ' ' ++ "run " ++ last names
     ]

tuplePattern :: [String] -> String
tuplePattern names = "(" ++ List.intercalate ", " names ++ ")"

moduleSource :: [String] -> String
moduleSource declarations = unlines $ ["{-# LANGUAGE BlockArguments #-}", "module NestedLambdaPattern where", ""] ++ declarations

leadingSpaces :: String -> Int
leadingSpaces = length . takeWhile (== ' ')

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
    parsed <- ParseModule.parseModule ["-haddock"] "NestedLambdaPattern.hs"
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

