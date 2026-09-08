{-# LANGUAGE LambdaCase #-}

module InfixRhsWrappingSpec (spec) where

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
spec projectRoot = Hspec.describe "infix RHS wrapping" $ do
  Hspec.it "fits the issue #191 singleton list by breaking after the operator" $ do
    output <- checkWithinColumns 80 2 $ moduleSource
      [ "example = do"
      , "  result <- pure []"
      , "  result"
      , "    `shouldContain`"
      , "      [ " ++ show compatibilityMessage
      , "      ]"
      ]
    assertSeparateRhs "`shouldContain`" output

  Hspec.it "fits the maintained CompatibilitySpec assertion at its nesting depth" $ do
    output <- checkWithinColumns 80 2 $ moduleSource
      [ "example = do"
      , "  Hspec.describe \"compatibility\" $ do"
      , "    Hspec.describe \"matrix\" $ do"
      , "      Hspec.it \"requires pragmas\" $ do"
      , "        Matrix.validateMatrix invalidMatrix discoveredPragmas"
      , "          `Hspec.shouldContain`"
      , "            [ " ++ show compatibilityMessage
      , "            ]"
      ]
    assertSeparateRhs "`Hspec.shouldContain`" output

  Hspec.it "fits every body line in the complete maintained CompatibilitySpec module" $ do
    source <- readFile $ projectRoot </> "source/test-suite/CompatibilitySpec.hs"
    let config = configWithLayout 80 2
    output <- formatChecked config source
    let body = dropWhile (not . List.isPrefixOf "spec ::") $ lines output
    body `Hspec.shouldSatisfy` (not . null)
    filter ((> 80) . length) body `Hspec.shouldBe` []
    assertStableAndEquivalent config source output

  Hspec.it "fits the shorter singleton assertion inside nested case and do blocks" $ do
    output <- checkWithinColumns 80 2 $ nestedAssertionSource
      $ show "feature has no compatibility case: ModuleHeaders"
    assertSeparateRhs "`Hspec.shouldContain`" output

  Hspec.it "fits a concatenated singleton element inside nested case and do blocks" $ do
    output <- checkWithinColumns 80 2 $ nestedAssertionSource
      $ show "case references unknown feature UnclassifiedFeature: "
        ++ " ++ Matrix.matrixCaseName firstCase"
    assertSeparateRhs "`Hspec.shouldContain`" output

  forM_ [41, 42, 43, 48, 52, 55] $ \literalLength ->
    forM_ [False, True] $ \compound ->
      Hspec.it
        ("fits a nested " ++ (if compound then "compound" else "plain")
          ++ " list element with literal length " ++ show literalLength) $ do
          let element = show (replicate literalLength 'x')
                ++ if compound then " ++ Matrix.matrixCaseName firstCase" else ""
          _ <- checkWithinColumns 80 2 $ nestedAssertionSource element
          pure ()

  forM_ [0, 1] $ \overflow ->
    Hspec.it
      (if overflow == 0 then "keeps an operator and RHS exactly at the width limit"
        else "breaks after the operator when its RHS exceeds the limit by one") $ do
        let literalLength = 80 - length "    `shouldContain` [\"\"]" + overflow
        output <- checkWithinColumns 80 2 $
          assertionSource "`shouldContain`" [replicate literalLength 'x']
        if overflow == 0
          then do
            output `Hspec.shouldSatisfy` (any ((== 80) . length) . lines)
            operatorLine "`shouldContain`" output
              `Hspec.shouldSatisfy` List.isInfixOf "["
          else assertSeparateRhs "`shouldContain`" output

  forM_ [2, 4] $ \indent -> forM_ [40, 80] $ \columns -> do
    forM_ ["`shouldContain`", "`Hspec.shouldContain`", "<++++++++++++>"] $ \operator ->
      forM_ [False, True] $ \multiple ->
        Hspec.it
          ("fits " ++ operator ++ " with "
            ++ (if multiple then "multiple list elements" else "a singleton list")
            ++ " at width " ++ show columns ++ " and indent " ++ show indent) $ do
            let literal = replicate (columns - 3 * indent - 6) 'x'
                elements = literal : ["tail" | multiple]
            _ <- checkWithinColumns columns indent $ assertionSource operator elements
            pure ()

    Hspec.it
      ("wraps a flattened operator chain at width " ++ show columns
        ++ " and indent " ++ show indent) $ do
        let literal = replicate (columns - 3 * indent - 6) 'x'
        _ <- checkWithinColumns columns indent $ moduleSource
          [ "example = do"
          , "  prefix <++++++++++++> [" ++ show literal
              ++ "] <++++++++++++> [" ++ show literal ++ "]"
          ]
        pure ()

  forM_ [2, 4] $ \indent ->
    Hspec.it ("keeps a short operator attached at indent " ++ show indent) $ do
      let literal = replicate (40 - 3 * indent - 6) 'x'
      output <- checkWithinColumns 40 indent $ assertionSource "$" [literal]
      operatorLine "$" output `Hspec.shouldSatisfy` List.isInfixOf "["

  Hspec.it "keeps a short assertion compact" $ do
    output <- checkWithinColumns 80 2 $ assertionSource "`shouldContain`" ["small"]
    output `Hspec.shouldBe` List.intercalate "\n"
      [ "module InfixRhsWrapping where"
      , ""
      , "example = do"
      , "  result `shouldContain` [\"small\"]"
      ]

  Hspec.it "breaks before a parenthesized RHS list without removing parentheses" $ do
    let list = "([" ++ show compatibilityMessage ++ "])"
    output <- checkWithinColumns 80 2 $ moduleSource
      [ "example = do"
      , "  result `shouldContain` " ++ list
      ]
    assertSeparateRhs "`shouldContain`" output
    output `Hspec.shouldContain` list

  Hspec.it "preserves an indivisible literal wider than the configured width" $ do
    let literal = replicate 100 'x'
        source = assertionSource "`shouldContain`" [literal]
        config = configWithLayout 40 2
    output <- formatChecked config source
    output `Hspec.shouldContain` show literal
    filter ((> 40) . length) (lines output)
      `Hspec.shouldSatisfy` \overflow ->
        length overflow == 1 && all (List.isInfixOf $ show literal) overflow
    assertStableAndEquivalent config source output

  forM_ ["-- RHS boundary", "{- RHS boundary -}"] $ \comment ->
    Hspec.it ("preserves an operator/RHS boundary comment " ++ comment) $ do
      output <- checkWithinColumns 80 2 $ moduleSource
        [ "example = do"
        , "  result `shouldContain` " ++ comment
        , "    [" ++ show compatibilityMessage ++ "]"
        ]
      output `Hspec.shouldContain` ("`shouldContain` " ++ comment)

  Hspec.it "rejects a missing RHS without replacing the inplace source" $ do
    let source = moduleSource ["example = result `shouldContain`"]
    directory <- Directory.getTemporaryDirectory
    Exception.bracket
      (do
        (path, handle) <- IO.openTempFile directory "brittany-infix-rhs-invalid.hs"
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
    parsed <- ParseModule.parseModule ["-haddock"] "InfixRhsWrapping.hs"
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

operatorLine :: String -> String -> String
operatorLine operator = List.intercalate "\n" . filter (List.isInfixOf operator) . lines

assertSeparateRhs :: String -> String -> IO ()
assertSeparateRhs operator output = do
  let line = operatorLine operator output
  line `Hspec.shouldContain` operator
  line `Hspec.shouldNotContain` "["

assertionSource :: String -> [String] -> String
assertionSource operator elements = moduleSource
  [ "example = do"
  , "  result " ++ operator ++ " [" ++ List.intercalate ", " (map show elements) ++ "]"
  ]

nestedAssertionSource :: String -> String
nestedAssertionSource element = moduleSource
  [ "example = Hspec.describe \"matrix\" $ do"
  , "  case loaded of"
  , "    Right (matrix, discoveredPragmas) -> do"
  , "      Hspec.describe \"manifest validation\" $ do"
  , "        Hspec.it \"requires coverage\" $ do"
  , "          Matrix.validateMatrix invalidMatrix discoveredPragmas"
  , "            `Hspec.shouldContain`"
  , "              [" ++ element ++ "]"
  ]

moduleSource :: [String] -> String
moduleSource declarations = unlines $ ["module InfixRhsWrapping where", ""] ++ declarations

compatibilityMessage :: String
compatibilityMessage = "case does not enable feature ModuleHeaders in data/Test132.hs"
