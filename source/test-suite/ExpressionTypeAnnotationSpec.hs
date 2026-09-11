{-# LANGUAGE LambdaCase #-}

module ExpressionTypeAnnotationSpec (spec) where

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
spec projectRoot = Hspec.describe "expression type annotation wrapping" $ do
  forM_ ["unknownCounterScenario", "focused-not-a-phase"] $ \marker ->
    Hspec.it ("wraps the " ++ marker ++ " annotation in the complete PerformanceSpec module") $ do
      source <- readFile $ projectRoot </> "source/test-suite/PerformanceSpec.hs"
      let config = configWithLayout 80 2
      output <- formatChecked config source
      let blocks =
            [ takeWhile (not . List.isInfixOf "decoded `should") suffix
            | suffix@(first : _) <- List.tails $ lines output
            , "let decoded" `List.isInfixOf` first
                || "decoded =" `List.isPrefixOf` dropWhile (== ' ') first
            ]
          matching = filter (any $ List.isInfixOf marker) blocks
      length matching `Hspec.shouldBe` 1
      filter ((> 80) . length) (concat matching) `Hspec.shouldBe` []
      assertStableAndEquivalent config source output

  Hspec.it "keeps a fitting annotated expression compact" $ do
    output <- checkWithinColumns 40 2 $ moduleSource ["value = (1 :: Int)"]
    output `Hspec.shouldContain` "value = (1 :: Int)"

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ annotatedExpressions $ \(description, expression) ->
      Hspec.it
        ("wraps " ++ description ++ " at width " ++ show columns
          ++ " and indent " ++ show indent) $ do
          _ <- checkWithinColumns columns indent $ moduleSource ["value = " ++ expression]
          pure ()

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ annotatedFunctions $ \(description, expression) ->
      Hspec.it
        ("preserves " ++ description ++ " at width " ++ show columns
          ++ " and indent " ++ show indent) $ do
          _ <- checkWithinColumns columns indent $
            "{-# LANGUAGE ExplicitForAll #-}\n{-# LANGUAGE KindSignatures #-}\n" ++ moduleSource ["value = " ++ expression]
          pure ()

  forM_ [2, 4] $ \indent -> do
    Hspec.it ("wraps an annotation in a nested let at indent " ++ show indent) $ do
      _ <- checkWithinColumns 40 indent $ moduleSource
        [ "outer source ="
        , "  let decoded = collect firstInput source :: Either String ParsedValue"
        , "  in consume decoded"
        ]
      pure ()
    Hspec.it ("wraps an annotation in a nested case at indent " ++ show indent) $ do
      _ <- checkWithinColumns 40 indent $ moduleSource
        [ "outer source = case source of"
        , "  Just selected -> collect firstInput selected :: Either String ParsedValue"
        , "  Nothing -> fallback"
        ]
      pure ()

  forM_ commentedAnnotations $ \(description, declarations) ->
    Hspec.it ("preserves " ++ description) $ do
      _ <- checkWithinColumns 80 2 $ moduleSource declarations
      pure ()

  forM_ ["value = collect source ::", "value = (collect source :: Either String"] $ \declaration ->
    Hspec.it ("rejects a malformed annotation without replacing inplace input: " ++ declaration) $ do
      let source = moduleSource [declaration]
      directory <- Directory.getTemporaryDirectory
      Exception.bracket
        (do
          (path, handle) <- IO.openTempFile directory "brittany-expression-type-invalid.hs"
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
    parsed <- ParseModule.parseModule ["-haddock"] "ExpressionTypeAnnotations.hs"
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
moduleSource declarations = unlines $
  ["module ExpressionTypeAnnotations where", ""] ++ declarations

annotatedExpressions :: [(String, String)]
annotatedExpressions =
  [ ("an application annotation",
      "collect firstInput secondInput :: Either String ParsedValue")
  , ("an infix expression annotation",
      "collect firstInput <> collect secondInput :: Either String ParsedValue")
  , ("an annotation inside a parenthesized argument",
      "consume (collect firstInput secondInput :: Either String ParsedValue)")
  ]

annotatedFunctions :: [(String, String)]
annotatedFunctions =
  [ ("a function type annotation",
      "adaptFunction sourceFunction :: FirstArgument -> SecondArgument -> ResultValue")
  , ("a polymorphic constrained annotation",
      "selectedFunction :: forall a. Eq a => a -> a")
  , ("a kinded explicit binder", "selectedFunction :: forall (a :: Type). Proxy a -> Proxy a")
  , ("an inferred binder", "selectedFunction :: forall {a}. a -> a")
  , ("a kinded inferred binder", "selectedFunction :: forall {a :: Type}. Proxy a -> Proxy a")
  , ("an empty explicit forall", "selectedFunction :: forall. Int")
  , ("a multiline explicit binder list",
      "selectedFunction :: forall first second third fourth fifth sixth seventh eighth. first -> eighth")
  ]

commentedAnnotations :: [(String, [String])]
commentedAnnotations =
  [ ("a line comment before the double colon",
      [ "value = collect firstInput secondInput -- expression note"
      , "  :: Either String ParsedValue"
      ])
  , ("a line comment after the double colon",
      [ "value = collect firstInput secondInput :: -- type note"
      , "  Either String ParsedValue"
      ])
  ]
