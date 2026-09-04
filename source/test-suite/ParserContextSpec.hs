module ParserContextSpec (spec) where

import Data.Either (isLeft, isRight)
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "parser context reuse" $ do
  Hspec.it "parses formatted source without creating another GHC session" $ do
    contextResult <- parseWithContext "ParserContextExpected.hs"
      "module ParserContextExpected where\nvalue = 1\n"
    case contextResult of
      Left parseError -> Hspec.expectationFailure parseError
      Right context -> do
        firstParsed <- parseInContext context "FirstOutput.hs"
          "module ParserContextExpected where\nvalue = 2\n"
        secondParsed <- parseInContext context "SecondOutput.hs"
          "module ParserContextExpected where\nvalue = 3\n"
        firstParsed `Hspec.shouldSatisfy` isRight
        secondParsed `Hspec.shouldSatisfy` isRight

  Hspec.it "retains language pragmas from the original source" $ do
    contextResult <- parseWithContext "ParserContextPragma.hs" $ unlines
      [ "{-# LANGUAGE LambdaCase #-}"
      , "module ParserContextPragma where"
      , "value = \\case { True -> 1; False -> 0 }"
      ]
    case contextResult of
      Left parseError -> Hspec.expectationFailure parseError
      Right context -> do
        parsed <- parseInContext context "output" $ unlines
          [ "module ParserContextPragma where"
          , "value = \\case { True -> 1; False -> 0 }"
          ]
        parsed `Hspec.shouldSatisfy` isRight

  Hspec.it "retains forwarded GHC options" $ do
    parsed <- ParseModule.parseModuleWithMetricsAndContext Nothing
      ["-XLambdaCase"]
      "ParserContextOptions.hs"
      (const $ pure $ Right ())
      "module ParserContextOptions where\nvalue = \\case True -> 1; False -> 0\n"
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError
      Right (_, _, _, context) -> do
        output <- parseInContext context "output"
          "module ParserContextOptions where\nvalue = \\case True -> 2; False -> 0\n"
        output `Hspec.shouldSatisfy` isRight

  Hspec.it "remains usable after a malformed parse" $ do
    contextResult <- parseWithContext "ParserContextRecovery.hs"
      "module ParserContextRecovery where\nvalue = 1\n"
    case contextResult of
      Left parseError -> Hspec.expectationFailure parseError
      Right context -> do
        malformed <- parseInContext context "output" "module Broken where\nvalue ="
        malformed `Hspec.shouldSatisfy` isLeft
        recovered <- parseInContext context "output"
          "module ParserContextRecovery where\nvalue = 3\n"
        recovered `Hspec.shouldSatisfy` isRight

parseWithContext
  :: FilePath
  -> String
  -> IO (Either String ParseModule.ParserContext)
parseWithContext filename source = do
  parsed <- ParseModule.parseModuleWithMetricsAndContext Nothing [] filename
    (const $ pure $ Right ()) source
  pure $ fmap (\(_, _, _, context) -> context) parsed

parseInContext
  :: ParseModule.ParserContext
  -> FilePath
  -> String
  -> IO (Either String ())
parseInContext context filename source = fmap (fmap $ const ())
  $ ParseModule.parseModuleInContextWithMetrics Nothing context filename source
