module ParserContextSpec (spec) where

import Data.Either (isLeft, isRight)
import qualified GHC.Driver.Session as GHC
import qualified GHC.LanguageExtensions.Type as GHC
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.Performance
  ( PerformanceCollector
  , PerformancePhase(..)
  , newPerformanceCollector
  , phaseSamplePhase
  , readPerformanceSamples
  )
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

  Hspec.it "parses several files in one scoped session" $ do
    ParseModule.withParserSession $ \session -> do
      first <- parseInSession session [] "BatchFirst.hs"
        "module BatchFirst where\nvalue = 1\n"
      second <- parseInSession session [] "BatchSecond.hs"
        "module BatchSecond where\nvalue = 2\n"
      first `Hspec.shouldSatisfy` isRight
      second `Hspec.shouldSatisfy` isRight

  Hspec.it "records one GHC session for a multi-file batch" $ do
    collector <- newPerformanceCollector
    ParseModule.withParserSessionWithMetrics (Just collector) $ \session -> do
      _ <- parseInMeasuredSession collector session "MeasuredFirst.hs"
      _ <- parseInMeasuredSession collector session "MeasuredSecond.hs"
      pure ()
    phases <- fmap phaseSamplePhase <$> readPerformanceSamples collector
    length (filter (== GhcSession) phases) `Hspec.shouldBe` 1
    length (filter (== GhcSessionSetup) phases) `Hspec.shouldBe` 2
    length (filter (== DynamicFlagParsing) phases) `Hspec.shouldBe` 2

  Hspec.it "does not leak source language pragmas between files" $ do
    ParseModule.withParserSession $ \session -> do
      withPragma <- unboxedTuplesEnabled session [] "WithPragma.hs" $ unlines
        [ "{-# LANGUAGE UnboxedTuples #-}"
        , "module WithPragma where"
        , "value = (# 1, 2 #)"
        ]
      withoutPragma <- unboxedTuplesEnabled session [] "WithoutPragma.hs" $ unlines
        [ "module WithoutPragma where"
        , "value = 2"
        ]
      recovered <- parseInSession session [] "AfterPragma.hs"
        "module AfterPragma where\nvalue = 3\n"
      withPragma `Hspec.shouldBe` Right True
      withoutPragma `Hspec.shouldBe` Right False
      recovered `Hspec.shouldSatisfy` isRight

  Hspec.it "does not leak forwarded options between files" $ do
    ParseModule.withParserSession $ \session -> do
      enabled <- unboxedTuplesEnabled session ["-XUnboxedTuples"]
        "OptionEnabled.hs" "module OptionEnabled where\nvalue = 1\n"
      disabled <- unboxedTuplesEnabled session [] "OptionDisabled.hs"
        "module OptionDisabled where\nvalue = 2\n"
      enabled `Hspec.shouldBe` Right True
      disabled `Hspec.shouldBe` Right False

  Hspec.it "recovers after an invalid option in the shared session" $ do
    ParseModule.withParserSession $ \session -> do
      invalid <- parseInSession session ["-XNotARealExtension"] "InvalidOption.hs"
        "module InvalidOption where\nvalue = 1\n"
      recovered <- parseInSession session [] "ValidAfterOption.hs"
        "module ValidAfterOption where\nvalue = 2\n"
      invalid `Hspec.shouldSatisfy` isLeft
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

parseInSession
  :: ParseModule.ParserSession
  -> [String]
  -> FilePath
  -> String
  -> IO (Either String ())
parseInSession session options filename source = fmap (fmap $ const ())
  $ ParseModule.parseModuleInSessionWithMetrics Nothing session options filename
    (const $ pure $ Right ()) source

unboxedTuplesEnabled
  :: ParseModule.ParserSession
  -> [String]
  -> FilePath
  -> String
  -> IO (Either String Bool)
unboxedTuplesEnabled session options filename source = fmap (fmap extractResult)
  $ ParseModule.parseModuleInSessionWithMetrics Nothing session options filename
    (pure . Right . GHC.xopt GHC.UnboxedTuples) source
 where
  extractResult (_, _, enabled, _) = enabled

parseInMeasuredSession
  :: PerformanceCollector
  -> ParseModule.ParserSession
  -> FilePath
  -> IO (Either String ())
parseInMeasuredSession collector session filename = fmap (fmap $ const ())
  $ ParseModule.parseModuleInSessionWithMetrics (Just collector) session []
    filename (const $ pure $ Right ())
    ("module " ++ takeWhile (/= '.') filename ++ " where\nvalue = 1\n")
