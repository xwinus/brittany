{-# LANGUAGE LambdaCase #-}

module InlineCommentSpacingSpec (spec) where

import qualified Control.Exception as Exception
import Control.Monad (forM_)
import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import Data.Char (isSpace)
import Data.Functor.Identity (Identity(..), runIdentity)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Semigroup (Last(..))
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Builder as Text.Builder
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
import Language.Haskell.Brittany.Internal.BackendUtils
  ( layoutWriteAppend
  , layoutWriteBlankLine
  , finishPriorCommentLineState
  , resumeInlineCommentState
  )
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SemanticFingerprint
  ( compareSemanticSyntax
  )
import Language.Haskell.Brittany.Internal.Types (LayoutState(..))
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "inline comment spacing" $ do
  Hspec.it "separates the nested do comment in the issue #188 reproducer" $ do
    output <- formatChecked (configWithIndent 2) minimalSource
    output `Hspec.shouldBe` List.intercalate "\n"
      [ "module NestedDoComment where"
      , ""
      , "example ="
      , "  let"
      , "    parser ="
      , "      do -- first comment line"
      , "        pure ()"
      , "  in  parser"
      ]
    assertStableAndEquivalent (configWithIndent 2) minimalSource output

  forM_ ["do", "mdo"] $ \keyword ->
    forM_ [False, True] $ \nested ->
      forM_ [2, 4] $ \indent ->
        Hspec.it
          ("separates " ++ (if nested then "multiply nested " else "top-level ")
            ++ keyword ++ " comments at indent " ++ show indent) $ do
            let source = blockSource keyword nested
                config = configWithIndent indent
            output <- formatChecked config source
            assertInlineSeparated keyword "-- block comment" output
            assertStableAndEquivalent config source output

  Hspec.it "keeps an aligned continuation and the first do statement on separate lines" $ do
    let source = unlines
          [ "module DoCommentContinuation where"
          , ""
          , "example ="
          , "  let parser = do -- first comment line"
          , "                  -- continuation comment"
          , "        pure ()"
          , "  in parser"
          ]
        config = configWithIndent 2
    output <- formatChecked config source
    assertInlineSeparated "do" "-- first comment line" output
    let seedColumns = [length $ takeWhile (/= '-') line
          | line <- lines output, "-- first comment line" `List.isInfixOf` line]
        continuationColumns = [length $ takeWhile (/= '-') line
          | line <- lines output, "-- continuation comment" `List.isInfixOf` line]
    continuationColumns `Hspec.shouldBe` seedColumns
    filter ((== "pure ()") . dropWhile isSpace) (lines output)
      `Hspec.shouldSatisfy` ((== 1) . length)
    assertStableAndEquivalent config source output

  Hspec.it "does not swallow or duplicate the statement following an inline line comment" $ do
    let source = unlines
          [ "module AdversarialDoComment where"
          , ""
          , "example ="
          , "  let parser = do -- putStrLn \"comment only\""
          , "        putStrLn \"first statement\""
          , "        pure ()"
          , "  in parser"
          ]
        config = configWithIndent 4
    output <- formatChecked config source
    assertInlineSeparated "do" "-- putStrLn \"comment only\"" output
    filter ((== "putStrLn \"first statement\"") . dropWhile isSpace)
      (lines output) `Hspec.shouldSatisfy` ((== 1) . length)
    assertStableAndEquivalent config source output

  forM_ [Left 19, Right 1] $ \cursor ->
    forM_ [Nothing, Just 8] $ \anchor ->
      Hspec.it
        ("keeps a token gap for cursor " ++ show cursor
          ++ " and comment anchor " ++ show anchor) $ do
          let initial = layoutState cursor anchor
              result = resumeInlineCommentState 1 initial
          _lstate_addSepSpace result `Hspec.shouldBe` Just 1
          _lstate_curYOrAddNewline result `Hspec.shouldBe` case cursor of
            Left column -> Left column
            Right _ -> Right 0
          _lstate_commentCol result `Hspec.shouldBe` case anchor of
            Just column -> Just column
            Nothing -> Just $ case cursor of
              Left column -> column + 2
              Right _ -> 4
          _lstate_commentNewlines result `Hspec.shouldBe` 3

  Hspec.it "accounts for embedded newlines while resuming an inline comment" $ do
    let result = resumeInlineCommentState 3 $ layoutState (Right 1) Nothing
    _lstate_addSepSpace result `Hspec.shouldBe` Just 1
    _lstate_commentNewlines result `Hspec.shouldBe` 5

  Hspec.it "restores the line boundary after resuming an inline line comment" $ do
    let resumed = resumeInlineCommentState 1 $ layoutState (Right 1) $ Just 8
        result = finishPriorCommentLineState resumed
    _lstate_curYOrAddNewline result `Hspec.shouldBe` Right 1
    _lstate_commentCol result `Hspec.shouldBe` Just 8

  Hspec.it "tracks the physical column when an inline write cancels a pending newline" $ do
    let initial = (layoutState (Right 0) Nothing)
          { _lstate_lastWrittenColumn = 5 }
        rendered :: (LayoutState, Text.Builder.Builder)
        rendered = runIdentity $ MultiRWSS.runMultiRWSTNil
          $ MultiRWSS.withMultiWriterAW
          $ MultiRWSS.withMultiStateS initial
          $ do
            layoutWriteAppend $ Text.pack "x"
            pure ()
        (state, builder) = rendered
    _lstate_lastWrittenColumn state `Hspec.shouldBe` 8
    Text.Builder.toLazyText builder `Hspec.shouldBe` TextL.pack "  x"

  Hspec.it "resets the physical column when pending newlines are written" $ do
    let initial = (layoutState (Right 2) Nothing)
          { _lstate_lastWrittenColumn = 17 }
        rendered :: (LayoutState, Text.Builder.Builder)
        rendered = runIdentity $ MultiRWSS.runMultiRWSTNil
          $ MultiRWSS.withMultiWriterAW
          $ MultiRWSS.withMultiStateS initial
          $ do
            layoutWriteAppend $ Text.pack "x"
            pure ()
        (state, builder) = rendered
    _lstate_lastWrittenColumn state `Hspec.shouldBe` 3
    Text.Builder.toLazyText builder `Hspec.shouldBe` TextL.pack "\n\n  x"

  Hspec.it "does not retain the previous column after an explicit blank line" $ do
    let initial = (layoutState (Right 2) Nothing)
          { _lstate_lastWrittenColumn = 17 }
        rendered :: (LayoutState, Text.Builder.Builder)
        rendered = runIdentity $ MultiRWSS.runMultiRWSTNil
          $ MultiRWSS.withMultiWriterAW
          $ MultiRWSS.withMultiStateS initial
          $ do
            layoutWriteBlankLine
            layoutWriteAppend $ Text.pack "x"
            pure ()
        (state, builder) = rendered
    _lstate_lastWrittenColumn state `Hspec.shouldBe` 1
    Text.Builder.toLazyText builder `Hspec.shouldBe` TextL.pack "\n\nx"

  Hspec.it "rejects malformed do input without replacing its inplace source" $ do
    directory <- Directory.getTemporaryDirectory
    Exception.bracket
      (do
        (path, handle) <- IO.openTempFile directory "brittany-inline-comment-invalid.hs"
        IO.hPutStr handle malformedSource
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
        TextIO.readFile path `Hspec.shouldReturn` Text.pack malformedSource

layoutState :: Either Int Int -> Maybe Int -> LayoutState
layoutState cursor anchor = LayoutState
  { _lstate_baseYs = [4]
  , _lstate_curYOrAddNewline = cursor
  , _lstate_lastWrittenColumn = 0
  , _lstate_indLevels = [4]
  , _lstate_indLevelLinger = 4
  , _lstate_comments = Map.empty
  , _lstate_emittedComments = Set.empty
  , _lstate_trailingCommentRun = Nothing
  , _lstate_commentCol = anchor
  , _lstate_addSepSpace = Just 2
  , _lstate_commentNewlines = 3
  }

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
    parsed <- ParseModule.parseModule ["-haddock"] "InlineCommentSpacing.hs"
      (const $ pure $ Right ()) source
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError >> fail parseError
      Right result -> pure result

assertInlineSeparated :: String -> String -> String -> IO ()
assertInlineSeparated keyword comment source = case
    [ prefix
    | line <- lines source
    , (prefix, suffix) <- zip (List.inits line) (List.tails line)
    , suffix == comment
    ] of
  [prefix] -> do
    prefix `Hspec.shouldSatisfy` (not . null)
    last prefix `Hspec.shouldSatisfy` isSpace
    reverse (dropWhile isSpace $ reverse prefix)
      `Hspec.shouldSatisfy` List.isSuffixOf keyword
  matches -> Hspec.expectationFailure
    ("expected one inline " ++ show comment ++ ", found " ++ show matches)

configWithIndent :: Int -> Config
configWithIndent indent = staticDefaultConfig
  { _conf_layout = (_conf_layout staticDefaultConfig)
      { _lconfig_cols = Identity $ Last 80
      , _lconfig_indentAmount = Identity $ Last indent
      }
  , _conf_errorHandling = (_conf_errorHandling staticDefaultConfig)
      { _econf_Werror = Identity $ Last True
      , _econf_failOnExactSourceFallback = Identity $ Last True
      }
  }

blockSource :: String -> Bool -> String
blockSource keyword nested = unlines $
  [ "{-# LANGUAGE RecursiveDo #-}"
  , "module BlockComment where"
  , ""
  ] ++ if nested
    then
      [ "example = do"
      , "  let parser = do"
      , "        let action = " ++ keyword ++ " -- block comment"
      , "              pure ()"
      , "        action"
      , "  parser"
      ]
    else
      [ "example = " ++ keyword ++ " -- block comment"
      , "  pure ()"
      ]

minimalSource :: String
minimalSource = unlines
  [ "module NestedDoComment where"
  , ""
  , "example ="
  , "  let parser = do -- first comment line"
  , "        pure ()"
  , "  in parser"
  ]

malformedSource :: String
malformedSource = unlines
  [ "module MalformedDoComment where"
  , ""
  , "example ="
  , "  let parser = do -- keep the invalid statement"
  , "        value <-"
  , "  in parser"
  ]
