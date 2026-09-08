{-# LANGUAGE LambdaCase #-}

module InfixBlockIndentationSpec (spec) where

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
spec projectRoot = Hspec.describe "infix block indentation" $ do
  Hspec.it "uses surrounding block indentation in both issue #192 examples" $ do
    output <- checkFormatting 2 minimalSource
    output `Hspec.shouldBe` List.intercalate "\n" (moduleLines ++
      [ "example = do"
      , "  (do"
      , "    work"
      , "    work"
      , "   ) `Exception.finally` do"
      , "    cleanup"
      , "    cleanup"
      , ""
      , "check = do"
      , "  normalizeCommentPlan (Map.singleton (nodeKeyAt \"Owner\" 2) annotation)"
      , "    `Hspec.shouldSatisfy` \\case"
      , "      Just _ -> True"
      , "      _      -> False"
      ])

  forM_ [2, 4] $ \indent -> forM_ [False, True] $ \longLeft ->
    forM_ blockKinds $ \(keyword, body, marker) ->
      Hspec.it
        ("keeps " ++ keyword ++ " body indentation independent of operator length"
          ++ " with " ++ (if longLeft then "a long" else "a short")
          ++ " LHS and indent " ++ show indent) $ do
          let left = if longLeft then longLeftOperand else "value"
              expectedIndent = (if longLeft then 3 else 2) * indent
          forM_ operators $ \operator -> do
            output <- checkFormatting indent $ moduleSource
              (["example = do", "  " ++ left ++ " " ++ operator ++ " " ++ keyword]
                ++ map ("    " ++) body)
            indentationOf marker output `Hspec.shouldReturn` expectedIndent

  forM_ ["do", "mdo"] $ \keyword ->
    Hspec.it ("preserves nested layout-sensitive statements inside " ++ keyword) $ do
      output <- checkFormatting 2 $ moduleSource
        [ "example = do"
        , "  " ++ longLeftOperand ++ " `Q.run` " ++ keyword
        , "    result <- do"
        , "      first <- pure value"
        , "      pure first"
        , "    pure result"
        ]
      indentationOf "result <-" output `Hspec.shouldReturn` 6
      indentationOf "first <-" output `Hspec.shouldReturn` 8

  forM_ (take 3 blockKinds) $ \(keyword, body, marker) ->
    Hspec.it ("keeps a flattened chain's final " ++ keyword ++ " RHS block-relative") $ do
      output <- checkFormatting 2 $ moduleSource
        ([ "example = do"
         , "  " ++ longLeftOperand ++ " >>= firstLongTransformation >>= " ++ keyword
         ] ++ map ("    " ++) body)
      indentationOf marker output `Hspec.shouldReturn` 6

  Hspec.it "preserves an explicit-brace intermediate do operand in a flattened chain" $ do
    output <- checkFormatting 2 $ moduleSource
      [ "example = " ++ longLeftOperand
          ++ " `Q.run` do { work; work } `Q.run` right"
      ]
    indentationOf "work" output `Hspec.shouldReturn` 6
    output `Hspec.shouldContain` "`Q.run` right"

  Hspec.it "keeps an empty lambda-case compact" $ do
    output <- checkFormatting 2 $ moduleSource ["example = value `Q.run` \\case {}"]
    output `Hspec.shouldContain` "example = value `Q.run` \\case {}"

  Hspec.it "preserves the existing parenthesized do RHS layout" $ do
    output <- checkFormatting 2 $ moduleSource
      [ "example = do"
      , "  startingActionWithLongName `Exception.finally` (do"
      , "    cleanup"
      , "    cleanup"
      , "    )"
      ]
    output `Hspec.shouldBe` List.intercalate "\n" (moduleLines ++
      [ "example = do"
      , "  startingActionWithLongName"
      , "    `Exception.finally` (do"
      , "      cleanup"
      , "      cleanup"
      , "    )"
      ])

  Hspec.it "preserves the existing parenthesized lambda-case RHS layout" $ do
    output <- checkFormatting 2 $ moduleSource
      [ "example = do"
      , "  startingActionWithLongName `Hspec.shouldSatisfy` (\\case"
      , "    Just value -> value"
      , "    Nothing -> fallback"
      , "    )"
      ]
    output `Hspec.shouldBe` List.intercalate "\n" (moduleLines ++
      [ "example = do"
      , "  startingActionWithLongName"
      , "    `Hspec.shouldSatisfy` (\\case"
      , "      Just value -> value"
      , "      Nothing    -> fallback"
      , "    )"
      ])

  forM_ (filter (\(keyword, _, _) -> keyword `elem` ["do", "\\case"]) blockKinds)
    $ \(keyword, body, marker) ->
    forM_ ["-- boundary note", "{- boundary note -}"] $ \comment ->
      Hspec.it ("preserves " ++ comment ++ " between an operator and " ++ keyword) $ do
        output <- checkFormatting 2 $ moduleSource
          ([ "example = do"
           , "  " ++ longLeftOperand ++ " `Q.run` " ++ comment
           , "    " ++ keyword
           ] ++ map ("      " ++) body)
        output `Hspec.shouldContain` ("`Q.run` " ++ comment)
        bodyIndent <- indentationOf marker output
        bodyIndent `Hspec.shouldSatisfy` (<= 8)

  Hspec.it "keeps the maintained CommandLineSpec cleanup body near its operator" $ do
    source <- readFile $ projectRoot </> "source/test-suite/CommandLineSpec.hs"
    output <- checkFormatting 2 source
    cleanupIndent <- indentationOf "IO.hFlush IO.stdout" output
    cleanupIndent `Hspec.shouldSatisfy` (<= 14)

  Hspec.it "keeps the maintained CommentPlanSpec lambda-case alternatives local" $ do
    source <- readFile $ projectRoot </> "source/test-suite/CommentPlanSpec.hs"
    output <- checkFormatting 2 source
    let alternatives = filter
          (List.isInfixOf "Left [AmbiguousCommentPlacement") $ lines output
    alternatives `Hspec.shouldSatisfy` (not . null)
    map leadingSpaces alternatives `Hspec.shouldSatisfy` all (<= 14)

  Hspec.it "rejects malformed block layout without replacing the inplace source" $ do
    let source = moduleSource
          [ "example = do"
          , "  value `Q.run` do"
          , "    cleanup"
          , "    <- broken"
          ]
    directory <- Directory.getTemporaryDirectory
    Exception.bracket
      (do
        (path, handle) <- IO.openTempFile directory "brittany-infix-block-invalid.hs"
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

checkFormatting :: Int -> String -> IO String
checkFormatting indent source = do
  let config = configWithIndent indent
  output <- formatChecked config source
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
    parsed <- ParseModule.parseModule ["-haddock"] "InfixBlockIndentation.hs"
      (const $ pure $ Right ()) source
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError >> fail parseError
      Right result -> pure result

configWithIndent :: Int -> Config
configWithIndent indent = staticDefaultConfig
  { _conf_layout = (_conf_layout staticDefaultConfig)
      { _lconfig_indentAmount = Identity $ Last indent }
  , _conf_errorHandling = (_conf_errorHandling staticDefaultConfig)
      { _econf_Werror = Identity $ Last True
      , _econf_failOnExactSourceFallback = Identity $ Last True
      }
  }

indentationOf :: String -> String -> IO Int
indentationOf marker source = case List.find (List.isInfixOf marker) $ lines source of
  Nothing -> Hspec.expectationFailure ("missing line containing " ++ show marker)
    >> fail "missing formatted line"
  Just line -> pure $ leadingSpaces line

leadingSpaces :: String -> Int
leadingSpaces = length . takeWhile (== ' ')

operators :: [String]
operators = ["`Q.op`", "`QualifiedOperator.longCleanupOperator`", "$"]

longLeftOperand :: String
longLeftOperand = "longLeftOperand" ++ replicate 60 'x'

blockKinds :: [(String, [String], String)]
blockKinds =
  [ ("do", ["cleanup", "cleanup"], "cleanup")
  , ("mdo", ["cleanup", "cleanup"], "cleanup")
  , ("\\case", ["Just _ -> True", "_ -> False"], "Just _")
  , ("case value of", ["Just _ -> True", "_ -> False"], "Just _")
  , ("if", ["| ready -> first", "| otherwise -> second"], "| ready")
  ]

moduleSource :: [String] -> String
moduleSource declarations = unlines $ moduleLines ++ declarations

moduleLines :: [String]
moduleLines =
  [ "{-# LANGUAGE BlockArguments #-}"
  , "{-# LANGUAGE LambdaCase #-}"
  , "{-# LANGUAGE RecursiveDo #-}"
  , "{-# LANGUAGE MultiWayIf #-}"
  , "{-# LANGUAGE EmptyCase #-}"
  , "module InfixBlockIndentation where"
  , ""
  ]

minimalSource :: String
minimalSource = moduleSource
  [ "example = do"
  , "  (do"
  , "    work"
  , "    work"
  , "    ) `Exception.finally` do"
  , "      cleanup"
  , "      cleanup"
  , ""
  , "check = do"
  , "  normalizeCommentPlan (Map.singleton (nodeKeyAt \"Owner\" 2) annotation)"
  , "    `Hspec.shouldSatisfy` \\case"
  , "      Just _ -> True"
  , "      _ -> False"
  ]
