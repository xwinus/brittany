{-# LANGUAGE LambdaCase #-}

module FunctionHeadWrappingSpec (spec) where

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
spec projectRoot = Hspec.describe "function head wrapping" $ do
  Hspec.it "wraps the all-variable head in the issue #189 reproducer" $ do
    source <- readFile $ projectRoot </> "source/test-suite/fixtures"
      </> "FunctionHeadWrappingInput.hs"
    output <- checkWithinColumns 80 2 source
    output `Hspec.shouldContain` "pPrintModulePreparedMeasured\n"

  Hspec.it "keeps a short all-variable equation compact" $ do
    let source = unlines ["module ShortHead where", "", "combine left right = left"]
    output <- checkWithinColumns 80 2 source
    output `Hspec.shouldBe` List.intercalate "\n"
      ["module ShortHead where", "", "combine left right = left"]

  forM_ [2, 4] $ \indent -> forM_ [40, 80] $ \columns ->
    forM_ mixedHeads $ \(description, headSource) ->
      Hspec.it
        ("wraps " ++ description ++ " at width " ++ show columns
          ++ " and indent " ++ show indent) $ do
          let source = moduleSource [headSource ++ " = firstArgument"]
          _ <- checkWithinColumns columns indent source
          pure ()

  Hspec.it "wraps arguments after a long function name that fits the limit" $ do
    let name = "functionWithAnUnusuallyLongButStillFittingNameForThisBinding"
        source = moduleSource
          [name ++ " firstArgument secondArgument thirdArgument = firstArgument"]
    output <- checkWithinColumns 80 2 source
    output `Hspec.shouldContain` (name ++ "\n")

  Hspec.it "keeps an unavoidable long token intact while wrapping its arguments" $ do
    let name = "function" ++ replicate 80 'x'
        source = moduleSource
          [name ++ " firstArgument secondArgument thirdArgument = firstArgument"]
        config = configWithLayout 40 2
    output <- formatChecked config source
    filter ((> 40) . length) (lines output) `Hspec.shouldBe` [name]
    assertStableAndEquivalent config source output

  Hspec.it "breaks around an unavoidable long argument token" $ do
    let argument = "argument" ++ replicate 80 'x'
        source = moduleSource
          [ "applyArguments firstArgument " ++ argument
              ++ " secondArgument thirdArgument = firstArgument"
          ]
        config = configWithLayout 40 2
    output <- formatChecked config source
    map (dropWhile (== ' ')) (filter ((> 40) . length) $ lines output)
      `Hspec.shouldBe` [argument]
    output `Hspec.shouldContain` "applyArguments\n"
    assertStableAndEquivalent config source output

  Hspec.it "keeps a short guarded head compact despite an unavoidable long RHS" $ do
    let value = "unavoidable" ++ replicate 80 'x'
        source = moduleSource
          [ "short value"
          , "  | value = " ++ value
          , "  | otherwise = value"
          ]
        config = configWithLayout 40 2
    output <- formatChecked config source
    output `Hspec.shouldContain` "short value"
    filter ((> 40) . length) (lines output)
      `Hspec.shouldSatisfy` \overflow ->
        length overflow == 1 && all (List.isInfixOf value) overflow
    assertStableAndEquivalent config source output

  forM_ [False, True] $ \multiple ->
    Hspec.it
      ("wraps heads with " ++ (if multiple then "multiple guards" else "one guard")) $ do
        let source = moduleSource $
              [ atomicHead
              , "  | firstArgument = secondArgument"
              ] ++ if multiple then ["  | otherwise = thirdArgument"] else []
        _ <- checkWithinColumns 40 2 source
        pure ()

  Hspec.it "keeps a where clause attached to its wrapped equation" $ do
    let source = moduleSource
          [ atomicHead ++ " = selected"
          , "  where"
          , "    selected = firstArgument"
          ]
    _ <- checkWithinColumns 40 4 source
    pure ()

  Hspec.it "wraps a local function head inside a let binding" $ do
    let source = moduleSource
          [ "outer ="
          , "  let " ++ atomicHead ++ " = firstArgument"
          , "  in applyArguments"
          ]
    _ <- checkWithinColumns 40 2 source
    pure ()

  Hspec.it "preserves comments between arguments when wrapping the head" $ do
    let source = moduleSource
          [ "applyArguments firstArgument -- argument note"
          , "    secondArgument {- keep this note -} thirdArgument"
          , "    fourthArgument fifthArgument sixthArgument = firstArgument"
          ]
    _ <- checkWithinColumns 40 2 source
    pure ()

  Hspec.it "keeps commented invisible type arguments adjacent to the next argument" $ do
    let source = typeAbstractionSource
          [ "applyArguments @typeArgument -- type note"
          , "  firstArgument secondArgument thirdArgument = firstArgument"
          ]
    output <- checkWithinColumns 40 2 source
    let equation = unlines $ dropWhile
          (not . List.isPrefixOf "applyArguments") $ lines output
    equation `Hspec.shouldSatisfy` (not . null)
    equation `Hspec.shouldNotContain` "\n\n"

  Hspec.it "keeps the equals sign after a final type argument line comment" $ do
    let source = typeAbstractionSource
          [ "applyArguments @firstType @secondType @typeArgument -- type note"
          , "  = ()"
          ]
    output <- checkWithinColumns 40 2 source
    case filter (List.isInfixOf "-- type note") $ lines output of
      [commentLine] -> commentLine `Hspec.shouldNotContain` "="
      comments -> Hspec.expectationFailure
        ("expected one type argument comment, found " ++ show comments)
    output `Hspec.shouldContain` "="

  Hspec.it "preserves infix equation syntax and parenthesized operands" $ do
    let source = moduleSource
          [ "(left : rest) <+> (right : more) = (left, right)"
          ]
    output <- checkWithinColumns 80 2 source
    output `Hspec.shouldContain` "(left : rest) <+> (right : more)"

  Hspec.it "rejects a missing RHS without replacing the inplace source" $ do
    let source = moduleSource [atomicHead ++ " ="]
    directory <- Directory.getTemporaryDirectory
    Exception.bracket
      (do
        (path, handle) <- IO.openTempFile directory "brittany-function-head-invalid.hs"
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
    parsed <- ParseModule.parseModule ["-haddock"] "FunctionHeadWrapping.hs"
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
moduleSource declarations = unlines $ ["module FunctionHeads where", ""] ++ declarations

typeAbstractionSource :: [String] -> String
typeAbstractionSource declarations = unlines
  [ "{-# LANGUAGE ExplicitForAll #-}"
  , "{-# LANGUAGE TypeAbstractions #-}"
  , "{-# LANGUAGE TypeApplications #-}"
  ] ++ moduleSource declarations

atomicHead :: String
atomicHead = unwords
  [ "applyArguments"
  , "firstArgument"
  , "secondArgument"
  , "thirdArgument"
  , "fourthArgument"
  , "fifthArgument"
  , "sixthArgument"
  ]

mixedHeads :: [(String, String)]
mixedHeads =
  [ ("atomic variable arguments", atomicHead)
  , ("variables, wildcards, and literal arguments", unwords
      [ "applyArguments firstArgument _ 0 secondArgument"
      , "'a' thirdArgument fourthArgument fifthArgument sixthArgument"
      ])
  , ("atomic and compound arguments", unwords
      [ "applyArguments firstArgument (Just secondArgument)"
      , "[thirdArgument, fourthArgument] fifthArgument sixthArgument"
      ])
  ]
