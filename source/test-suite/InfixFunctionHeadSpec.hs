{-# LANGUAGE LambdaCase #-}

module InfixFunctionHeadSpec (spec) where

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
spec projectRoot = Hspec.describe "infix function head wrapping" $ do
  Hspec.it "wraps the actual AnnotationIndex equation when formatting its module" $ do
    source <- readFile $ projectRoot
      </> "source/library/Language/Haskell/Brittany/Internal/AnnotationIndex.hs"
    let config = configWithLayout 80 2
    output <- formatChecked config source
    filter ((> 80) . length)
      (filter (not . List.isPrefixOf "import ") $ lines output)
      `Hspec.shouldBe` []
    output `Hspec.shouldContain` List.intercalate "\n"
      [ "  AnnotationIndex leftNodes leftOverrides"
      , "    <> AnnotationIndex rightNodes rightOverrides"
      , "    = AnnotationIndex (leftNodes <> rightNodes)"
      , "                      (leftOverrides <> rightOverrides)"
      ]
    assertStableAndEquivalent config source output

  forM_ ["left <+> right = left", "left `combine` right = left"] $ \equation ->
    Hspec.it ("keeps a compact equation: " ++ equation) $ do
      output <- checkWithinColumns 80 2 $ moduleSource [equation]
      output `Hspec.shouldContain` equation

  forM_ ["left <+> right", "(Just left) <+> (Just right)"] $ \headSource ->
    Hspec.it ("keeps a fitting head compact despite an unavoidable RHS: " ++ headSource) $ do
      let value = "unavoidable" ++ replicate 80 'x'
          source = moduleSource [headSource ++ " = " ++ value]
          config = configWithLayout 40 2
      output <- formatChecked config source
      output `Hspec.shouldContain` headSource
      filter ((> 40) . length) (lines output)
        `Hspec.shouldBe` ["  " ++ value]
      assertStableAndEquivalent config source output

  forM_ [2, 4] $ \indent -> forM_ [40, 80] $ \columns ->
    forM_ longHeads $ \(description, headSource) ->
      Hspec.it
        ("wraps " ++ description ++ " at width " ++ show columns
          ++ " and indent " ++ show indent) $ do
          _ <- checkWithinColumns columns indent $ moduleSource [headSource ++ " = ()"]
          pure ()

  forM_ [2, 4] $ \indent -> forM_ [40, 80] $ \columns ->
    forM_ ["<+>", "`combine`"] $ \operator ->
      Hspec.it
        ("wraps parenthesized infix heads with extra arguments using " ++ operator
          ++ " at width " ++ show columns ++ " and indent " ++ show indent) $ do
          let headSource = "(firstLongOperand " ++ operator
                ++ " secondLongOperand) additionalArgument finalArgument"
          output <- checkWithinColumns columns indent $ moduleSource [headSource ++ " = ()"]
          output `Hspec.shouldContain` operator
          output `Hspec.shouldContain` "additionalArgument"
          output `Hspec.shouldContain` "finalArgument"

  forM_ [False, True] $ \multiple ->
    Hspec.it ("wraps infix heads with " ++ if multiple then "multiple guards" else "one guard") $ do
      let source = moduleSource $
            [ symbolicHead
            , "  | True = ()"
            ] ++ if multiple then ["  | otherwise = ()"] else []
      _ <- checkWithinColumns 40 2 source
      pure ()

  Hspec.it "keeps grouped structural operands and further arguments compositional" $ do
    output <- checkWithinColumns 40 2 $ moduleSource
      [ "((leftFirst, leftSecond, leftThird, leftFourth)"
      , "  <+> (rightFirst, rightSecond, rightThird))"
      , "  (Just extraArgument) finalArgument = ()"
      ]
    output `Hspec.shouldContain` "(Just extraArgument)"
    output `Hspec.shouldContain` "finalArgument"

  Hspec.it "keeps a where clause attached to the wrapped infix equation" $ do
    output <- checkWithinColumns 40 4 $ moduleSource
      [ symbolicHead ++ " = result"
      , "  where"
      , "    result = ()"
      ]
    output `Hspec.shouldContain` "where"

  Hspec.it "wraps a local infix binding inside let" $ do
    _ <- checkWithinColumns 40 2 $ moduleSource
      [ "outer ="
      , "  let " ++ symbolicHead ++ " = ()"
      , "  in ()"
      ]
    pure ()

  Hspec.it "wraps an infix binding inside a nested where clause" $ do
    _ <- checkWithinColumns 40 4 $ moduleSource
      [ "outer = inner"
      , "  where"
      , "    inner = ()"
      , "      where"
      , "        " ++ symbolicHead ++ " = ()"
      ]
    pure ()

  forM_ commentedHeads $ \(description, declarations) ->
    Hspec.it ("preserves " ++ description ++ " while wrapping an infix head") $ do
      _ <- checkWithinColumns 80 2 $ moduleSource declarations
      pure ()

  Hspec.it "keeps an unavoidable long operand intact while separating the other operand" $ do
    let operand = "operand" ++ replicate 80 'x'
        source = moduleSource [operand ++ " <+> rightOperand = ()"]
        config = configWithLayout 40 2
    output <- formatChecked config source
    map (dropWhile (== ' ')) (filter ((> 40) . length) $ lines output)
      `Hspec.shouldBe` [operand]
    assertStableAndEquivalent config source output

  forM_ [symbolicHead ++ " =", "(left <+> right additional = ()"] $ \equation ->
    Hspec.it ("rejects a malformed equation without changing its source: " ++ equation) $ do
      let source = moduleSource [equation]
      directory <- Directory.getTemporaryDirectory
      Exception.bracket
        (do
          (path, handle) <- IO.openTempFile directory "brittany-infix-head-invalid.hs"
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
    parsed <- ParseModule.parseModule ["-haddock"] "InfixFunctionHeads.hs"
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
moduleSource declarations = unlines $ ["module InfixFunctionHeads where", ""] ++ declarations

symbolicHead :: String
symbolicHead = "Wrapped firstLeftOperand secondLeftOperand"
  ++ " <+> Wrapped firstRightOperand secondRightOperand"

longHeads :: [(String, String)]
longHeads =
  [ ("symbolic constructor operands", symbolicHead)
  , ("backticked constructor operands",
      "Wrapped firstLeftOperand secondLeftOperand"
        ++ " `combine` Wrapped firstRightOperand secondRightOperand")
  , ("parenthesized constructor operands",
      "(Wrapped firstLeftOperand secondLeftOperand)"
        ++ " <+> (Wrapped firstRightOperand secondRightOperand)")
  , ("tuple and list operands",
      "(firstLeftOperand, secondLeftOperand)"
        ++ " <+> [firstRightOperand, secondRightOperand]")
  , ("as-pattern and cons operands",
      "left@(Wrapped firstLeftOperand secondLeftOperand)"
        ++ " <+> (firstRightOperand : remainingRightOperands)")
  ]

commentedHeads :: [(String, [String])]
commentedHeads =
  [ ("a line comment after the left operand",
      [ "Wrapped firstLeftOperand secondLeftOperand -- left operand"
      , "  <+> Wrapped firstRightOperand secondRightOperand = ()"
      ])
  , ("a block comment beside the operator",
      [ "Wrapped firstLeftOperand secondLeftOperand"
      , "  <+> {- operator note -} Wrapped firstRightOperand secondRightOperand = ()"
      ])
  , ("a line comment before the equals sign",
      [ "Wrapped firstLeftOperand secondLeftOperand"
      , "  <+> Wrapped firstRightOperand secondRightOperand -- right operand"
      , "  = ()"
      ])
  ]
