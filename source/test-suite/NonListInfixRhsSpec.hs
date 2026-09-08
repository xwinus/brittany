{-# LANGUAGE LambdaCase #-}

module NonListInfixRhsSpec (spec) where

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
spec projectRoot = Hspec.describe "non-list infix RHS wrapping" $ do
  Hspec.it "fits the raw string assertion reported in issue #199" $ do
    output <- checkWithinColumns 80 2 $
      assertionSource "`Hspec.shouldContain`" $ show documentationMessage
    assertSeparateRhs "`Hspec.shouldContain`" output
    output `Hspec.shouldContain` show documentationMessage

  Hspec.it "fits the assertion in the complete maintained LocalTrailingCommentSpec" $ do
    source <- readFile $ projectRoot </> "source/test-suite/LocalTrailingCommentSpec.hs"
    let config = configWithLayout 80 2
    output <- formatChecked config source
    let matchingLines = filter (List.isInfixOf $ show documentationMessage) $ lines output
    length matchingLines `Hspec.shouldBe` 1
    map length matchingLines `Hspec.shouldSatisfy` all (<= 80)
    assertStableAndEquivalent config source output

  forM_ [41, 42, 43, 48, 52, 55] $ \operandLength ->
    Hspec.it
      ("fits a nested raw string operand with length " ++ show operandLength) $ do
        let operand = show $ replicate operandLength 'x'
        output <- checkWithinColumns 80 2 $ nestedAssertionSource operand
        output `Hspec.shouldContain` operand

  forM_
    [ ("source/library/Language/Haskell/Brittany/Internal/Layouters/Decl.hs",
        "let localComments =")
    , ("source/library/Language/Haskell/Brittany/Internal.hs",
        "let preambleUnit =")
    ] $ \(relativePath, bindingHead) ->
      Hspec.it ("preserves the fitting " ++ bindingHead ++ " in its full module") $ do
        source <- readFile $ projectRoot </> relativePath
        let config = configWithLayout 80 2
        output <- formatChecked config source
        output `Hspec.shouldContain` bindingHead
        assertStableAndEquivalent config source output

  forM_ [0, 1] $ \overflow ->
    Hspec.it
      (if overflow == 0 then "keeps a raw literal exactly at the width limit"
        else "breaks before a raw literal one column beyond the width limit") $ do
        let literal = show $ replicate
              (80 - length "    `shouldContain` \"\"" + overflow) 'x'
        output <- checkWithinColumns 80 2 $ assertionSource "`shouldContain`" literal
        if overflow == 0
          then do
            output `Hspec.shouldSatisfy` (any ((== 80) . length) . lines)
            operatorLine "`shouldContain`" output `Hspec.shouldContain` literal
          else assertSeparateRhs "`shouldContain`" output

  forM_ [2, 4] $ \indent -> forM_ [40, 80] $ \columns -> do
    forM_ ["`shouldContain`", "`Hspec.shouldContain`", "<++++++++++++>"] $ \operator ->
      Hspec.it
        ("fits a literal after " ++ operator ++ " at width " ++ show columns
          ++ " and indent " ++ show indent) $ do
          let literal = show $ replicate (columns - 3 * indent - 4) 'x'
          output <- checkWithinColumns columns indent $ assertionSource operator literal
          output `Hspec.shouldContain` literal

    Hspec.it
      ("fits raw literals throughout a flattened chain at width " ++ show columns
        ++ " and indent " ++ show indent) $ do
        let literal = show $ replicate (columns - 3 * indent - 4) 'x'
        output <- checkWithinColumns columns indent $ moduleSource
          [ "example = do"
          , "  prefix <++++++++++++> " ++ literal ++ " <++++++++++++> " ++ literal
          ]
        length (filter (List.isInfixOf literal) $ lines output) `Hspec.shouldBe` 2

  forM_
    [ ("integer literal", replicate 60 '1')
    , ("primitive string literal", show (replicate 60 'x') ++ "#")
    ] $ \(description, operand) ->
      Hspec.it ("places the " ++ description ++ " on its own continuation") $ do
        output <- checkWithinColumns 80 2 $ assertionSource "`Hspec.shouldContain`" operand
        output `Hspec.shouldContain` operand
        operatorLine "`Hspec.shouldContain`" output `Hspec.shouldNotContain` operand

  Hspec.it "preserves a parenthesized raw literal on its continuation line" $ do
    let operand = "(" ++ show documentationMessage ++ ")"
    output <- checkWithinColumns 80 2 $ assertionSource "`Hspec.shouldContain`" operand
    output `Hspec.shouldContain` operand
    assertSeparateRhs "`Hspec.shouldContain`" output

  forM_ [2, 4] $ \indent ->
    Hspec.it ("keeps a short dollar operator attached at indent " ++ show indent) $ do
      let literal = show $ replicate (40 - 3 * indent - 4) 'x'
      output <- checkWithinColumns 40 indent $ assertionSource "$" literal
      operatorLine "$" output `Hspec.shouldContain` literal

  Hspec.it "keeps a fitting raw string assertion compact" $ do
    output <- checkWithinColumns 80 2 $ assertionSource "`shouldContain`" "\"small\""
    output `Hspec.shouldContain` "  result `shouldContain` \"small\""

  Hspec.it "preserves an indivisible literal longer than the configured width" $ do
    let literal = show $ replicate 100 'x'
        source = assertionSource "`shouldContain`" literal
        config = configWithLayout 40 2
    output <- formatChecked config source
    output `Hspec.shouldContain` literal
    filter ((> 40) . length) (lines output)
      `Hspec.shouldSatisfy` \overflow ->
        length overflow == 1 && all (List.isInfixOf literal) overflow
    assertStableAndEquivalent config source output

  forM_ ["-- RHS boundary", "{- RHS boundary -}"] $ \comment ->
    Hspec.it ("preserves an operator/RHS boundary comment " ++ comment) $ do
      output <- checkWithinColumns 80 2 $ moduleSource
        [ "example = do"
        , "  result `Hspec.shouldContain` " ++ comment
        , "    " ++ show documentationMessage
        ]
      output `Hspec.shouldContain` ("`Hspec.shouldContain` " ++ comment)

  Hspec.it "rejects a missing RHS without replacing the inplace source" $ do
    let source = moduleSource ["example = result `shouldContain`"]
    directory <- Directory.getTemporaryDirectory
    Exception.bracket
      (do
        (path, handle) <- IO.openTempFile directory "brittany-non-list-infix-invalid.hs"
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
    parsed <- ParseModule.parseModule ["-haddock"] "NonListInfixRhs.hs"
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
  line `Hspec.shouldNotContain` "\""

assertionSource :: String -> String -> String
assertionSource operator operand = moduleSource
  [ "example = do"
  , "  result " ++ operator ++ " " ++ operand
  ]

nestedAssertionSource :: String -> String
nestedAssertionSource operand = moduleSource
  [ "example = Hspec.describe \"matrix\" $ do"
  , "  case loaded of"
  , "    Right (matrix, discoveredPragmas) -> do"
  , "      Hspec.describe \"manifest validation\" $ do"
  , "        Hspec.it \"requires coverage\" $ do"
  , "          Matrix.validateMatrix invalidMatrix discoveredPragmas"
  , "            `Hspec.shouldContain`"
  , "              " ++ operand
  ]

moduleSource :: [String] -> String
moduleSource declarations = unlines $
  [ "{-# LANGUAGE MagicHash #-}"
  , "module NonListInfixRhs where"
  , ""
  ] ++ declarations

documentationMessage :: String
documentationMessage = "\n  -- Documentation for buildChildAnn.\n  buildChildAnn value = value"
