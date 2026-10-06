{-# LANGUAGE LambdaCase #-}

module CompactInfixRhsSpec (spec) where

import qualified Control.Exception as Exception
import Control.Monad (forM_)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import Data.Semigroup (Last(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import CompactInfixRhsFixtures
import Language.Haskell.Brittany
  ( CConfig(..), CErrorHandlingConfig(..), CLayoutConfig(..), Config
  , parsePrintModule, staticDefaultConfig
  )
import Language.Haskell.Brittany.Internal.CommentPlan
  ( commentPlanFingerprint, normalizeCommentPlan )
import Language.Haskell.Brittany.Internal.Config.Types (AltChooser(..), IndentPolicy(..))
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SemanticFingerprint (compareSemanticSyntax)
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "compact structured infix right operands" $ do
  Hspec.it "keeps the maintained AlignmentPlanner singleton tuple compact" $ do
    source <- readFile $ projectRoot </> "source/test-suite/AlignmentPlannerSpec.hs"
    output <- checkedSource 80 2 IndentPolicyFree source
    assertCompactRhs 80 reportedTuple output
  Hspec.it "keeps fitting maintained Compatibility singleton operands compact" $ do
    source <- readFile $ projectRoot </> "source/test-suite/CompatibilitySpec.hs"
    output <- checkedSource 80 2 IndentPolicyFree source
    forM_
      [ "[\"duplicate feature: \" ++ Matrix.featureName firstFeature]"
      , "[\"feature has no compatibility case: ModuleHeaders\"]"
      ] $ \rhs -> assertCompactRhs 80 rhs output

  forM_ [40, 80, 100] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ policies $ \policy -> do
      let context = layoutDescription columns indent policy
      forM_ ["`Hspec.shouldContain`", "`shouldContain`", "<~~~~~~~~>"] $ \operator ->
        forM_ [False, True] $ \chain ->
          Hspec.it ("keeps a fitting structured RHS compact after " ++ operator
            ++ (if chain then " in a flattened chain" else " with a short left operand") ++ context) $ do
            forM_ [False, True] $ \tuple -> do
              let rhs = structuredRhs tuple $ columns - 3 * indent - 1
              output <- checkedSource columns indent policy $ assertionSource chain operator rhs
              assertCompactRhs columns rhs output
              assertWithinColumns columns output
      Hspec.it ("retains a fitting short-operator attachment" ++ context) $ do
        let rhs = structuredRhs True $ columns - 3 * indent - 1
        output <- checkedSource columns indent policy $ assertionSource False "==" rhs
        assertCompactRhs columns rhs output
        output `Hspec.shouldSatisfy` (any (List.isInfixOf (compact $ "== " ++ rhs) . compact) . lines)
        assertWithinColumns columns output

  forM_ [2, 4] $ \indent -> forM_ policies $ \policy -> do
    let context = layoutDescription 80 indent policy
        operator = "`Hspec.shouldContain`"
    forM_ [0, 1] $ \extra ->
      Hspec.it ("chooses compact attachment at the operator-column boundary plus " ++ show extra ++ context) $ do
        let rhs = structuredRhs True $ 80 - 2 * indent - length operator - 1 + extra
            source = moduleSource ["example = do", "  selectedValuesFromThePreviousStep " ++ operator ++ " " ++ rhs]
        output <- checkedSource 80 indent policy source
        assertCompactRhs 80 rhs output
        line <- uniqueLine operator output
        if extra == 0
          then line `Hspec.shouldContain` "["
          else line `Hspec.shouldNotContain` "["
        assertWithinColumns 80 output
    Hspec.it ("preserves parentheses around a compact detached operand" ++ context) $ do
      let rhs = "(" ++ structuredRhs True (80 - 3 * indent - 3) ++ ")"
      output <- checkedSource 80 indent policy $ assertionSource False operator rhs
      assertCompactRhs 80 rhs output
      assertWithinColumns 80 output
    Hspec.it ("keeps a fitting multiple-element list compact after an operator break" ++ context) $ do
      let rhs = "[firstElement, secondElement, thirdElement, fourthElement]"
      output <- checkedSource 80 indent policy $ assertionSource False operator rhs
      assertCompactRhs 80 rhs output
      assertWithinColumns 80 output
    Hspec.it ("allows a genuinely oversized structured operand to remain multiline" ++ context) $ do
      let rhs = structuredRhs True 90
      output <- checkedSource 40 indent policy $ assertionSource False operator rhs
      lines output `Hspec.shouldSatisfy` all (not . List.isInfixOf (compact rhs) . compact)
      output `Hspec.shouldContain` "marker"
    Hspec.it ("retains the separate continuation for an indivisible oversized literal" ++ context) $ do
      let literal = show $ replicate 90 'x'
      output <- checkedSource 80 indent policy $ assertionSource False operator literal
      line <- uniqueLine literal output
      dropWhile (== ' ') line `Hspec.shouldBe` literal
      length (takeWhile (== ' ') line) `Hspec.shouldBe` 3 * indent
    Hspec.it ("preserves fitting attached and detached options with the shallow chooser" ++ context) $ do
      let config = configWithLayout 80 indent policy
          shallow = config { _conf_layout = (_conf_layout config)
            { _lconfig_altChooser = Identity $ Last AltChooserShallowBest } }
      forM_ [20, 80 - 4 * indent - 1] $ \size -> do
        let rhs = structuredRhs True size
        output <- checkedModule shallow $ assertionSource False operator rhs
        assertCompactRhs 80 rhs output
        if size == 20
          then output `Hspec.shouldSatisfy` (any (List.isInfixOf (compact $ operator ++ " " ++ rhs) . compact) . lines)
          else pure ()
        assertWithinColumns 80 output

  forM_ [2, 4] $ \indent -> forM_ commentSources $ \(name, declarations, marker) ->
    Hspec.it ("preserves " ++ name ++ " at indent " ++ show indent) $ do
      output <- checkedSource 80 indent IndentPolicyFree $ moduleSource $ "example = do" : declarations
      _ <- uniqueLine marker output
      assertWithinColumns 80 output

  forM_ ["`Hspec.shouldContain`", "`shouldContain`", "<~~~~~~~~>"] $ \operator ->
    Hspec.it ("keeps the reported nested do/case tuple compact after " ++ operator) $ do
      output <- checkedSource 80 2 IndentPolicyFree $ nestedSource operator
      assertCompactRhs 80 reportedTuple output
      assertWithinColumns 80 output

  forM_
    [ ("an unfinished tuple-list", "example = values `shouldContain` [(first,")
    , ("a missing flattened-chain RHS", "example = values <~~~~~~~~> [0] <~~~~~~~~>")
    ] $ \(name, declaration) ->
      Hspec.it ("rejects " ++ name ++ " without replacing inplace input") $ do
        let source = moduleSource [declaration]
        directory <- Directory.getTemporaryDirectory
        Exception.bracket
          (do
            (path, handle) <- IO.openTempFile directory "brittany-compact-rhs-invalid.hs"
            IO.hPutStr handle source
            IO.hClose handle
            pure path)
          Directory.removeFile
          $ \path -> do
            Brittany.mainWith "brittany"
              [ "--no-user-config", "--write-mode", "inplace"
              , "--werror", "--fail-on-fallback", path
              ] `Hspec.shouldThrow` (== Exit.ExitFailure 60)
            TextIO.readFile path `Hspec.shouldReturn` Text.pack source

assertCompactRhs :: Int -> String -> String -> IO ()
assertCompactRhs columns rhs output = case
  filter (List.isInfixOf (compact rhs) . compact) $ lines output of
    [line] -> length line `Hspec.shouldSatisfy` (<= columns)
    matches -> Hspec.expectationFailure $
      "expected one compact RHS " ++ rhs ++ ", found " ++ show matches ++ " in:\n" ++ output

assertWithinColumns :: Int -> String -> IO ()
assertWithinColumns columns output = filter ((> columns) . length) (lines output) `Hspec.shouldBe` []

uniqueLine :: String -> String -> IO String
uniqueLine marker output = case filter (List.isInfixOf marker) $ lines output of
  [line] -> pure line
  matches -> Hspec.expectationFailure ("expected one " ++ marker ++ " line, found " ++ show matches)
    >> fail "missing or repeated marker"

compact :: String -> String
compact = filter (/= ' ')

policies :: [IndentPolicy]
policies = [IndentPolicyLeft, IndentPolicyMultiple, IndentPolicyFree]

layoutDescription :: Int -> Int -> IndentPolicy -> String
layoutDescription columns indent policy = " at width " ++ show columns
  ++ ", indent " ++ show indent ++ ", " ++ show policy

checkedSource :: Int -> Int -> IndentPolicy -> String -> IO String
checkedSource columns indent policy = checkedModule $ configWithLayout columns indent policy

checkedModule :: Config -> String -> IO String
checkedModule config original = do
  first <- formatChecked config original
  (inputAnns, inputParsed, ()) <- parseSource original
  (outputAnns, outputParsed, ()) <- parseSource first
  compareSemanticSyntax inputParsed outputParsed `Hspec.shouldBe` Right Nothing
  case (normalizeCommentPlan inputAnns, normalizeCommentPlan outputAnns) of
    (Right inputPlan, Right outputPlan) ->
      commentPlanFingerprint outputPlan `Hspec.shouldBe` commentPlanFingerprint inputPlan
    _ -> Hspec.expectationFailure "input or output has an invalid comment plan"
  second <- formatChecked config first
  third <- formatChecked config second
  second `Hspec.shouldBe` first
  third `Hspec.shouldBe` first
  pure first
 where
  parseSource source = do
    parsed <- ParseModule.parseModule ["-haddock"] "CompactInfixRhs.hs"
      (const $ pure $ Right ()) source
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError >> fail parseError
      Right result -> pure result

formatChecked :: Config -> String -> IO String
formatChecked config source = parsePrintModule config (Text.pack source) >>= \case
  Left errors -> Hspec.expectationFailure
    ("formatting returned " ++ show (length errors) ++ " errors") >> fail "formatting failed"
  Right output -> pure $ Text.unpack output

configWithLayout :: Int -> Int -> IndentPolicy -> Config
configWithLayout columns indent policy = staticDefaultConfig
  { _conf_layout = (_conf_layout staticDefaultConfig)
      { _lconfig_cols = Identity $ Last columns
      , _lconfig_indentAmount = Identity $ Last indent
      , _lconfig_indentPolicy = Identity $ Last policy
      }
  , _conf_errorHandling = (_conf_errorHandling staticDefaultConfig)
      { _econf_Werror = Identity $ Last True
      , _econf_failOnExactSourceFallback = Identity $ Last True
      }
  }
