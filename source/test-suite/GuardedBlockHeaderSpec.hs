{-# LANGUAGE LambdaCase #-}

module GuardedBlockHeaderSpec (spec) where

import qualified Control.Exception as Exception
import Control.Monad (forM_)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import Data.Semigroup (Last(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import GuardedBlockHeaderFixtures
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
spec projectRoot = Hspec.describe "guarded multiline block headers" $ do
  Hspec.it "keeps the maintained InlineBranchCommentSpec nested case header cohesive" $ do
    source <- readFile $ projectRoot </> "source/test-suite/InlineBranchCommentSpec.hs"
    output <- checkedSource 80 2 IndentPolicyFree source
    assertHeader (caseHeader 80) output

  forM_ [40, 80, 100] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ policies $ \policy -> forM_ ["function", "pattern", "case", "lambda-case"] $ \context ->
      Hspec.it ("keeps a fitting case header cohesive in a guarded " ++ context
        ++ layoutDescription columns indent policy) $ do
        let header = caseHeader columns
        output <- checkedSource columns indent policy $ guardedSource context header
        assertHeader header output
        assertWithinColumns columns output

  forM_ [2, 4] $ \indent -> forM_ policies $ \policy -> do
    let context = layoutDescription 80 indent policy
    forM_ [False, True] $ \alternative ->
      Hspec.it ("retains a fitting attached " ++ (if alternative then "arrow" else "equals")
        ++ " case header" ++ context) $ do
        output <- checkedSource 80 indent policy $ attachedSource alternative
        output `Hspec.shouldContain` ((if alternative then "->" else "=") ++ " case value of")
        assertWithinColumns 80 output
    Hspec.it ("keeps compact scalar guarded bodies attached" ++ context) $ do
      output <- checkedSource 80 indent policy $ moduleSource
        [ "choose value | ready = value"
        , "other input = case input of"
        , "  Just value | ready -> value"
        , "  Nothing -> fallback"
        ]
      output `Hspec.shouldContain` "| ready = value"
      output `Hspec.shouldContain` "| ready -> value"
      assertWithinColumns 80 output
    forM_ extraGuardCases $ \(name, declarations, headers) ->
      Hspec.it ("preserves fitting block headers with " ++ name ++ context) $ do
        output <- checkedSource 80 indent policy $ moduleSource declarations
        forM_ headers $ \header -> assertHeader header output
        assertWithinColumns 80 output
    forM_ neighboringBlocks $ \(name, declarations, marker, longCall) ->
      Hspec.it ("retains the neighboring multiline " ++ name ++ " form" ++ context) $ do
        output <- checkedSource 80 indent policy $ moduleSource declarations
        output `Hspec.shouldContain` marker
        output `Hspec.shouldNotContain` longCall
        assertWithinColumns 80 output
    Hspec.it ("still wraps a genuinely overlong scrutinee" ++ context) $ do
      let header = "case buildSelectedInput firstArgument secondArgument thirdArgument of"
      output <- checkedSource 40 indent policy $ guardedSource "function" header
      output `Hspec.shouldNotContain` header
      assertWithinColumns 40 output
    Hspec.it ("keeps a fitting header when a branch contains an oversized literal" ++ context) $ do
      let literal = show $ replicate 90 'x'
      output <- checkedSource 80 indent policy $ moduleSource
        [ "choose value | ready = case value of"
        , "  True -> " ++ literal
        , "  False -> \"short\""
        ]
      assertHeader "case value of" output
      output `Hspec.shouldContain` literal
      filter (not . List.isInfixOf literal) (lines output)
        `Hspec.shouldSatisfy` all ((<= 80) . length)

  forM_ [2, 4] $ \indent -> forM_ commentedBlocks $ \(name, declarations, marker) ->
    Hspec.it ("preserves " ++ name ++ " at indent " ++ show indent) $ do
      output <- checkedSource 80 indent IndentPolicyFree $ moduleSource declarations
      assertHeader "case pick value of" output
      length (filter (List.isInfixOf marker) $ lines output) `Hspec.shouldBe` 1
      assertWithinColumns 80 output

  forM_ [120, 160] $ \columns -> forM_ [2, 4] $ \indent ->
    Hspec.it ("retains a wide fitting guarded lambda-case header at width " ++ show columns
      ++ " and indent " ++ show indent) $ do
      let header = caseHeader columns
      output <- checkedSource columns indent IndentPolicyFree $ guardedSource "lambda-case" header
      output `Hspec.shouldContain` ("-> " ++ header)
      assertWithinColumns columns output

  forM_ [2, 4] $ \indent -> forM_ [False, True] $ \alternative ->
    forM_ [0, 1] $ \excess ->
      Hspec.it ("reserves the complete attached " ++ (if alternative then "arrow" else "equals")
        ++ " header at indent " ++ show indent ++ " with excess " ++ show excess) $ do
        let prefix = if alternative
              then replicate indent ' ' ++ "Just value | ready -> "
              else "choose value | ready = "
            header = "case " ++ replicate (80 - length prefix - 8 + excess) 's' ++ " of"
            branches amount = map (replicate amount ' ' ++)
              ["True -> good", "False -> bad"]
            declarations = if alternative
              then ["choose input = case input of", prefix ++ header]
                ++ branches (2 * indent) ++ [replicate indent ' ' ++ "Nothing -> fallback"]
              else [prefix ++ header] ++ branches indent
            attached = (if alternative then "-> " else "= ") ++ header
        output <- checkedSource 80 indent IndentPolicyFree $ moduleSource declarations
        assertHeader header output
        if excess == 0
          then do
            output `Hspec.shouldContain` attached
            map length (filter (List.isInfixOf attached) $ lines output) `Hspec.shouldBe` [80]
          else output `Hspec.shouldNotContain` attached
        assertWithinColumns 80 output

  forM_ [2, 4] $ \indent -> forM_ policies $ \policy ->
    Hspec.it ("retains nested case headers inside a guarded case branch"
      ++ layoutDescription 80 indent policy) $ do
      output <- checkedSource 80 indent policy $ moduleSource
        [ "choose value | isSelected value && isEnabled value ="
        , "  case pick value of"
        , "    Just chosen -> case inspect chosen of"
        , "      True -> good"
        , "      False -> bad"
        , "    Nothing -> fallback"
        ]
      forM_ ["case pick value of", "case inspect chosen of"] $ \header -> assertHeader header output
      assertWithinColumns 80 output

  Hspec.it "keeps the reported header cohesive with a bounded-search budget of one" $ do
    let base = configWithLayout 80 2 IndentPolicyFree
        config = base { _conf_layout = (_conf_layout base)
          { _lconfig_altChooser = Identity $ Last $ AltChooserBoundedSearch 1 } }
    output <- checkedModule config $ guardedSource "lambda-case" $ caseHeader 80
    assertHeader (caseHeader 80) output
    assertWithinColumns 80 output

  forM_
    [ ("a missing guarded case scrutinee", ["choose value | ready = case of", "  True -> good"])
    , ("an incomplete guarded lambda-case branch", ["choose = \\case", "  Just value | ready -> case value of", "    True ->"])
    ] $ \(name, declarations) ->
      Hspec.it ("rejects " ++ name ++ " without replacing inplace input") $ do
        let source = moduleSource declarations
        directory <- Directory.getTemporaryDirectory
        Exception.bracket
          (do
            (path, handle) <- IO.openTempFile directory "brittany-guarded-block-invalid.hs"
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

assertHeader :: String -> String -> IO ()
assertHeader header output =
  filter (List.isInfixOf header) (lines output) `Hspec.shouldSatisfy` (not . null)

assertWithinColumns :: Int -> String -> IO ()
assertWithinColumns columns output = filter ((> columns) . length) (lines output) `Hspec.shouldBe` []

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
    parsed <- ParseModule.parseModule ["-haddock"] "GuardedBlockHeaders.hs"
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
