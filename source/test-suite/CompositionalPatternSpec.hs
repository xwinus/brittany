{-# LANGUAGE LambdaCase #-}

module CompositionalPatternSpec (spec) where

import           Data.Functor.Identity                    ( Identity(..) )
import           Data.Semigroup                           ( Last(..) )
import qualified Data.Text                               as Text
import qualified GHC
import           Language.Haskell.Brittany
import           Language.Haskell.Brittany.Internal.CommentPlan
                                                          ( commentPlanFingerprint
                                                          , normalizeCommentPlan
                                                          )
import           Language.Haskell.Brittany.Internal.Config.Types
                                                          ( IndentPolicy(..) )
import qualified Language.Haskell.Brittany.Internal.ParseModule
                                                         as ParseModule
import           Language.Haskell.Brittany.Internal.SemanticFingerprint
                                                          ( compareSemanticSyntax
                                                          )
import           Language.Haskell.Brittany.Internal.SourceComment.Types
                                                          ( CommentRole
                                                          , SourceCommentSyntax
                                                          )
import qualified Language.Haskell.Brittany.Main          as Brittany
import qualified System.Directory                        as Directory
import qualified System.Exit                             as Exit
import qualified System.FilePath                         as FilePath
import qualified Test.Hspec                              as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "compositional pattern layout" $ do
  goldenExample
    projectRoot
    "wraps the self-hosted ImportDecl pattern within 80 columns"
    "CompositionalPatternSelfHostedInput.hs"
    "CompositionalPatternSelfHostedExpected.hs"
  goldenExample
    projectRoot
    "composes records and nested patterns in every binding context"
    "CompositionalPatternInput.hs"
    "CompositionalPatternExpected.hs"
  mapM_ policyExample
    [IndentPolicyLeft, IndentPolicyMultiple, IndentPolicyFree]
  Hspec.it "is width-safe and idempotent for generated supported patterns" $
    mapM_ generatedPatternExample generatedPatterns
  parseFailureExample
    projectRoot
    "leaves a malformed nested record pattern unchanged"
    "CompositionalPatternInvalid.hs"

goldenExample
  :: FilePath -> String -> FilePath -> FilePath -> Hspec.SpecWith ()
goldenExample projectRoot description inputName expectedName =
  Hspec.it description $ do
    let input = fixturePath projectRoot inputName
        expectedFixture = fixturePath projectRoot expectedName
        output = outputPath projectRoot inputName
        arguments = formatterArguments projectRoot output
    inputSource <- readFile input
    expected <- readFile expectedFixture
    Directory.copyFile input output
    Brittany.mainWith "brittany" arguments
    firstPass <- readFile output
    firstPass `Hspec.shouldBe` expected
    assertLineWidth 80 firstPass
    assertEquivalent input inputSource output firstPass
    firstFingerprint <- commentFingerprint output firstPass
    Brittany.mainWith "brittany" arguments
    secondPass <- readFile output
    secondPass `Hspec.shouldBe` firstPass
    commentFingerprint output secondPass `Hspec.shouldReturn` firstFingerprint
    Brittany.mainWith "brittany" arguments
    readFile output `Hspec.shouldReturn` secondPass

policyExample :: IndentPolicy -> Hspec.SpecWith ()
policyExample policy = Hspec.it
  ("respects width with " ++ show policy)
  $ assertGeneratedFormatting (configFor policy 2 48) policyInput

generatedPatternExample :: String -> IO ()
generatedPatternExample pattern' =
  assertGeneratedFormatting
    (configFor IndentPolicyFree 2 52)
    (generatedSource pattern')

assertGeneratedFormatting :: Config -> String -> IO ()
assertGeneratedFormatting config input = do
  firstPass <- formatSource config input
  assertLineWidth (configuredColumns config) firstPass
  assertEquivalent "Input.hs" input "Output.hs" firstPass
  formatSource config firstPass `Hspec.shouldReturn` firstPass

formatSource :: Config -> String -> IO String
formatSource config input = parsePrintModule config (Text.pack input) >>= \case
  Left errors -> Hspec.expectationFailure
    ("formatting returned " ++ show (length errors) ++ " errors")
    >> fail "formatting failed"
  Right output -> pure $ Text.unpack output

parseFailureExample :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
parseFailureExample projectRoot description fixtureName =
  Hspec.it description $ do
    let fixture = fixturePath projectRoot fixtureName
        output = outputPath projectRoot fixtureName
    expected <- readFile fixture
    Directory.copyFile fixture output
    Brittany.mainWith "brittany" (formatterArguments projectRoot output)
      `Hspec.shouldThrow` (== Exit.ExitFailure 60)
    readFile output `Hspec.shouldReturn` expected

assertEquivalent :: FilePath -> String -> FilePath -> String -> IO ()
assertEquivalent inputPath input outputPath' output = do
  inputParsed <- parseSource inputPath input
  outputParsed <- parseSource outputPath' output
  compareSemanticSyntax inputParsed outputParsed `Hspec.shouldBe` Right Nothing

parseSource :: FilePath -> String -> IO GHC.ParsedSource
parseSource path source = do
  parsed <- ParseModule.parseModule ["-haddock"] path
    (const $ pure $ Right ()) source
  case parsed of
    Left parseError -> Hspec.expectationFailure parseError >> fail parseError
    Right (_, parsedSource, ()) -> pure parsedSource

commentFingerprint
  :: FilePath
  -> String
  -> IO [(Text.Text, SourceCommentSyntax, CommentRole, String)]
commentFingerprint filename source = do
  parsed <- ParseModule.parseModule ["-haddock"] filename
    (const $ pure $ Right ()) source
  case parsed of
    Left parseError -> Hspec.expectationFailure parseError >> pure []
    Right (annotations, _, ()) -> case normalizeCommentPlan annotations of
      Left errors -> Hspec.expectationFailure (show errors) >> pure []
      Right plan -> pure $ commentPlanFingerprint plan

assertLineWidth :: Int -> String -> Hspec.Expectation
assertLineWidth columns source =
  filter ((> columns) . length) (lines source) `Hspec.shouldBe` []

configFor :: IndentPolicy -> Int -> Int -> Config
configFor policy indent columns = staticDefaultConfig
  { _conf_layout = (_conf_layout staticDefaultConfig)
    { _lconfig_cols = Identity $ Last columns
    , _lconfig_indentPolicy = Identity $ Last policy
    , _lconfig_indentAmount = Identity $ Last indent
    }
  , _conf_errorHandling = (_conf_errorHandling staticDefaultConfig)
    { _econf_failOnExactSourceFallback = Identity $ Last True
    }
  }

configuredColumns :: Config -> Int
configuredColumns = getLast . runIdentity . _lconfig_cols . _conf_layout

policyInput :: String
policyInput = unlines
  [ "{-# LANGUAGE NamedFieldPuns #-}"
  , "module PatternPolicy where"
  , ""
  , "match Node { firstField = firstValue, secondField = secondValue, thirdField = thirdValue } = result"
  ]

generatedPatterns :: [String]
generatedPatterns =
  [ "Node { firstField = firstValue, secondField = secondValue, thirdField = thirdValue }"
  , "(Node { firstField = firstValue, secondField = secondValue }, [firstValue, secondValue])"
  , "(firstInput :: Reflex.Event Reflex.Spider String, secondInput :: String -> IO Bool)"
  , "alias@(Node { firstField = firstValue, secondField = secondValue })"
  , "!(Node { firstField = firstValue, secondField = secondValue })"
  , "~(Node { firstField = firstValue, secondField = secondValue })"
  , "(extract -> Node { firstField = firstValue, secondField = secondValue })"
  , "(Node { firstField = firstValue, secondField = secondValue } :: NodeType)"
  , "(Left { leftField = firstValue, leftOther = secondValue }; Right { rightField = firstValue, rightOther = secondValue })"
  ]

generatedSource :: String -> String
generatedSource pattern' = unlines
  [ "{-# LANGUAGE BangPatterns #-}"
  , "{-# LANGUAGE OrPatterns #-}"
  , "{-# LANGUAGE ScopedTypeVariables #-}"
  , "{-# LANGUAGE ViewPatterns #-}"
  , "module GeneratedPattern where"
  , ""
  , "match " ++ pattern' ++ " = result"
  ]

fixturePath :: FilePath -> FilePath -> FilePath
fixturePath projectRoot = FilePath.combine
  $ FilePath.combine projectRoot "source/test-suite/fixtures"

outputPath :: FilePath -> FilePath -> FilePath
outputPath projectRoot = FilePath.combine
  $ FilePath.combine projectRoot "output"

formatterArguments :: FilePath -> FilePath -> [String]
formatterArguments projectRoot input =
  [ "--config-file"
  , FilePath.combine projectRoot "data/brittany.yaml"
  , "--no-user-config"
  , "--columns"
  , "80"
  , "--fail-on-fallback"
  , "--write-mode"
  , "inplace"
  , input
  ]
