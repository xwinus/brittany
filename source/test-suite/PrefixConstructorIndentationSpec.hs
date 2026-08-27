{-# LANGUAGE LambdaCase #-}

module PrefixConstructorIndentationSpec (spec) where

import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import Data.Semigroup (Last(..))
import qualified Data.Text as Text
import qualified GHC
import Language.Haskell.Brittany
  ( CConfig(..)
  , CErrorHandlingConfig(..)
  , CLayoutConfig(..)
  , Config
  , parsePrintModule
  , staticDefaultConfig
  )
import Language.Haskell.Brittany.Internal.Config.Types (IndentPolicy(..))
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SemanticFingerprint
  ( compareSemanticSyntax )
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "prefix-constructor indentation" $ do
  goldenExample projectRoot
    "uses structural layout for the long single-constructor reproducer"
    "PrefixConstructorIndentationInput.hs"
    "PrefixConstructorIndentationExpected.hs"
  goldenExample projectRoot
    "keeps compact constructors, comments, modifiers, and deriving clauses"
    "PrefixConstructorIndentationEdgeInput.hs"
    "PrefixConstructorIndentationEdgeExpected.hs"
  mapM_ policyExample
    [ IndentPolicyLeft
    , IndentPolicyMultiple
    , IndentPolicyFree
    ]
  Hspec.it "uses configured four-space structural indentation" $ do
    _ <- assertFormatting
      (configFor IndentPolicyFree 4)
      policyInput
      fourSpaceExpected
    pure ()
  Hspec.it "chooses equivalent annotated and unannotated layouts" $ do
    formatted <- assertFormatting
      (configFor IndentPolicyFree 2)
      annotatedComparisonInput
      annotatedComparisonExpected
    let argumentIndents = fmap leadingSpaces
          $ filter (List.isInfixOf "ArgumentType")
          $ lines formatted
    argumentIndents `Hspec.shouldBe` [6, 6, 6, 6]
  parseFailureExample projectRoot
    "leaves malformed inplace input byte-identical"
    "PrefixConstructorIndentationInvalid.hs"

policyExample :: IndentPolicy -> Hspec.SpecWith ()
policyExample policy = Hspec.it
  ("bounds wrapped arguments with " ++ show policy)
  $ do
    _ <- assertFormatting (configFor policy 2) policyInput twoSpaceExpected
    pure ()

goldenExample
  :: FilePath -> String -> FilePath -> FilePath -> Hspec.SpecWith ()
goldenExample projectRoot description inputName expectedName =
  Hspec.it description $ do
    let input = fixturePath projectRoot inputName
        expectedFixture = fixturePath projectRoot expectedName
        output = outputPath projectRoot inputName
    inputSource <- readFile input
    expected <- readFile expectedFixture
    Directory.copyFile input output
    Brittany.mainWith "brittany"
      ("--fail-on-fallback" : formatterArgs projectRoot output)
    firstPass <- readFile output
    firstPass `Hspec.shouldBe` expected
    assertLineWidth 80 firstPass
    assertEquivalent input inputSource output firstPass
    Brittany.mainWith "brittany"
      ("--fail-on-fallback" : formatterArgs projectRoot output)
    readFile output `Hspec.shouldReturn` firstPass

parseFailureExample :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
parseFailureExample projectRoot description fixtureName =
  Hspec.it description $ do
    let fixture = fixturePath projectRoot fixtureName
        output = outputPath projectRoot fixtureName
    expected <- readFile fixture
    Directory.copyFile fixture output
    Brittany.mainWith "brittany" (formatterArgs projectRoot output)
      `Hspec.shouldThrow` (== Exit.ExitFailure 60)
    readFile output `Hspec.shouldReturn` expected

assertFormatting :: Config -> String -> String -> IO String
assertFormatting config input expected = do
  firstPass <- formatSource config input
  firstPass `Hspec.shouldBe` expected
  assertLineWidth 80 firstPass
  assertEquivalent "Input.hs" input "Output.hs" firstPass
  formatSource config firstPass `Hspec.shouldReturn` firstPass
  pure firstPass

formatSource :: Config -> String -> IO String
formatSource config input = parsePrintModule config (Text.pack input) >>= \case
  Left errors -> Hspec.expectationFailure
    ("formatting returned " ++ show (length errors) ++ " errors")
    >> fail "formatting failed"
  Right output -> pure $ Text.unpack output

assertEquivalent :: FilePath -> String -> FilePath -> String -> IO ()
assertEquivalent inputPath input formattedPath output = do
  inputParsed <- parseSource inputPath input
  outputParsed <- parseSource formattedPath output
  compareSemanticSyntax inputParsed outputParsed `Hspec.shouldBe` Right Nothing

parseSource :: FilePath -> String -> IO GHC.ParsedSource
parseSource path source = do
  parsed <- ParseModule.parseModule ["-haddock"] path
    (const $ pure $ Right ()) source
  case parsed of
    Left parseError -> Hspec.expectationFailure parseError >> fail parseError
    Right (_, parsedSource, ()) -> pure parsedSource

assertLineWidth :: Int -> String -> Hspec.Expectation
assertLineWidth columns source =
  filter ((> columns) . length) (lines source) `Hspec.shouldBe` []

leadingSpaces :: String -> Int
leadingSpaces = length . takeWhile (== ' ')

configFor :: IndentPolicy -> Int -> Config
configFor policy indent = staticDefaultConfig
  { _conf_layout = (_conf_layout staticDefaultConfig)
    { _lconfig_cols = Identity $ Last 80
    , _lconfig_indentPolicy = Identity $ Last policy
    , _lconfig_indentAmount = Identity $ Last indent
    }
  , _conf_errorHandling = (_conf_errorHandling staticDefaultConfig)
    { _econf_failOnExactSourceFallback = Identity $ Last True
    }
  }

fixturePath :: FilePath -> FilePath -> FilePath
fixturePath projectRoot fixtureName = FilePath.combine
  (FilePath.combine projectRoot "source/test-suite/fixtures")
  fixtureName

outputPath :: FilePath -> FilePath -> FilePath
outputPath projectRoot fixtureName =
  FilePath.combine (FilePath.combine projectRoot "output") fixtureName

formatterArgs :: FilePath -> FilePath -> [String]
formatterArgs projectRoot input =
  [ "--config-file"
  , FilePath.combine projectRoot "data/brittany.yaml"
  , "--no-user-config"
  , "--columns"
  , "80"
  , "--write-mode"
  , "inplace"
  , input
  ]

policyInput :: String
policyInput = renderLines
  [ "module PrefixPolicy where"
  , ""
  , "data ConfigurationParseError = ConfigurationParseError ConfigurationScope"
  , "                                                       Y.ParseException"
  , "  deriving Show"
  ]

twoSpaceExpected :: String
twoSpaceExpected = renderLines
  [ "module PrefixPolicy where"
  , ""
  , "data ConfigurationParseError"
  , "  = ConfigurationParseError"
  , "      ConfigurationScope"
  , "      Y.ParseException"
  , "  deriving Show"
  ]

fourSpaceExpected :: String
fourSpaceExpected = renderLines
  [ "module PrefixPolicy where"
  , ""
  , "data ConfigurationParseError"
  , "    = ConfigurationParseError"
  , "          ConfigurationScope"
  , "          Y.ParseException"
  , "    deriving Show"
  ]

annotatedComparisonInput :: String
annotatedComparisonInput = renderLines
  [ "module AnnotatedComparison where"
  , ""
  , "data Plain = ConstructorWithANameLongEnoughToRequireStructuralLayout"
  , "  FirstArgumentType SecondArgumentType"
  , ""
  , "data Documented ="
  , "  -- | Constructor documentation."
  , "  ConstructorWithANameLongEnoughToRequireStructuralLayout"
  , "    FirstArgumentType SecondArgumentType"
  ]

annotatedComparisonExpected :: String
annotatedComparisonExpected = renderLines
  [ "module AnnotatedComparison where"
  , ""
  , "data Plain"
  , "  = ConstructorWithANameLongEnoughToRequireStructuralLayout"
  , "      FirstArgumentType"
  , "      SecondArgumentType"
  , ""
  , "data Documented"
  , "  = -- | Constructor documentation."
  , "    ConstructorWithANameLongEnoughToRequireStructuralLayout"
  , "      FirstArgumentType"
  , "      SecondArgumentType"
  ]

renderLines :: [String] -> String
renderLines = List.intercalate "\n"
