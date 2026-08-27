{-# LANGUAGE LambdaCase #-}

module SiblingBindingAlignmentSpec
  ( spec
  ) where

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
import Language.Haskell.Brittany.Internal.Config.Types (ColumnAlignMode(..))
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SemanticFingerprint
  (compareSemanticSyntax)
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "sibling binding alignment" $ do
  Hspec.it "aligns equations for sibling functions in where and let blocks" $ do
    assertFormatting defaultConfig primaryInput primaryExpected
  Hspec.it "keeps nested pattern alignment scoped to one function" $ do
    assertFormatting defaultConfig edgeInput edgeExpected
  Hspec.it "leaves sibling equations unaligned when alignment is disabled" $ do
    assertFormatting
      (configWithAlignMode ColumnAlignModeDisabled)
      primaryInput
      primaryDisabledExpected
  parseFailureExample
    projectRoot
    "leaves malformed sibling bindings byte-identical"
    "SiblingBindingAlignmentInvalid.hs"

assertFormatting :: Config -> String -> String -> IO ()
assertFormatting config input expected = do
  firstPass <- formatSource config input
  firstPass `Hspec.shouldBe` expected
  assertEquivalent "Input.hs" input "Output.hs" firstPass
  formatSource config firstPass `Hspec.shouldReturn` firstPass

formatSource :: Config -> String -> IO String
formatSource config input = parsePrintModule config (Text.pack input) >>= \case
  Left errors ->
    Hspec.expectationFailure
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
  parsed <- ParseModule.parseModule
    ["-haddock"]
    path
    (const $ pure $ Right ())
    source
  case parsed of
    Left parseError -> Hspec.expectationFailure parseError >> fail parseError
    Right (_, parsedSource, ()) -> pure parsedSource

parseFailureExample :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
parseFailureExample projectRoot description fixtureName =
  Hspec.it description $ do
    let
      fixture = fixturePath projectRoot fixtureName
      output = outputPath projectRoot fixtureName
    expected <- readFile fixture
    Directory.copyFile fixture output
    Brittany.mainWith "brittany" (formatterArgs projectRoot output)
      `Hspec.shouldThrow` (== Exit.ExitFailure 60)
    readFile output `Hspec.shouldReturn` expected

defaultConfig :: Config
defaultConfig =
  staticDefaultConfig
    { _conf_errorHandling =
        (_conf_errorHandling staticDefaultConfig)
          { _econf_failOnExactSourceFallback = Identity $ Last True
          }
    }

configWithAlignMode :: ColumnAlignMode -> Config
configWithAlignMode mode =
  defaultConfig
    { _conf_layout =
        (_conf_layout defaultConfig)
          { _lconfig_columnAlignMode = Identity $ Last mode
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
  , "--write-mode"
  , "inplace"
  , input
  ]

primaryInput :: String
primaryInput = renderLines
  [ "module BindingAlignment where"
  , ""
  , "f = go"
  , " where"
  , "  count k = 1"
  , "  noteOf e = 2"
  , ""
  , "withLet ="
  , "  let left value = value"
  , "      substantiallyLonger n = n + 1"
  , "  in left 0"
  ]

primaryExpected :: String
primaryExpected = renderLines
  [ "module BindingAlignment where"
  , ""
  , "f = go"
  , " where"
  , "  count k  = 1"
  , "  noteOf e = 2"
  , ""
  , "withLet ="
  , "  let left value            = value"
  , "      substantiallyLonger n = n + 1"
  , "  in  left 0"
  ]

primaryDisabledExpected :: String
primaryDisabledExpected = renderLines
  [ "module BindingAlignment where"
  , ""
  , "f = go"
  , " where"
  , "  count k = 1"
  , "  noteOf e = 2"
  , ""
  , "withLet ="
  , "  let left value = value"
  , "      substantiallyLonger n = n + 1"
  , "  in  left 0"
  ]

edgeInput :: String
edgeInput = renderLines
  [ "module BindingAlignmentEdge where"
  , ""
  , "example ="
  , "  let noArg = 0"
  , "      unary x = 1"
  , "      longer y z = 2"
  , "      same [] x = 3"
  , "      same (x : xs) y = 4"
  , "  in noArg"
  ]

edgeExpected :: String
edgeExpected = renderLines
  [ "module BindingAlignmentEdge where"
  , ""
  , "example ="
  , "  let noArg           = 0"
  , "      unary x         = 1"
  , "      longer y z      = 2"
  , "      same []       x = 3"
  , "      same (x : xs) y = 4"
  , "  in  noArg"
  ]

renderLines :: [String] -> String
renderLines = List.intercalate "\n"
