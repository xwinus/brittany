{-# LANGUAGE LambdaCase #-}

module InfixContinuationAlignmentSpec
  ( spec
  ) where

import Control.Monad (forM_)
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
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "infix continuation alignment" $ do
  Hspec.it "uses one local separator in a heterogeneous operator chain" $ do
    _ <- assertFormatting defaultConfig reproducerInput reproducerExpected
    pure ()

  Hspec.it "uses local separators in every column alignment mode" $ do
    allAlignmentModes `forM_` \mode -> do
      formatted <- formatSource (configWithMode mode) reproducerInput
      formatted `Hspec.shouldBe` reproducerExpected

  Hspec.it "handles compatible, nested, commented, and narrow operators" $ do
    let config = configWithWidth 40
    formatted <- assertFormatting config edgeInput edgeExpected
    configuredOverflow 40 formatted
      `Hspec.shouldSatisfy` (<= configuredOverflow 40 edgeInput)
    formatted `Hspec.shouldNotContain` "$          "
    formatted `Hspec.shouldNotContain` ">>=          "

assertFormatting :: Config -> String -> String -> IO String
assertFormatting config input expected = do
  firstPass <- formatSource config input
  firstPass `Hspec.shouldBe` expected
  assertEquivalent "Input.hs" input "Output.hs" firstPass
  secondPass <- formatSource config firstPass
  thirdPass <- formatSource config secondPass
  secondPass `Hspec.shouldBe` firstPass
  thirdPass `Hspec.shouldBe` firstPass
  pure firstPass

formatSource :: Config -> String -> IO String
formatSource config input = parsePrintModule config (Text.pack input) >>= \case
  Left errors ->
    Hspec.expectationFailure
        ("formatting returned " ++ show (length errors) ++ " errors")
      >> fail "formatting failed"
  Right output -> pure $ Text.unpack output

assertEquivalent :: FilePath -> String -> FilePath -> String -> IO ()
assertEquivalent inputPath input outputPath output = do
  inputParsed <- parseSource inputPath input
  outputParsed <- parseSource outputPath output
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

configuredOverflow :: Int -> String -> Int
configuredOverflow width = sum
  . fmap (max 0 . subtract width . length)
  . lines

defaultConfig :: Config
defaultConfig =
  staticDefaultConfig
    { _conf_errorHandling =
        (_conf_errorHandling staticDefaultConfig)
          { _econf_failOnExactSourceFallback = Identity $ Last True
          }
    }

configWithMode :: ColumnAlignMode -> Config
configWithMode mode =
  defaultConfig
    { _conf_layout =
        (_conf_layout defaultConfig)
          { _lconfig_columnAlignMode = Identity $ Last mode
          }
    }

configWithWidth :: Int -> Config
configWithWidth width =
  defaultConfig
    { _conf_layout =
        (_conf_layout defaultConfig)
          { _lconfig_cols = Identity $ Last width
          }
    }

allAlignmentModes :: [ColumnAlignMode]
allAlignmentModes =
  [ ColumnAlignModeDisabled
  , ColumnAlignModeUnanimously
  , ColumnAlignModeMajority 0.7
  , ColumnAlignModeAnimouslyScale 0
  , ColumnAlignModeAnimously
  , ColumnAlignModeAlways
  ]

reproducerInput :: String
reproducerInput = renderLines
  [ "module InfixContinuation where"
  , ""
  , "contradictory row ="
  , "  not"
  , "    $ null"
  , "    $ (structuralKeys row ++ optionalKeys row)"
  , "    `intersect` prohibitedKeys row"
  ]

reproducerExpected :: String
reproducerExpected = reproducerInput

edgeInput :: String
edgeInput = renderLines
  [ "module InfixContinuationEdge where"
  , ""
  , "repeated actions ="
  , "  startingActionWithLongName"
  , "    >>= firstLongTransformation"
  , "    >>= secondLongTransformation"
  , ""
  , "mixed values allowed ="
  , "  consumeLongResult"
  , "    $ valuesWithLongName"
  , "    <$> transformLongValue"
  , "    >>= collectLongResult"
  , "    `Data.List.intersect` allowedValues"
  , ""
  , "nested values fallback ="
  , "  finalizeLongResult"
  , "    $ (valuesWithLongName >>= normalizeLongValue)"
  , "    <|> fallbackWithLongName"
  , ""
  , "commented values allowed ="
  , "  consumeLongResult"
  , "    $ valuesWithLongName"
  , "    >>= -- Normalize before intersecting."
  , "      normalizeLongValues values"
  , "    `Data.List.intersect` allowedValues"
  ]

edgeExpected :: String
edgeExpected = renderLines
  [ "module InfixContinuationEdge where"
  , ""
  , "repeated actions ="
  , "  startingActionWithLongName"
  , "    >>= firstLongTransformation"
  , "    >>= secondLongTransformation"
  , ""
  , "mixed values allowed ="
  , "  consumeLongResult"
  , "    $ valuesWithLongName"
  , "    <$> transformLongValue"
  , "    >>= collectLongResult"
  , "    `Data.List.intersect` allowedValues"
  , ""
  , "nested values fallback ="
  , "  finalizeLongResult"
  , "    $ (valuesWithLongName"
  , "      >>= normalizeLongValue"
  , "      )"
  , "    <|> fallbackWithLongName"
  , ""
  , "commented values allowed ="
  , "  consumeLongResult"
  , "      $ valuesWithLongName"
  , "      >>= -- Normalize before intersecting."
  , "          normalizeLongValues values"
  , "    `Data.List.intersect` allowedValues"
  ]

renderLines :: [String] -> String
renderLines = List.intercalate "\n"
