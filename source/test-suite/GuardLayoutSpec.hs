{-# LANGUAGE LambdaCase #-}

module GuardLayoutSpec
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
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SemanticFingerprint
  ( compareSemanticSyntax
  )
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "guard layout" $ do
  Hspec.it "moves a self-hosted guard below a long pattern" $ do
    assertFormatting defaultConfig reproducerInput reproducerExpected

  Hspec.it "wraps a boolean continuation without splitting its application" $ do
    assertFormatting (configWithWidthAndIndent 60 2) edgeInput edgeExpected

  Hspec.it "uses the configured four-space indentation for a moved guard" $ do
    assertFormatting
      (configWithWidthAndIndent 80 4)
      edgeInput
      fourSpaceExpected

  Hspec.it "keeps a short guard on the binding line" $ do
    assertFormatting defaultConfig compactInput compactExpected

  Hspec.it "reports malformed guard syntax" $ do
    parsePrintModule defaultConfig (Text.pack malformedInput) >>= \case
      Left _ -> pure ()
      Right output -> Hspec.expectationFailure
        $ "malformed input formatted as: " ++ Text.unpack output

assertFormatting :: Config -> String -> String -> IO ()
assertFormatting config input expected = do
  firstPass <- formatSource config input
  firstPass `Hspec.shouldBe` expected
  assertEquivalent "GuardInput.hs" input "GuardOutput.hs" firstPass
  secondPass <- formatSource config firstPass
  thirdPass <- formatSource config secondPass
  secondPass `Hspec.shouldBe` firstPass
  thirdPass `Hspec.shouldBe` firstPass

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

defaultConfig :: Config
defaultConfig =
  staticDefaultConfig
    { _conf_errorHandling =
        (_conf_errorHandling staticDefaultConfig)
          { _econf_failOnExactSourceFallback = Identity $ Last True
          }
    }

configWithWidthAndIndent :: Int -> Int -> Config
configWithWidthAndIndent width indentAmount =
  defaultConfig
    { _conf_layout =
        (_conf_layout defaultConfig)
          { _lconfig_cols = Identity $ Last width
          , _lconfig_indentAmount = Identity $ Last indentAmount
          }
    }

reproducerInput :: String
reproducerInput = renderLines
  [ "module Guard where"
  , ""
  , "format value = case value of"
  , "  L _ (ConDeclH98 _ consName False [] context details _)"
  , "    | contextIsEmpty mCtxt && contextIsEmpty context -> do"
  , "      pure details"
  ]

reproducerExpected :: String
reproducerExpected = reproducerInput

edgeInput :: String
edgeInput = renderLines
  [ "module Guard where"
  , ""
  , "format value = case value of"
  , "  L _ (ConDeclH98 _ consName False [] context details _)"
  , "    | contextIsEmpty maybeConstructorContext"
  , "      && contextIsEmpty dataDeclarationContext -> do"
  , "      pure details"
  ]

edgeExpected :: String
edgeExpected = edgeInput

fourSpaceExpected :: String
fourSpaceExpected = renderLines
  [ "module Guard where"
  , ""
  , "format value = case value of"
  , "    L _ (ConDeclH98 _ consName False [] context details _)"
  , "        | contextIsEmpty maybeConstructorContext"
  , "            && contextIsEmpty dataDeclarationContext -> do"
  , "            pure details"
  ]

compactInput :: String
compactInput = renderLines
  [ "module Guard where"
  , ""
  , "short x | valid x = x"
  ]

compactExpected :: String
compactExpected = compactInput

malformedInput :: String
malformedInput = renderLines
  [ "module Guard where"
  , ""
  , "broken x | = x"
  ]

renderLines :: [String] -> String
renderLines = List.intercalate "\n"
