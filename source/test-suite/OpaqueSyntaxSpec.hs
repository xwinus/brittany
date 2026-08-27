{-# LANGUAGE OverloadedStrings #-}

module OpaqueSyntaxSpec (spec) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Data.Aeson.Types as Aeson.Types
import Data.Functor.Identity (Identity(..))
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Language.Haskell.Brittany
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

data Inventory = Inventory
  { inventorySchemaVersion :: Int
  , actionableOccurrences :: Int
  , actionableFamilies :: Int
  , actionableFamilyCounts :: [(String, Int)]
  , opaqueOccurrences :: Int
  , opaqueFamilies :: Int
  , opaqueFamilyCounts :: [(String, Int)]
  , occurrenceFamilies :: [Maybe String]
  , occurrenceDispositions :: [String]
  , occurrenceScopes :: [String]
  , occurrenceReasons :: [String]
  }
  deriving (Eq, Show)

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "supported opaque syntax" $ do
  Hspec.it "keeps opaque payloads byte-identical under native parent layout" $ do
    let input = fixturePath projectRoot "OpaqueSyntaxInput.hs"
        originalPayloads =
          [ "[interpolate|Hello,  ${name}!\nKeep  these   spaces.|]"
          , "[uri|https://example.test/a//b?x=1|]"
          , "$(pure [t|Either Int String|])"
          ]
    original <- readFile input
    mapM_ (original `Hspec.shouldContain`) originalPayloads
    mapM_ (assertIdempotentAtIndent projectRoot input originalPayloads) [2, 4]

  Hspec.it "ignores opaque leaves in strict fallback mode and reports JSON" $ do
    let input = fixturePath projectRoot "OpaqueSyntaxInput.hs"
    (result, messages) <- runCore (opaqueInventoryConfig 2)
      True (Just input) Nothing
    isSuccessful result `Hspec.shouldBe` True
    inventory <- decodeSingleInventory messages
    inventorySchemaVersion inventory `Hspec.shouldBe` 1
    actionableOccurrences inventory `Hspec.shouldBe` 0
    actionableFamilies inventory `Hspec.shouldBe` 0
    actionableFamilyCounts inventory `Hspec.shouldBe` []
    opaqueOccurrences inventory `Hspec.shouldBe` 6
    opaqueFamilies inventory `Hspec.shouldBe` 3
    List.sort (opaqueFamilyCounts inventory) `Hspec.shouldBe` List.sort
      [ ("QuasiQuote", 2)
      , ("TemplateHaskellQuote", 1)
      , ("TemplateHaskellSplice", 3)
      ]
    List.sort (occurrenceFamilies inventory) `Hspec.shouldBe` List.sort
      [ Just "QuasiQuote"
      , Just "QuasiQuote"
      , Just "TemplateHaskellQuote"
      , Just "TemplateHaskellSplice"
      , Just "TemplateHaskellSplice"
      , Just "TemplateHaskellSplice"
      ]
    occurrenceDispositions inventory `Hspec.shouldSatisfy`
      all (== "supported-opaque")
    occurrenceScopes inventory `Hspec.shouldSatisfy`
      all (`elem` ["inline", "declaration"])
    occurrenceReasons inventory `Hspec.shouldSatisfy` all (not . null)

  Hspec.it "fails only the explicit opaque policy" $ do
    let input = fixturePath projectRoot "OpaqueSyntaxInput.hs"
        config = staticDefaultConfig
          { _conf_errorHandling = (_conf_errorHandling staticDefaultConfig)
            { _econf_failOnOpaque = Identity $ Semigroup.Last True
            }
          }
    (result, messages) <- runCore config True (Just input) Nothing
    (result == Left 70) `Hspec.shouldBe` True
    messages `Hspec.shouldSatisfy` any
      (List.isInfixOf "supported opaque TemplateHaskellSplice")
    messages `Hspec.shouldNotSatisfy` any
      (List.isInfixOf "EXACT-SOURCE FALLBACKS")

  Hspec.it "counts actionable and opaque dispositions independently" $ do
    let rendered = renderRenderInventory
          [ fallbackRenderNotice TypeFallback "type-location"
          , opaqueRenderNotice QuasiQuote InlineScope "quote-location"
          ]
    inventory <- decodeInventory rendered
    actionableOccurrences inventory `Hspec.shouldBe` 1
    actionableFamilies inventory `Hspec.shouldBe` 1
    opaqueOccurrences inventory `Hspec.shouldBe` 1
    opaqueFamilies inventory `Hspec.shouldBe` 1

  Hspec.it "preserves malformed input when opaque reporting is enabled" $ do
    let input = fixturePath projectRoot "TemplateHaskellFallbackInvalid.hs"
        output = outputPath projectRoot "OpaqueSyntaxInvalid.hs"
    original <- readFile input
    Directory.copyFile input output
    (result, _) <- runCore (opaqueInventoryConfig 2)
      False (Just output) (Just output)
    (result == Left 60) `Hspec.shouldBe` True
    readFile output `Hspec.shouldReturn` original

assertIdempotentAtIndent
  :: FilePath -> FilePath -> [String] -> Int -> IO ()
assertIdempotentAtIndent projectRoot input payloads indent = do
  let output = outputPath projectRoot $ "OpaqueSyntaxIndent" ++ show indent ++ ".hs"
      config = opaqueInventoryConfig indent
  Directory.copyFile input output
  (firstResult, _) <- runCore config False (Just output) (Just output)
  isSuccessful firstResult `Hspec.shouldBe` True
  firstPass <- readFile output
  mapM_ (firstPass `Hspec.shouldContain`) payloads
  (secondResult, _) <- runCore config False (Just output) (Just output)
  isSuccessful secondResult `Hspec.shouldBe` True
  readFile output `Hspec.shouldReturn` firstPass

opaqueInventoryConfig :: Int -> Config
opaqueInventoryConfig indent = staticDefaultConfig
  { _conf_debug = (_conf_debug staticDefaultConfig)
    { _dconf_dump_fallbacks_json = Identity $ Semigroup.Last True
    }
  , _conf_layout = (_conf_layout staticDefaultConfig)
    { _lconfig_indentAmount = Identity $ Semigroup.Last indent
    }
  , _conf_errorHandling = (_conf_errorHandling staticDefaultConfig)
    { _econf_failOnExactSourceFallback = Identity $ Semigroup.Last True
    }
  }

decodeSingleInventory :: [String] -> IO Inventory
decodeSingleInventory messages = case filter (List.isPrefixOf "{") messages of
  [rendered] -> decodeInventory rendered
  rendered -> Hspec.expectationFailure
    ("expected one JSON inventory, got " ++ show rendered)
    >> fail "unreachable"

decodeInventory :: String -> IO Inventory
decodeInventory rendered = case Aeson.eitherDecodeStrict'
  (Text.Encoding.encodeUtf8 $ Text.pack rendered) of
    Left decodeError -> Hspec.expectationFailure decodeError >> fail "unreachable"
    Right value -> case Aeson.Types.parseEither parseInventory value of
      Left parseError -> Hspec.expectationFailure parseError >> fail "unreachable"
      Right inventory -> pure inventory

parseInventory :: Aeson.Value -> Aeson.Types.Parser Inventory
parseInventory = Aeson.withObject "render inventory" $ \root -> do
  schemaVersion <- root .: "schemaVersion"
  summary <- root .: "summary"
  occurrences <- root .: "occurrences"
  actionableCountValues <- summary .: "actionableFallbackFamilies"
  opaqueCountValues <- summary .: "supportedOpaqueFamilies"
  Inventory schemaVersion
    <$> summary .: "actionableFallbackOccurrences"
    <*> summary .: "actionableFallbackUniqueFamilies"
    <*> mapM parseFamilyCount actionableCountValues
    <*> summary .: "supportedOpaqueOccurrences"
    <*> summary .: "supportedOpaqueUniqueFamilies"
    <*> mapM parseFamilyCount opaqueCountValues
    <*> mapM (Aeson.withObject "occurrence" (.: "family")) occurrences
    <*> mapM (Aeson.withObject "occurrence" (.: "disposition")) occurrences
    <*> mapM (Aeson.withObject "occurrence" (.: "scope")) occurrences
    <*> mapM (Aeson.withObject "occurrence" (.: "reason")) occurrences

parseFamilyCount :: Aeson.Value -> Aeson.Types.Parser (String, Int)
parseFamilyCount = Aeson.withObject "family count" $ \value ->
  (,) <$> value .: "family" <*> value .: "occurrences"

runCore
  :: Config
  -> Bool
  -> Maybe FilePath
  -> Maybe FilePath
  -> IO (Either Int Brittany.ChangeStatus, [String])
runCore config suppressOutput input output = do
  messagesRef <- IORef.newIORef []
  result <- Brittany.coreIO
    (\message -> IORef.modifyIORef' messagesRef (++ [message]))
    config suppressOutput False input output
  messages <- IORef.readIORef messagesRef
  pure (result, messages)

isSuccessful :: Either Int Brittany.ChangeStatus -> Bool
isSuccessful = either (const False) (const True)

fixturePath :: FilePath -> FilePath -> FilePath
fixturePath projectRoot = FilePath.combine
  $ FilePath.combine projectRoot "source/test-suite/fixtures"

outputPath :: FilePath -> FilePath -> FilePath
outputPath projectRoot = FilePath.combine
  $ FilePath.combine projectRoot "output"
