{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
-- GHC cannot specialise imported container and tuple Ord dictionaries here.
{-# OPTIONS_GHC -Wno-missed-specialisations #-}

module CompatibilityMatrix
  ( CaseKind(..)
  , ExpectedResult(..)
  , Feature(..)
  , FeatureKind(..)
  , Matrix(..)
  , MatrixCase(..)
  , SupportMode(..)
  , discoverLanguagePragmas
  , loadMatrix
  , validateMatrix
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (Parser)
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Char as Char
import Data.Kind (Type)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Yaml as Yaml
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

type FeatureKind :: Type
data FeatureKind
  = Edition
  | Extension
  | Syntax
  deriving (Eq, Show)

type SupportMode :: Type
data SupportMode
  = Native
  | ExactSource
  | Unsupported
  deriving (Eq, Show)

type CaseKind :: Type
data CaseKind
  = Expected
  | Edge
  | Malformed
  | UnsupportedCase
  deriving (Eq, Ord, Show)

type ExpectedResult :: Type
data ExpectedResult
  = Formats
  | ParseFailure
  | FormattingFailure
  deriving (Eq, Show)

type Feature :: Type
data Feature = Feature
  { featureName :: String
  , featureKind :: FeatureKind
  , featureSupport :: SupportMode
  , featureTrackingIssue :: Int
  }
  deriving (Eq, Show)

type MatrixCase :: Type
data MatrixCase = MatrixCase
  { matrixCaseName :: String
  , matrixCaseKind :: CaseKind
  , matrixCaseFixture :: FilePath
  , matrixCaseFeatures :: [String]
  , matrixCaseExpectedResult :: ExpectedResult
  , matrixCaseSkipped :: Bool
  }
  deriving (Eq, Show)

type Matrix :: Type
data Matrix = Matrix
  { matrixSchemaVersion :: Int
  , matrixFeatures :: [Feature]
  , matrixCases :: [MatrixCase]
  }
  deriving (Eq, Show)

instance Aeson.FromJSON FeatureKind where
  parseJSON = parseEnum "feature kind"
    [ ("edition", Edition)
    , ("extension", Extension)
    , ("syntax", Syntax)
    ]

instance Aeson.FromJSON SupportMode where
  parseJSON = parseEnum "support mode"
    [ ("native", Native)
    , ("exact-source", ExactSource)
    , ("unsupported", Unsupported)
    ]

instance Aeson.FromJSON CaseKind where
  parseJSON = parseEnum "case kind"
    [ ("expected", Expected)
    , ("edge", Edge)
    , ("malformed", Malformed)
    , ("unsupported", UnsupportedCase)
    ]

instance Aeson.FromJSON ExpectedResult where
  parseJSON = parseEnum "expected result"
    [ ("formats", Formats)
    , ("parse-failure", ParseFailure)
    , ("formatting-failure", FormattingFailure)
    ]

instance Aeson.FromJSON Feature where
  parseJSON = Aeson.withObject "feature" $ \object ->
    Feature
      <$> object .: "name"
      <*> object .: "kind"
      <*> object .: "support"
      <*> object .: "tracking-issue"

instance Aeson.FromJSON MatrixCase where
  parseJSON = Aeson.withObject "matrix case" $ \object ->
    MatrixCase
      <$> object .: "name"
      <*> object .: "kind"
      <*> object .: "fixture"
      <*> object .: "features"
      <*> object .: "expected-result"
      <*> object .:? "skip" .!= False

instance Aeson.FromJSON Matrix where
  parseJSON = Aeson.withObject "compatibility matrix" $ \object ->
    Matrix
      <$> object .: "schema-version"
      <*> object .: "features"
      <*> object .: "cases"

parseEnum
  :: String
  -> [(Text.Text, value)]
  -> Aeson.Value
  -> Parser value
parseEnum description values = Aeson.withText description $ \value ->
  case lookup value values of
    Just parsed -> pure parsed
    Nothing -> fail
      $ "unknown "
      ++ description
      ++ ": "
      ++ Text.unpack value

loadMatrix :: FilePath -> IO (Either String Matrix)
loadMatrix path =
  Bifunctor.first Yaml.prettyPrintParseException <$> Yaml.decodeFileEither path

discoverLanguagePragmas :: FilePath -> IO [(String, FilePath)]
discoverLanguagePragmas projectRoot = do
  files <- concat <$> mapM haskellFiles
    [ FilePath.combine projectRoot "data"
    , FilePath.combine projectRoot "source/test-suite/fixtures"
    ]
  concat <$> mapM pragmasInFile files
 where
  haskellFiles directory = do
    entries <- Directory.listDirectory directory
    pure
      [ FilePath.combine directory entry
      | entry <- List.sort entries
      , FilePath.takeExtension entry == ".hs"
      ]

  pragmasInFile path = do
    source <- Text.IO.readFile path
    let relativePath = FilePath.makeRelative projectRoot path
    pure [(name, relativePath) | name <- languagePragmas source]

languagePragmas :: Text.Text -> [String]
languagePragmas = go
 where
  marker = Text.pack "{-# LANGUAGE"
  terminator = Text.pack "#-}"

  go source = case Text.breakOn marker source of
    (_, rest) | Text.null rest -> []
    (_, rest) ->
      let afterMarker = Text.drop (Text.length marker) rest
          (body, afterBody) = Text.breakOn terminator afterMarker
          names = Text.split (\char -> char == ',' || Char.isSpace char) body
          parsedNames = Text.unpack <$> filter (not . Text.null) names
      in if Text.null afterBody
        then []
        else parsedNames ++ go (Text.drop (Text.length terminator) afterBody)

validateMatrix :: Matrix -> [(String, FilePath)] -> [String]
validateMatrix matrix discoveredPragmas = concat
  [ schemaErrors
  , duplicateErrors "feature" featureNames
  , duplicateErrors "case" caseNames
  , editionErrors
  , requiredCaseKindErrors
  , concatMap validateCase cases
  , concatMap validateFeature features
  , unclassifiedPragmaErrors
  ]
 where
  features = matrixFeatures matrix
  cases = matrixCases matrix
  featureNames = featureName <$> features
  caseNames = matrixCaseName <$> cases
  featureMap = Map.fromList [(featureName feature, feature) | feature <- features]
  caseFeatureNames = Set.fromList $ concatMap matrixCaseFeatures cases
  discoveredSet = Set.fromList discoveredPragmas

  schemaErrors =
    [ "unsupported compatibility matrix schema: " ++ show (matrixSchemaVersion matrix)
    | matrixSchemaVersion matrix /= 1
    ]

  editionNames = Set.fromList
    [ featureName feature
    | feature <- features
    , featureKind feature == Edition
    ]
  requiredEditions = Set.fromList ["Haskell2010", "GHC2021", "GHC2024"]
  editionErrors =
    [ "edition coverage must be exactly Haskell2010, GHC2021, and GHC2024"
    | editionNames /= requiredEditions
    ]

  requiredCaseKinds = Set.fromList [Expected, Edge, Malformed]
  presentCaseKinds = Set.fromList $ matrixCaseKind <$> cases
  requiredCaseKindErrors =
    [ "compatibility matrix requires expected, edge, and malformed cases"
    | not $ requiredCaseKinds `Set.isSubsetOf` presentCaseKinds
    ]

  validateCase matrixCase = concat
    [ ["case has no features: " ++ matrixCaseName matrixCase
      | null $ matrixCaseFeatures matrixCase]
    , ["case is marked skipped: " ++ matrixCaseName matrixCase
      | matrixCaseSkipped matrixCase]
    , [ "case references unknown feature "
        ++ name
        ++ ": "
        ++ matrixCaseName matrixCase
      | name <- matrixCaseFeatures matrixCase
      , Map.notMember name featureMap
      ]
    , [ "case does not enable feature "
        ++ name
        ++ " in "
        ++ matrixCaseFixture matrixCase
      | name <- matrixCaseFeatures matrixCase
      , featureRequiresPragma name
      , (name, matrixCaseFixture matrixCase) `Set.notMember` discoveredSet
      ]
    , caseKindErrors matrixCase
    , supportResultErrors matrixCase
    ]

  caseKindErrors matrixCase = case matrixCaseKind matrixCase of
    Expected -> mustFormat matrixCase
    Edge -> mustFormat matrixCase
    Malformed ->
      [ "malformed case must expect a parse failure: " ++ matrixCaseName matrixCase
      | matrixCaseExpectedResult matrixCase /= ParseFailure
      ]
    UnsupportedCase ->
      [ "unsupported case must expect failure: " ++ matrixCaseName matrixCase
      | matrixCaseExpectedResult matrixCase == Formats
      ]

  mustFormat matrixCase =
    [ "expected or edge case must format successfully: " ++ matrixCaseName matrixCase
    | matrixCaseExpectedResult matrixCase /= Formats
    , not $ all featureIsUnsupported $ matrixCaseFeatures matrixCase
    ]

  featureIsUnsupported name = case Map.lookup name featureMap of
    Just feature -> featureSupport feature == Unsupported
    Nothing -> False

  supportResultErrors matrixCase = concatMap validateResult
    $ matrixCaseFeatures matrixCase
   where
    validateResult name = case Map.lookup name featureMap of
      Nothing -> []
      Just feature -> case featureSupport feature of
        Unsupported ->
          [ "unsupported feature has a successful case: " ++ name
          | matrixCaseExpectedResult matrixCase == Formats
          ]
        _ ->
          [ "supported feature has no successful result in case "
            ++ matrixCaseName matrixCase
          | matrixCaseKind matrixCase /= Malformed
          , matrixCaseExpectedResult matrixCase /= Formats
          ]

  validateFeature feature = concat
    [ ["feature has invalid tracking issue: " ++ featureName feature
      | featureTrackingIssue feature <= 0]
    , ["feature has no compatibility case: " ++ featureName feature
      | featureName feature `Set.notMember` caseFeatureNames]
    , case featureSupport feature of
        Unsupported -> []
        _ ->
          [ "supported feature has no successful compatibility case: "
            ++ featureName feature
          | not $ any (successfulCaseFor $ featureName feature) cases
          ]
    ]

  successfulCaseFor name matrixCase =
    name `elem` matrixCaseFeatures matrixCase
      && matrixCaseExpectedResult matrixCase == Formats

  featureRequiresPragma name = case Map.lookup name featureMap of
    Just feature -> featureKind feature /= Syntax
    Nothing -> False

  unclassifiedPragmaErrors =
    [ "unclassified LANGUAGE pragma " ++ name ++ " in " ++ path
    | (name, path) <- discoveredPragmas
    , Map.notMember name featureMap
    ]

duplicateErrors :: String -> [String] -> [String]
duplicateErrors description values =
  ["duplicate " ++ description ++ ": " ++ value | value <- duplicates values]

duplicates :: Ord value => [value] -> [value]
duplicates = Maybe.mapMaybe firstDuplicate
  . List.group
  . List.sort
 where
  firstDuplicate (value : _ : _) = Just value
  firstDuplicate _ = Nothing
