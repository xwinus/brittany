{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.SemanticFingerprint
  ( SemanticDifference(..)
  , SemanticFingerprint
  , SemanticProjectionError(..)
  , compareSemanticSyntax
  , renderSemanticPath
  , semanticFingerprint
  ) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Short as ShortByteString
import qualified Data.Data as Data
import Data.Kind (Type)
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified GHC.Data.FastString as FastString
import qualified GHC.Types.Name as Name
import qualified GHC.Types.Name.Occurrence as Occurrence
import qualified Language.Haskell.Syntax.Module.Name as ModuleName
import Language.Haskell.Brittany.Internal.Prelude

type SemanticFingerprint :: Type
data SemanticFingerprint
  = SemanticAtom String String
  | SemanticNode String [SemanticFingerprint]
  deriving (Eq, Show)

type SemanticProjectionError :: Type
data SemanticProjectionError = SemanticProjectionError
  { projectionErrorPath :: [String]
  , projectionErrorType :: String
  }
  deriving (Eq, Show)

type SemanticDifference :: Type
data SemanticDifference = SemanticDifference
  { semanticDifferencePath :: [String]
  , semanticInputSummary :: String
  , semanticOutputSummary :: String
  }
  deriving (Eq, Show)

semanticFingerprint
  :: Data.Data ast
  => ast
  -> Either SemanticProjectionError SemanticFingerprint
semanticFingerprint value = do
  projected <- projectValue [] value
  case projected of
    Just fingerprint -> pure fingerprint
    Nothing -> Left SemanticProjectionError
      { projectionErrorPath = ["root"]
      , projectionErrorType = "ignored root syntax"
      }

compareSemanticSyntax
  :: (Data.Data input, Data.Data output)
  => input
  -> output
  -> Either SemanticProjectionError (Maybe SemanticDifference)
compareSemanticSyntax input output = do
  inputFingerprint <- semanticFingerprint input
  outputFingerprint <- semanticFingerprint output
  pure $ firstDifference [] inputFingerprint outputFingerprint

renderSemanticPath :: [String] -> String
renderSemanticPath = \case
  [] -> "root"
  elements -> intercalate " > " elements

projectValue
  :: forall ast
   . Data.Data ast
  => [String]
  -> ast
  -> Either SemanticProjectionError (Maybe SemanticFingerprint)
projectValue path value
  | ignoredRepresentation typeRepresentation = pure Nothing
  | Just atom <- atomicValue value =
      pure $ Just $ SemanticAtom typeName atom
  | otherwise = case Data.dataTypeRep $ Data.dataTypeOf value of
      Data.NoRep -> Left SemanticProjectionError
        { projectionErrorPath = reverse path
        , projectionErrorType = qualifiedTypeName
        }
      _ -> do
        children <- sequence $ zipWith projectChild [0 :: Int ..]
          $ Data.gmapQ Box value
        let semanticChildren = catMaybes children
            label = typeName ++ "." ++ Data.showConstr (Data.toConstr value)
        pure $ normalizeNode typeName label semanticChildren
 where
  typeRepresentation = Typeable.typeOf value
  typeConstructor = Typeable.typeRepTyCon typeRepresentation
  typeModule = Typeable.tyConModule typeConstructor
  typeName = Typeable.tyConName typeConstructor
  qualifiedTypeName = typeModule ++ "." ++ typeName

  projectChild index (Box child) =
    projectValue ((typeName ++ "[" ++ show index ++ "]") : path) child

type Box :: Type
data Box = forall value. Data.Data value => Box value

normalizeNode
  :: String
  -> String
  -> [SemanticFingerprint]
  -> Maybe SemanticFingerprint
normalizeNode typeName label children
  | typeName == "GenLocated" = singleChild children
  | constructorName label `elem` redundantParentheses = payloadChild children
  | otherwise = Just $ SemanticNode label children
 where
  redundantParentheses = ["HsPar", "HsParTy", "ParPat"]

  constructorName = reverse . takeWhile (/= '.') . reverse

  singleChild = \case
    [child] -> Just child
    _ -> Just $ SemanticNode label children

  payloadChild = \case
    [] -> Just $ SemanticNode label children
    -- Parsed parentheses store token metadata before their semantic payload.
    child : semanticChildren ->
      Just $ foldl (\_ nextChild -> nextChild) child semanticChildren

ignoredRepresentation :: Typeable.TypeRep -> Bool
ignoredRepresentation representation =
  ignoredType typeModule typeName
    || (not $ null typeArguments)
      && all ignoredRepresentation typeArguments
 where
  (typeConstructor, typeArguments) = Typeable.splitTyConApp representation
  typeModule = Typeable.tyConModule typeConstructor
  typeName = Typeable.tyConName typeConstructor

ignoredType :: String -> String -> Bool
ignoredType typeModule typeName = or
  [ typeName /= "GenLocated"
      && "GHC.Types.SrcLoc" `isPrefixOf` typeModule
  , typeName /= "GenLocated"
      && "GHC.Parser.Annotation" `isPrefixOf` typeModule
  , typeModule == "GHC.Types.SourceText" && typeName == "SourceText"
  , typeName `elem`
      [ "HsDocString"
      , "HsDocStringChunk"
      , "WithHsDocIdentifiers"
      ]
  ]

atomicValue :: forall value. Data.Data value => value -> Maybe String
atomicValue value = asum
  [ show <$> (Data.cast value :: Maybe Int)
  , show <$> (Data.cast value :: Maybe Integer)
  , show <$> (Data.cast value :: Maybe Word)
  , show <$> (Data.cast value :: Maybe Float)
  , show <$> (Data.cast value :: Maybe Double)
  , show <$> (Data.cast value :: Maybe Char)
  , Text.unpack <$> (Data.cast value :: Maybe Text.Text)
  , FastString.unpackFS <$> (Data.cast value :: Maybe FastString.FastString)
  , ModuleName.moduleNameString
      <$> (Data.cast value :: Maybe ModuleName.ModuleName)
  , Occurrence.occNameString
      <$> (Data.cast value :: Maybe Occurrence.OccName)
  , Name.nameStableString <$> (Data.cast value :: Maybe Name.Name)
  , show <$> (Data.cast value :: Maybe ByteString.ByteString)
  , show <$> (Data.cast value :: Maybe ShortByteString.ShortByteString)
  ]

firstDifference
  :: [String]
  -> SemanticFingerprint
  -> SemanticFingerprint
  -> Maybe SemanticDifference
firstDifference path input output
  | nodeSummary input /= nodeSummary output = Just SemanticDifference
      { semanticDifferencePath = reverse path
      , semanticInputSummary = nodeSummary input
      , semanticOutputSummary = nodeSummary output
      }
  | otherwise = compareChildren 0 (nodeChildren input) (nodeChildren output)
 where
  compareChildren
    :: Int
    -> [SemanticFingerprint]
    -> [SemanticFingerprint]
    -> Maybe SemanticDifference
  compareChildren index inputChildren outputChildren =
    case (inputChildren, outputChildren) of
      ([], []) -> Nothing
      (inputChild : inputRest, outputChild : outputRest) ->
        firstDifference
          ((nodeLabel input ++ "[" ++ show index ++ "]") : path)
          inputChild
          outputChild
          <|> compareChildren (index + 1) inputRest outputRest
      _ -> Just SemanticDifference
        { semanticDifferencePath = reverse
            $ (nodeLabel input ++ ".children") : path
        , semanticInputSummary =
            "child count " ++ show (length $ nodeChildren input)
        , semanticOutputSummary =
            "child count " ++ show (length $ nodeChildren output)
        }

nodeLabel :: SemanticFingerprint -> String
nodeLabel = \case
  SemanticAtom typeName _ -> typeName
  SemanticNode label _ -> label

nodeSummary :: SemanticFingerprint -> String
nodeSummary = \case
  SemanticAtom typeName value -> typeName ++ " " ++ show value
  SemanticNode label _ -> label

nodeChildren :: SemanticFingerprint -> [SemanticFingerprint]
nodeChildren = \case
  SemanticAtom{} -> []
  SemanticNode _ children -> children
