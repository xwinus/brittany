{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.SemanticModel
  ( SemanticDifference(..)
  , SemanticField(..)
  , SemanticModel(..)
  , SemanticProjectionError(..)
  , firstDifference
  , renderSemanticPath
  ) where

import Data.Kind (Type)
import Language.Haskell.Brittany.Internal.Prelude

type SemanticModel :: Type
data SemanticModel
  = SemanticAtom String String
  | SemanticNode String [SemanticField]
  deriving (Eq, Ord, Show)

type SemanticField :: Type
data SemanticField = SemanticField String SemanticModel
  deriving (Eq, Ord, Show)

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

renderSemanticPath :: [String] -> String
renderSemanticPath = \case
  [] -> "root"
  elements -> intercalate " > " elements

firstDifference
  :: [String]
  -> SemanticModel
  -> SemanticModel
  -> Maybe SemanticDifference
firstDifference path input output
  | nodeSummary input /= nodeSummary output = difference path input output
  | otherwise = compareFields (nodeFields input) (nodeFields output)
 where
  compareFields inputFields outputFields = case (inputFields, outputFields) of
    ([], []) -> Nothing
    (SemanticField inputName inputValue : inputRest,
      SemanticField outputName outputValue : outputRest)
      | inputName /= outputName -> Just SemanticDifference
          { semanticDifferencePath = reverse $ inputName : path
          , semanticInputSummary = "field " ++ inputName
          , semanticOutputSummary = "field " ++ outputName
          }
      | otherwise ->
          firstDifference (inputName : path) inputValue outputValue
            <|> compareFields inputRest outputRest
    _ -> Just SemanticDifference
      { semanticDifferencePath = reverse path
      , semanticInputSummary = "field count " ++ show (length inputFields)
      , semanticOutputSummary = "field count " ++ show (length outputFields)
      }

difference
  :: [String]
  -> SemanticModel
  -> SemanticModel
  -> Maybe SemanticDifference
difference path input output = Just SemanticDifference
  { semanticDifferencePath = reverse path
  , semanticInputSummary = nodeSummary input
  , semanticOutputSummary = nodeSummary output
  }

nodeSummary :: SemanticModel -> String
nodeSummary = \case
  SemanticAtom typeName value -> typeName ++ " " ++ show value
  SemanticNode label _ -> label

nodeFields :: SemanticModel -> [SemanticField]
nodeFields = \case
  SemanticAtom{} -> []
  SemanticNode _ fields -> fields
