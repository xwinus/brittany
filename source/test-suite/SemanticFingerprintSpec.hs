{-# LANGUAGE StandaloneKindSignatures #-}

module SemanticFingerprintSpec (spec) where

import qualified Data.Data as Data
import Data.Kind (Type)
import qualified GHC
import Language.Haskell.Brittany.Internal (semanticErrors)
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SemanticFingerprint
  ( SemanticDifference(..)
  , SemanticProjectionError(..)
  , compareSemanticSyntax
  , semanticFingerprint
  )
import Language.Haskell.Brittany.Internal.Types (BrittanyError(..))
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified Test.Hspec as Hspec

type Mutation :: Type
data Mutation = Mutation String String String

type UnknownSyntax :: Type
data UnknownSyntax = UnknownSyntax

instance Data.Data UnknownSyntax where
  gunfold _ _ _ = error "UnknownSyntax cannot be constructed generically"
  toConstr _ = error "UnknownSyntax has no generic constructor"
  dataTypeOf _ = Data.mkNoRepType "UnknownSyntax"

spec :: Hspec.Spec
spec = Hspec.describe "semantic parsed-AST validation" $ do
  Hspec.it "ignores comments whitespace and source literal spelling" $ do
    input <- parseSource "EquivalentInput.hs" $ source
      [ "module Equivalent where"
      , "value=0x10"
      ]
    output <- parseSource "EquivalentOutput.hs" $ source
      [ "module Equivalent where"
      , ""
      , "-- formatting-only comment"
      , "value = 16"
      ]
    compareSemanticSyntax input output `Hspec.shouldBe` Right Nothing

  Hspec.it "ignores layout changes across imports and declarations" $ do
    input <- parseSource "LayoutInput.hs" $ source
      [ "module Layout where"
      , "import qualified Data.List as List"
      , "convert values = let mapped = List.map (+ 1) values in mapped"
      ]
    twoSpaceOutput <- parseSource "LayoutTwoSpaceOutput.hs" $ source
      [ "module Layout where"
      , ""
      , "import qualified Data.List as List"
      , ""
      , "convert values ="
      , "  let mapped = List.map (+ 1) values"
      , "  in mapped"
      ]
    fourSpaceOutput <- parseSource "LayoutFourSpaceOutput.hs" $ source
      [ "module Layout where"
      , ""
      , "import qualified Data.List as List"
      , ""
      , "convert values ="
      , "    let mapped = List.map (+ 1) values"
      , "    in mapped"
      ]
    compareSemanticSyntax input twoSpaceOutput `Hspec.shouldBe` Right Nothing
    compareSemanticSyntax input fourSpaceOutput `Hspec.shouldBe` Right Nothing

  Hspec.it "preserves supported opaque and exact-source payloads" $ do
    input <- parseSource "OpaqueInput.hs" opaqueInputSource
    output <- parseSource "OpaqueOutput.hs" opaqueOutputSource
    compareSemanticSyntax input output `Hspec.shouldBe` Right Nothing

  Hspec.it "normalizes redundant expression parentheses" $ do
    input <- parseSource "ParenthesizedInput.hs" $ source
      [ "module Parenthesized where"
      , "value = (((Just ((1)))))"
      ]
    output <- parseSource "ParenthesizedOutput.hs" $ source
      [ "module Parenthesized where"
      , "value = Just 1"
      ]
    compareSemanticSyntax input output `Hspec.shouldBe` Right Nothing

  Hspec.it "rejects unknown generic representations conservatively" $
    case semanticFingerprint UnknownSyntax of
      Left projectionError -> do
        projectionErrorPath projectionError `Hspec.shouldBe` []
        projectionErrorType projectionError
          `Hspec.shouldContain` "UnknownSyntax"
        Brittany.shouldEmitOutput False False True True
          [ErrorSemanticProjection "root" "UnknownSyntax"]
          `Hspec.shouldBe` False
      Right{} -> Hspec.expectationFailure
        "expected an unknown semantic projection to fail"

  Hspec.it "reports the #101 lazy-field regression through the fatal gate" $ do
    input <- parseSource "LazyFieldInput.hs" $ source
      [ "{-# LANGUAGE StrictData #-}"
      , "module LazyField where"
      , "data Box = Box { field :: ~Int }"
      ]
    output <- parseSource "LazyFieldOutput.hs" $ source
      [ "{-# LANGUAGE StrictData #-}"
      , "module LazyField where"
      , "data Box = Box { field :: Int }"
      ]
    case semanticErrors input output of
      [ErrorSemanticChange path inputSummary outputSummary] -> do
        path `Hspec.shouldNotBe` "root"
        inputSummary `Hspec.shouldNotBe` outputSummary
        Brittany.shouldEmitOutput False False True True
          (semanticErrors input output) `Hspec.shouldBe` False
      errors -> Hspec.expectationFailure
        $ "expected one semantic change, got " ++ show (length errors)

  mapM_ mutationExample mutations

mutationExample :: Mutation -> Hspec.SpecWith ()
mutationExample (Mutation description inputSource outputSource) =
  Hspec.it description $ do
    input <- parseSource "MutationInput.hs" inputSource
    output <- parseSource "MutationOutput.hs" outputSource
    case compareSemanticSyntax input output of
      Right (Just difference) -> assertActionable difference
      Left projectionError -> Hspec.expectationFailure
        $ "projection failed at " ++ show (projectionErrorPath projectionError)
        ++ ": "
        ++ projectionErrorType projectionError
      Right Nothing -> Hspec.expectationFailure
        "syntax-affecting mutation was accepted"

assertActionable :: SemanticDifference -> Hspec.Expectation
assertActionable difference = do
  semanticDifferencePath difference `Hspec.shouldNotBe` []
  semanticInputSummary difference
    `Hspec.shouldNotBe` semanticOutputSummary difference

parseSource :: FilePath -> String -> IO GHC.ParsedSource
parseSource filename input = do
  parsed <- ParseModule.parseModule ["-haddock"] filename
    (const $ pure $ Right ()) input
  case parsed of
    Left parseError -> Hspec.expectationFailure parseError >> fail parseError
    Right (_, parsedSource, ()) -> pure parsedSource

source :: [String] -> String
source = unlines

opaqueInputSource :: String
opaqueInputSource = source
  [ "{-# LANGUAGE QuasiQuotes #-}"
  , "module Opaque where"
  , "{-# SPECIALISE identity::Int->Int #-}"
  , "identity value=value"
  , "payload=[raw|{- unchanged opaque payload -}|]"
  ]

opaqueOutputSource :: String
opaqueOutputSource = source
  [ "{-# LANGUAGE QuasiQuotes #-}"
  , "module Opaque where"
  , "{-# SPECIALISE identity :: Int -> Int #-}"
  , "identity value = value"
  , "payload = [raw|{- unchanged opaque payload -}|]"
  ]

mutations :: [Mutation]
mutations =
  [ Mutation "detects constructor field strictness changes"
      (source
        [ "{-# LANGUAGE StrictData #-}"
        , "module Mutation where"
        , "data Box = Box { field :: ~Int }"
        ])
      (source
        [ "{-# LANGUAGE StrictData #-}"
        , "module Mutation where"
        , "data Box = Box { field :: !Int }"
        ])
  , Mutation "detects constructor field unpacking changes"
      (source
        [ "module Mutation where"
        , "data Box = Box { field :: {-# UNPACK #-} !Int }"
        ])
      (source
        [ "module Mutation where"
        , "data Box = Box { field :: !Int }"
        ])
  , Mutation "detects strict pattern changes"
      (source
        [ "{-# LANGUAGE BangPatterns #-}"
        , "module Mutation where"
        , "value !argument = argument"
        ])
      (source
        [ "{-# LANGUAGE BangPatterns #-}"
        , "module Mutation where"
        , "value argument = argument"
        ])
  , Mutation "detects deriving-via changes"
      (source
        [ "{-# LANGUAGE DerivingVia #-}"
        , "{-# LANGUAGE StandaloneDeriving #-}"
        , "module Mutation where"
        , "newtype Box a = Box a"
        , "deriving instance Eq a => Eq (Box a)"
        ])
      (source
        [ "{-# LANGUAGE DerivingVia #-}"
        , "{-# LANGUAGE StandaloneDeriving #-}"
        , "module Mutation where"
        , "newtype Box a = Box a"
        , "deriving via Maybe a instance Eq a => Eq (Box a)"
        ])
  , Mutation "detects deriving strategy changes"
      (source
        [ "{-# LANGUAGE DeriveAnyClass #-}"
        , "{-# LANGUAGE DerivingStrategies #-}"
        , "{-# LANGUAGE StandaloneDeriving #-}"
        , "module Mutation where"
        , "data Box = Box"
        , "deriving stock instance Eq Box"
        ])
      (source
        [ "{-# LANGUAGE DeriveAnyClass #-}"
        , "{-# LANGUAGE DerivingStrategies #-}"
        , "{-# LANGUAGE StandaloneDeriving #-}"
        , "module Mutation where"
        , "data Box = Box"
        , "deriving anyclass instance Eq Box"
        ])
  , Mutation "detects role annotation changes"
      (source
        [ "{-# LANGUAGE RoleAnnotations #-}"
        , "module Mutation where"
        , "type role Box nominal"
        , "data Box a = Box a"
        ])
      (source
        [ "{-# LANGUAGE RoleAnnotations #-}"
        , "module Mutation where"
        , "type role Box representational"
        , "data Box a = Box a"
        ])
  , Mutation "detects multiplicity changes"
      (source
        [ "{-# LANGUAGE LinearTypes #-}"
        , "module Mutation where"
        , "value :: a %1 -> a"
        , "value argument = argument"
        ])
      (source
        [ "{-# LANGUAGE LinearTypes #-}"
        , "module Mutation where"
        , "value :: a %Many -> a"
        , "value argument = argument"
        ])
  , Mutation "detects foreign-call safety changes"
      (source
        [ "{-# LANGUAGE ForeignFunctionInterface #-}"
        , "module Mutation where"
        , "foreign import ccall safe \"operation\""
        , "  operation :: Int -> IO Int"
        ])
      (source
        [ "{-# LANGUAGE ForeignFunctionInterface #-}"
        , "module Mutation where"
        , "foreign import ccall unsafe \"operation\""
        , "  operation :: Int -> IO Int"
        ])
  , Mutation "detects foreign calling-convention changes"
      (source
        [ "{-# LANGUAGE CApiFFI #-}"
        , "{-# LANGUAGE ForeignFunctionInterface #-}"
        , "module Mutation where"
        , "foreign import ccall unsafe \"operation\""
        , "  operation :: Int -> IO Int"
        ])
      (source
        [ "{-# LANGUAGE CApiFFI #-}"
        , "{-# LANGUAGE ForeignFunctionInterface #-}"
        , "module Mutation where"
        , "foreign import capi unsafe \"operation\""
        , "  operation :: Int -> IO Int"
        ])
  , Mutation "detects explicit namespace changes"
      (source
        [ "{-# LANGUAGE ExplicitNamespaces #-}"
        , "module Mutation (type Box) where"
        , "type Box = Int"
        ])
      (source
        [ "{-# LANGUAGE ExplicitNamespaces #-}"
        , "module Mutation (Box) where"
        , "type Box = Int"
        ])
  , Mutation "detects promoted-name changes"
      (source
        [ "{-# LANGUAGE DataKinds #-}"
        , "module Mutation where"
        , "type Flag = 'True"
        ])
      (source
        [ "{-# LANGUAGE DataKinds #-}"
        , "module Mutation where"
        , "type Flag = True"
        ])
  , Mutation "detects record pun changes"
      (source
        [ "{-# LANGUAGE NamedFieldPuns #-}"
        , "module Mutation where"
        , "data Box = Box { field :: Int }"
        , "readField Box { field } = field"
        ])
      (source
        [ "{-# LANGUAGE NamedFieldPuns #-}"
        , "module Mutation where"
        , "data Box = Box { field :: Int }"
        , "readField Box { field = field } = field"
        ])
  , Mutation "detects record wildcard changes"
      (source
        [ "{-# LANGUAGE NamedFieldPuns #-}"
        , "{-# LANGUAGE RecordWildCards #-}"
        , "module Mutation where"
        , "data Box = Box { field :: Int, other :: Int }"
        , "readField Box { field, .. } = field + other"
        ])
      (source
        [ "{-# LANGUAGE NamedFieldPuns #-}"
        , "{-# LANGUAGE RecordWildCards #-}"
        , "module Mutation where"
        , "data Box = Box { field :: Int, other :: Int }"
        , "readField Box { field, other } = field + other"
        ])
  , Mutation "detects literal value changes"
      (source
        [ "module Mutation where"
        , "value = 1"
        ])
      (source
        [ "module Mutation where"
        , "value = 2"
        ])
  , Mutation "detects declaration pragma changes"
      (source
        [ "module Mutation where"
        , "{-# INLINE value #-}"
        , "value = 1"
        ])
      (source
        [ "module Mutation where"
        , "{-# NOINLINE value #-}"
        , "value = 1"
        ])
  , Mutation "detects instance overlap changes"
      (source
        [ "{-# LANGUAGE FlexibleInstances #-}"
        , "module Mutation where"
        , "class Marker value"
        , "instance {-# OVERLAPPABLE #-} Marker Int"
        ])
      (source
        [ "{-# LANGUAGE FlexibleInstances #-}"
        , "module Mutation where"
        , "class Marker value"
        , "instance {-# OVERLAPPING #-} Marker Int"
        ])
  ]
