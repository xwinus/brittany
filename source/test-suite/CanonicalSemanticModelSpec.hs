{-# LANGUAGE LambdaCase #-}

module CanonicalSemanticModelSpec (spec) where

import qualified Data.Text as Text
import qualified GHC
import Language.Haskell.Brittany
  ( parsePrintModule
  , staticDefaultConfig
  )
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SemanticFingerprint
  ( SemanticDifference(..)
  , compareSemanticSyntax
  )
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "canonical semantic model" $ do
  Hspec.it "treats import declarations as a duplicate-preserving multiset" $
    assertEquivalent
      (source
        [ "module Imports where"
        , "import Data.Map"
        , "import Data.List"
        , "import Data.Map"
        ])
      (source
        [ "module Imports where"
        , "import Data.Map"
        , "import Data.Map"
        , "import Data.List"
        ])

  Hspec.it "canonicalizes import items, namespaces, and children" $
    assertEquivalent importItemsInput importItemsPermutation

  Hspec.it "normalizes singleton parenthesized deriving clauses" $
    assertEquivalent
      "module Deriving where\ndata Box = Box deriving (Show)\n"
      "module Deriving where\ndata Box = Box deriving Show\n"

  Hspec.it "does not discard duplicate import items" $ do
    assertEquivalent
      "module Duplicate where\nimport Data.List (map, nub, map)\n"
      "module Duplicate where\nimport Data.List (map, map, nub)\n"
    assertDifferent
      "module Duplicate where\nimport Data.List (map, nub, map)\n"
      "module Duplicate where\nimport Data.List (map, nub)\n"

  Hspec.it "retains every import declaration attribute" $
    mapM_ (uncurry assertDifferent) importAttributeMutations

  Hspec.it "retains import item modes, namespaces, wildcards, and names" $
    mapM_ (uncurry assertDifferent) importItemMutations

  Hspec.it "retains deriving strategies, via types, and class lists" $
    mapM_ (uncurry assertDifferent) derivingMutations

  Hspec.it "keeps declarations and expression syntax order-sensitive" $
    mapM_ (uncurry assertDifferent) orderedSyntaxMutations

  Hspec.it "reports explicit semantic field names" $ do
    difference <- semanticDifference
      "module Diagnostic where\nimport Data.List\n"
      "module Diagnostic where\nimport Data.Map\n"
    semanticDifferencePath difference `Hspec.shouldContain` ["imports"]
    semanticDifferencePath difference `Hspec.shouldContain` ["module"]
    show (semanticDifferencePath difference) `Hspec.shouldNotContain` "[0]"

  Hspec.it "formats canonicalized syntax and remains idempotent" $ do
    firstPass <- formatSource formattingInput
    firstPass `Hspec.shouldNotBe` formattingInput
    secondPass <- formatSource firstPass
    secondPass `Hspec.shouldBe` firstPass

assertEquivalent :: String -> String -> Hspec.Expectation
assertEquivalent inputSource outputSource = do
  input <- parseSource "CanonicalInput.hs" inputSource
  output <- parseSource "CanonicalOutput.hs" outputSource
  compareSemanticSyntax input output `Hspec.shouldBe` Right Nothing

assertDifferent :: String -> String -> Hspec.Expectation
assertDifferent inputSource outputSource = do
  difference <- semanticDifference inputSource outputSource
  semanticDifferencePath difference `Hspec.shouldNotBe` []
  semanticInputSummary difference
    `Hspec.shouldNotBe` semanticOutputSummary difference

semanticDifference :: String -> String -> IO SemanticDifference
semanticDifference inputSource outputSource = do
  input <- parseSource "MutationInput.hs" inputSource
  output <- parseSource "MutationOutput.hs" outputSource
  case compareSemanticSyntax input output of
    Right (Just difference) -> pure difference
    Left projectionError -> Hspec.expectationFailure (show projectionError)
      >> fail "semantic projection failed"
    Right Nothing -> Hspec.expectationFailure "mutation was accepted"
      >> fail "mutation was accepted"

parseSource :: FilePath -> String -> IO GHC.ParsedSource
parseSource filename input = do
  parsed <- ParseModule.parseModule [] filename
    (const $ pure $ Right ()) input
  case parsed of
    Left parseError -> Hspec.expectationFailure parseError >> fail parseError
    Right (_, parsedSource, ()) -> pure parsedSource

formatSource :: String -> IO String
formatSource input = parsePrintModule staticDefaultConfig (Text.pack input) >>= \case
  Left errors -> Hspec.expectationFailure
    ("formatting returned " ++ show (length errors) ++ " errors")
    >> fail "formatting failed"
  Right output -> pure $ Text.unpack output

source :: [String] -> String
source = unlines

importItemsInput :: String
importItemsInput = source
  [ "{-# LANGUAGE ExplicitNamespaces #-}"
  , "{-# LANGUAGE PatternSynonyms #-}"
  , "module ImportItems where"
  , "import Example (type TypeName, pattern Match, value, Tree(Leaf, Branch))"
  ]

importItemsPermutation :: String
importItemsPermutation = source
  [ "{-# LANGUAGE ExplicitNamespaces #-}"
  , "{-# LANGUAGE PatternSynonyms #-}"
  , "module ImportItems where"
  , "import Example (Tree(Branch, Leaf), value, pattern Match, type TypeName)"
  ]

importAttributeMutations :: [(String, String)]
importAttributeMutations =
  [ pair "import Data.List" "import Data.Map"
  , pairWith "PackageImports" "import \"base\" Data.List" "import \"other\" Data.List"
  , pair "import Data.List" "import {-# SOURCE #-} Data.List"
  , pair "import Data.List" "import safe Data.List"
  , pair "import Data.List" "import qualified Data.List"
  , pair "import qualified Data.List as List" "import qualified Data.List as Other"
  , pairWith "ImportQualifiedPost" "import qualified Data.List" "import Data.List qualified"
  , pairWith "ExplicitLevelImports" "import Data.List" "import splice Data.List"
  , pair "import Data.List (map)" "import Data.List hiding (map)"
  , pair "import Data.List" "import Data.List ()"
  ]

importItemMutations :: [(String, String)]
importItemMutations =
  [ pair "import Example (value)" "import Example (other)"
  , pairWith "ExplicitNamespaces" "import Example (TypeName)"
      "import Example (type TypeName)"
  , pair "import Example (Tree)" "import Example (Tree(..))"
  , pair "import Example (Tree(Leaf))" "import Example (Tree(Branch))"
  , pair "import Example (Tree(Leaf))" "import Example (Tree(Leaf, Branch))"
  ]

derivingMutations :: [(String, String)]
derivingMutations =
  [ modules "data Box = Box deriving Show" "data Box = Box deriving Eq"
  , withExtensions ["DerivingStrategies", "DeriveAnyClass"]
      "data Box = Box deriving stock Show"
      "data Box = Box deriving anyclass Show"
  , withExtensions ["DerivingVia", "GeneralizedNewtypeDeriving"]
      "newtype Box = Box Int deriving Eq via Int"
      "newtype Box = Box Int deriving Eq via Integer"
  , modules "data Box = Box deriving (Eq, Show)"
      "data Box = Box deriving (Show, Eq)"
  ]

orderedSyntaxMutations :: [(String, String)]
orderedSyntaxMutations =
  [ ( "module Mutation {-# WARNING \"first\" #-} where\nvalue = 1\n"
    , "module Mutation {-# WARNING \"second\" #-} where\nvalue = 1\n"
    )
  , modules "first = 1\nsecond = 2" "second = 2\nfirst = 1"
  , modules "value = do { first; second }" "value = do { second; first }"
  , modules "value x | x > 0 = 1 | otherwise = 2"
      "value x | otherwise = 2 | x > 0 = 1"
  , modules "value True = 1\nvalue False = 0"
      "value False = 0\nvalue True = 1"
  , modules "value (left, right) = left" "value (right, left) = left"
  ]

formattingInput :: String
formattingInput = source
  [ "module Format where"
  , "import Data.Map"
  , "import Data.List (nub, map)"
  , "data Box = Box deriving (Show)"
  ]

pair :: String -> String -> (String, String)
pair = pairWithNoExtension

pairWithNoExtension :: String -> String -> (String, String)
pairWithNoExtension left right = modules left right

pairWith :: String -> String -> String -> (String, String)
pairWith extension = withExtensions [extension]

modules :: String -> String -> (String, String)
modules = withExtensions []

withExtensions :: [String] -> String -> String -> (String, String)
withExtensions extensions left right =
  (moduleSource left, moduleSource right)
 where
  moduleSource body = source
    $ map (\extension -> "{-# LANGUAGE " ++ extension ++ " #-}") extensions
    ++ ["module Mutation where", body]
