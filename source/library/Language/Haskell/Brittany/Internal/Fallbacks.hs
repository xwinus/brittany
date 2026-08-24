{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.Fallbacks
  ( FallbackId(..)
  , FallbackInfo(..)
  , FallbackScope(..)
  , FallbackSupport(..)
  , fallbackInfo
  , fallbackInventory
  , renderFallbackNotice
  ) where

import Data.Kind (Type)
import Language.Haskell.Brittany.Internal.Prelude

type FallbackId :: Type
data FallbackId
  = DataDeclarationFallback
  | DeclarationFallback
  | SignatureFallback
  | ImplicitParameterFallback
  | TypeClassDeclarationFallback
  | FamilyDefaultFallback
  | ExpressionFallback
  | TypeFallback
  | PatternFallback
  | StatementFallback
  | ImportFallback
  | ExactPrintOnlyFallback
  | WholeModuleFallback
  deriving (Bounded, Enum, Eq, Ord, Show)

type FallbackScope :: Type
data FallbackScope
  = InlineScope
  | DeclarationScope
  | ModuleScope
  deriving (Eq, Show)

type FallbackSupport :: Type
data FallbackSupport
  = ExactSourceSupport
  | SafetyNetSupport
  deriving (Eq, Show)

type FallbackInfo :: Type
data FallbackInfo = FallbackInfo
  { fallbackId :: FallbackId
  , fallbackScope :: FallbackScope
  , fallbackSupport :: FallbackSupport
  , fallbackTrigger :: String
  , fallbackReason :: String
  , fallbackFeatures :: [String]
  , fallbackTests :: [String]
  }
  deriving (Eq, Show)

fallbackInventory :: [FallbackInfo]
fallbackInventory = fallbackInfo <$> [minBound .. maxBound]

fallbackInfo :: FallbackId -> FallbackInfo
fallbackInfo fallback = FallbackInfo
  { fallbackId = fallback
  , fallbackScope = scope
  , fallbackSupport = support
  , fallbackTrigger = trigger
  , fallbackReason = reason
  , fallbackFeatures = features
  , fallbackTests = tests
  }
 where
  (scope, support, trigger, reason, features, tests) = case fallback of
    DataDeclarationFallback ->
      ( DeclarationScope
      , ExactSourceSupport
      , "data and newtype declaration shapes without a safe native layout"
      , "extended constructors and comments must remain attached"
      , ["DatatypeContexts", "ExistentialQuantification", "GADTs"]
      , ["source/test-suite/FallbackSpec.hs"]
      )
    DeclarationFallback ->
      ( DeclarationScope
      , ExactSourceSupport
      , "foreign, pragma, splice, and experimental top-level declarations"
      , "the declaration layouter does not model the complete syntax"
      , ["ForeignFunctionInterface", "TemplateHaskell", "WarningPragmas"]
      , ["source/test-suite/fixtures/DeclarationPragmasEdge.hs"]
      )
    SignatureFallback ->
      ( DeclarationScope
      , ExactSourceSupport
      , "specialisation and unsupported signature forms"
      , "signature-specific syntax is not represented by the native path"
      , ["SpecialisePragmas"]
      , ["source/test-suite/fixtures/SpecialisePragmasEdge.hs"]
      )
    ImplicitParameterFallback ->
      ( DeclarationScope
      , ExactSourceSupport
      , "implicit-parameter bindings outside the native binding subset"
      , "exact source avoids changing implicit binding syntax"
      , ["ImplicitParams"]
      , ["data/Test233.hs"]
      )
    TypeClassDeclarationFallback ->
      ( DeclarationScope
      , ExactSourceSupport
      , "type and class declaration forms outside the native subset"
      , "the native declaration path cannot preserve every binder shape"
      , ["TypeData", "TypeFamilies"]
      , ["source/test-suite/fixtures/TypeDataEdge.hs"]
      )
    FamilyDefaultFallback ->
      ( DeclarationScope
      , ExactSourceSupport
      , "default associated type-family equations"
      , "family equation layout still depends on exact annotations"
      , ["TypeFamilies"]
      , ["source/test-suite/fixtures/FamilyCommentsEdge.hs"]
      )
    ExpressionFallback ->
      ( InlineScope
      , ExactSourceSupport
      , "expressions without a comment-safe native layout"
      , "inline exact source preserves extension-specific punctuation"
      , ["OverloadedRecordUpdate", "TemplateHaskell"]
      , ["source/test-suite/fixtures/TemplateHaskellSyntaxEdge.hs"]
      )
    TypeFallback ->
      ( InlineScope
      , ExactSourceSupport
      , "type operators, splices, sums, and unsupported type forms"
      , "the type layouter cannot safely reconstruct all delimiters"
      , ["QuasiQuotes", "TypeOperators", "UnboxedSums"]
      , ["source/test-suite/fixtures/TypeDataEdge.hs"]
      )
    PatternFallback ->
      ( InlineScope
      , ExactSourceSupport
      , "patterns outside the native pattern subset"
      , "exact source preserves extension-specific pattern punctuation"
      , ["OrPatterns", "PatternSynonyms"]
      , ["source/test-suite/fixtures/OrPatternSyntaxEdge.hs"]
      )
    StatementFallback ->
      ( InlineScope
      , ExactSourceSupport
      , "statements outside the native statement subset"
      , "statement context and qualifiers are not fully modeled"
      , ["QualifiedDo", "RecursiveDo"]
      , ["source/test-suite/fixtures/DoSyntaxEdge.hs"]
      )
    ImportFallback ->
      ( DeclarationScope
      , ExactSourceSupport
      , "explicit-level and comment-sensitive imports"
      , "the exact source path retains import modifiers and comments"
      , ["ExplicitLevelImports", "ImportQualifiedPost"]
      , ["source/test-suite/fixtures/ExplicitLevelImportsEdge.hs"]
      )
    ExactPrintOnlyFallback ->
      ( DeclarationScope
      , ExactSourceSupport
      , "formatting is disabled for a declaration by configuration"
      , "the user explicitly requested exact-source round-tripping"
      , []
      , ["data/Test1.hs"]
      )
    WholeModuleFallback ->
      ( ModuleScope
      , SafetyNetSupport
      , "an unknown GHC AST node reaches a native layouter"
      , "the original module is returned instead of unsafe partial output"
      , []
      , ["source/test-suite/FallbackSpec.hs"]
      )

renderFallbackNotice :: FallbackId -> String -> String
renderFallbackNotice fallback location =
  "exact-source fallback "
    ++ show fallback
    ++ " ("
    ++ renderFallbackScope (fallbackScope $ fallbackInfo fallback)
    ++ " scope)"
    ++ " at "
    ++ location
    ++ ": "
    ++ fallbackReason (fallbackInfo fallback)

renderFallbackScope :: FallbackScope -> String
renderFallbackScope = \case
  InlineScope -> "inline"
  DeclarationScope -> "declaration"
  ModuleScope -> "module"
