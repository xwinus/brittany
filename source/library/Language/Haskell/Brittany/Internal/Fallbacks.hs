{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.Fallbacks
  ( FallbackId(..)
  , FallbackInfo(..)
  , FallbackScope(..)
  , FallbackSupport(..)
  , OpaqueFamily(..)
  , RenderDisposition(..)
  , RenderNotice(..)
  , fallbackRenderNotice
  , opaqueRenderNotice
  , untypedSpliceFamily
  , fallbackInfo
  , fallbackInventory
  , renderFallbackNotice
  , renderRenderInventory
  , renderRenderNotice
  ) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Kind (Type)
import qualified Data.Map as Map
import GHC.Hs (HsUntypedSplice(..))
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
  deriving (Eq, Ord, Show)

type FallbackSupport :: Type
data FallbackSupport
  = ExactSourceSupport
  | SafetyNetSupport
  deriving (Eq, Show)

type OpaqueFamily :: Type
data OpaqueFamily
  = QuasiQuote
  | TemplateHaskellQuote
  | TemplateHaskellSplice
  deriving (Bounded, Enum, Eq, Ord, Show)

type RenderDisposition :: Type
data RenderDisposition
  = NativeLayout
  | SupportedOpaque OpaqueFamily
  | UnsupportedFallback FallbackId
  | WholeModuleFallbackDisposition
  deriving (Eq, Ord, Show)

type RenderNotice :: Type
data RenderNotice = RenderNotice
  { renderDisposition :: RenderDisposition
  , renderScope :: FallbackScope
  , renderLocation :: String
  , renderReason :: String
  }
  deriving (Eq, Ord, Show)

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
      , "unsupported Haskell 98 and GADT constructors must remain attached"
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
      , "type, class, and instance head forms outside the native subset"
      , "the native declaration path cannot preserve every binder and type shape"
      , ["TypeData", "TypeFamilies"]
      , [ "source/test-suite/fixtures/TypeDataEdge.hs"
        , "source/test-suite/fixtures/InstanceHeadUnsupported.hs"
        ]
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
      , ["OverloadedRecordUpdate", "QuasiQuotes", "TemplateHaskell"]
      , [ "source/test-suite/fixtures/TemplateHaskellSyntaxEdge.hs"
        , "source/test-suite/fixtures/ScopedExpressionFallbackInput.hs"
        ]
      )
    TypeFallback ->
      ( InlineScope
      , ExactSourceSupport
      , "type splices, sums, and unsupported type forms"
      , "the type layouter cannot safely reconstruct all delimiters"
      , ["QuasiQuotes", "UnboxedSums"]
      , ["source/test-suite/fixtures/UnliftedSyntaxEdge.hs"]
      )
    PatternFallback ->
      ( InlineScope
      , ExactSourceSupport
      , "patterns outside the native pattern subset"
      , "exact source preserves extension-specific pattern punctuation"
      , ["OrPatterns", "PatternSynonyms", "QuasiQuotes"]
      , [ "source/test-suite/fixtures/OrPatternSyntaxEdge.hs"
        , "source/test-suite/fixtures/ScopedPatternFallbackInput.hs"
        ]
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
renderFallbackNotice fallback location = renderRenderNotice
  $ fallbackRenderNotice fallback location

fallbackRenderNotice :: FallbackId -> String -> RenderNotice
fallbackRenderNotice fallback location = RenderNotice
  { renderDisposition = if fallback == WholeModuleFallback
      then WholeModuleFallbackDisposition
      else UnsupportedFallback fallback
  , renderScope = fallbackScope info
  , renderLocation = location
  , renderReason = fallbackReason info
  }
 where
  info = fallbackInfo fallback

opaqueRenderNotice :: OpaqueFamily -> FallbackScope -> String -> RenderNotice
opaqueRenderNotice family scope location = RenderNotice
  { renderDisposition = SupportedOpaque family
  , renderScope = scope
  , renderLocation = location
  , renderReason = opaqueFamilyReason family
  }

renderRenderNotice :: RenderNotice -> String
renderRenderNotice notice =
  renderDispositionText (renderDisposition notice)
    ++ " ("
    ++ renderFallbackScope (renderScope notice)
    ++ " scope)"
    ++ " at "
    ++ renderLocation notice
    ++ ": "
    ++ renderReason notice

renderRenderInventory :: [RenderNotice] -> String
renderRenderInventory notices = ByteString.unpack $ Aeson.encode $ Aeson.object
  [ "schemaVersion" .= (1 :: Int)
  , "summary" .= Aeson.object
    [ "actionableFallbackOccurrences" .= length fallbackNotices
    , "actionableFallbackUniqueFamilies" .= uniqueFamilyCount
      fallbackFamily fallbackNotices
    , "actionableFallbackFamilies" .= renderCounts fallbackFamily fallbackNotices
    , "supportedOpaqueOccurrences" .= length opaqueNotices
    , "supportedOpaqueUniqueFamilies" .= uniqueFamilyCount
      opaqueFamily opaqueNotices
    , "supportedOpaqueFamilies" .= renderCounts opaqueFamily opaqueNotices
    ]
  , "occurrences" .= fmap noticeValue notices
  ]
 where
  fallbackNotices = filter isFallback notices
  opaqueNotices = filter isOpaque notices

  isFallback notice = case renderDisposition notice of
    UnsupportedFallback{} -> True
    WholeModuleFallbackDisposition -> True
    _ -> False

  isOpaque notice = case renderDisposition notice of
    SupportedOpaque{} -> True
    _ -> False

  fallbackFamily notice = case renderDisposition notice of
    UnsupportedFallback fallback -> show fallback
    WholeModuleFallbackDisposition -> show WholeModuleFallback
    _ -> ""

  opaqueFamily notice = case renderDisposition notice of
    SupportedOpaque family -> show family
    _ -> ""

  renderCounts familyName selected =
    [ Aeson.object
      [ "family" .= family
      , "occurrences" .= count
      ]
    | (family, count) <- Map.toAscList
      $ Map.fromListWith (+) [(familyName notice, 1 :: Int) | notice <- selected]
    ]

  uniqueFamilyCount familyName selected = Map.size $ Map.fromList
    [(familyName notice, ()) | notice <- selected]

noticeValue :: RenderNotice -> Aeson.Value
noticeValue notice = Aeson.object
  [ "disposition" .= dispositionName (renderDisposition notice)
  , "family" .= dispositionFamily (renderDisposition notice)
  , "scope" .= renderFallbackScope (renderScope notice)
  , "location" .= renderLocation notice
  , "reason" .= renderReason notice
  ]

dispositionName :: RenderDisposition -> String
dispositionName = \case
  NativeLayout -> "native-layout"
  SupportedOpaque{} -> "supported-opaque"
  UnsupportedFallback{} -> "unsupported-fallback"
  WholeModuleFallbackDisposition -> "whole-module-fallback"

dispositionFamily :: RenderDisposition -> Maybe String
dispositionFamily = \case
  NativeLayout -> Nothing
  SupportedOpaque family -> Just $ show family
  UnsupportedFallback fallback -> Just $ show fallback
  WholeModuleFallbackDisposition -> Just $ show WholeModuleFallback

renderDispositionText :: RenderDisposition -> String
renderDispositionText = \case
  NativeLayout -> "native layout"
  SupportedOpaque family -> "supported opaque " ++ show family
  UnsupportedFallback fallback -> "exact-source fallback " ++ show fallback
  WholeModuleFallbackDisposition ->
    "exact-source fallback " ++ show WholeModuleFallback

opaqueFamilyReason :: OpaqueFamily -> String
opaqueFamilyReason = \case
  QuasiQuote ->
    "the extension-owned quasiquote payload is preserved byte-for-byte"
  TemplateHaskellQuote ->
    "the Template Haskell quotation is an atomic exact-source boundary"
  TemplateHaskellSplice ->
    "the Template Haskell splice is an atomic exact-source boundary"

untypedSpliceFamily :: HsUntypedSplice GhcPs -> OpaqueFamily
untypedSpliceFamily = \case
  HsQuasiQuote{} -> QuasiQuote
  _ -> TemplateHaskellSplice

renderFallbackScope :: FallbackScope -> String
renderFallbackScope = \case
  InlineScope -> "inline"
  DeclarationScope -> "declaration"
  ModuleScope -> "module"
