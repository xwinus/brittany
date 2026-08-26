{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.Layouters.StandaloneDeriving
  ( layoutStandaloneDeriving
  ) where

import qualified Data.Text as Text
import Data.Kind (Type)
import GHC (GenLocated(L))
import GHC.Hs
import qualified GHC.OldList as List
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.IE (toL)
import Language.Haskell.Brittany.Internal.Layouters.Instance
  ( overlapPragma
  , supportsOverlapMode
  )
import Language.Haskell.Brittany.Internal.Layouters.Type
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.TypeFallbacks (exactSourceTypes)
import Language.Haskell.Brittany.Internal.Types

type StrategyLayout :: Type
data StrategyLayout
  = NoStrategy
  | KeywordStrategy Text
  | ViaStrategyType (LHsSigType GhcPs)

layoutStandaloneDeriving
  :: DerivDecl GhcPs -> Maybe (ToBriDocM BriDocNumbered)
layoutStandaloneDeriving declaration@DerivDecl{}
  | not (null $ exactSourceTypes declaration) = Nothing
  | not (supportsOverlapMode $ deriv_overlap_mode declaration) = Nothing
  | not (supportsHeadBinders $ deriv_type declaration) = Nothing
  | otherwise = do
      let strategy = standaloneStrategy $ deriv_strategy declaration
          (binders, body) = standaloneHead $ deriv_type declaration
      viaType <- traverse viaTypeBody $ case strategy of
        ViaStrategyType signature -> Just signature
        _ -> Nothing
      pure $ do
        bodyDoc <- docSharedWrapper layoutType (toL body)
        binderDocs <- case binders of
          HsOuterExplicit _ variables -> Just <$> layoutTyVarBndrs variables
          HsOuterImplicit{} -> pure Nothing
        viaDoc <- traverse (docSharedWrapper layoutType . toL) viaType
        let pragmaDocs = maybe [] (pure . docLit . Text.pack)
              (overlapPragma =<< deriv_overlap_mode declaration)
            instanceLine = spacedWords $ docLitS "instance" : pragmaDocs
            compactHead = case binderDocs of
              Nothing -> docForceSingleline bodyDoc
              Just variables -> docSeq
                $ docLitS "forall"
                : processTyVarBndrsSingleline variables
                ++ [docLitS ". ", docForceSingleline bodyDoc]
            multilineHead = case binderDocs of
              Nothing -> bodyDoc
              Just variables -> docLines
                [ docSeq
                  $ docLitS "forall"
                  : processTyVarBndrsSingleline variables
                  ++ [docLitS "."]
                , bodyDoc
                ]
            compactPrefix = spacedWords
              $ compactWords strategy viaDoc
              ++ [docLitS "instance"]
              ++ pragmaDocs
            compact = docSeq [appSep compactPrefix, compactHead]
            indentedHead = regularIndent multilineHead
            multiline = case (strategy, viaDoc) of
              (ViaStrategyType{}, Just renderedVia) -> docLines
                [ docLitS "deriving via"
                , regularIndent $ docLines
                  [ renderedVia
                  , instanceLine
                  , indentedHead
                  ]
                ]
              _ -> docLines
                [ spacedWords $ multilineWords strategy ++ [instanceLine]
                , indentedHead
                ]
        docAlt [compact, multiline]

standaloneHead
  :: LHsSigWcType GhcPs
  -> (HsOuterSigTyVarBndrs GhcPs, LHsType GhcPs)
standaloneHead (HsWC _ (L _ (HsSig _ binders body))) = (binders, body)

supportsHeadBinders :: LHsSigWcType GhcPs -> Bool
supportsHeadBinders signature = case fst $ standaloneHead signature of
  HsOuterImplicit{} -> True
  HsOuterExplicit _ binders -> all supportsBinder binders
 where
  supportsBinder (L _ (HsTvb _ _ HsBndrVar{} _)) = True
  supportsBinder _ = False

standaloneStrategy
  :: Maybe (LDerivStrategy GhcPs) -> StrategyLayout
standaloneStrategy = \case
  Nothing -> NoStrategy
  Just (L _ StockStrategy{}) -> KeywordStrategy $ Text.pack "stock"
  Just (L _ AnyclassStrategy{}) ->
    KeywordStrategy $ Text.pack "anyclass"
  Just (L _ NewtypeStrategy{}) ->
    KeywordStrategy $ Text.pack "newtype"
  Just (L _ (ViaStrategy (XViaStrategyPs _ signature))) ->
    ViaStrategyType signature

viaTypeBody :: LHsSigType GhcPs -> Maybe (LHsType GhcPs)
viaTypeBody = \case
  L _ (HsSig _ HsOuterImplicit{} body) -> Just body
  L _ (HsSig _ HsOuterExplicit{} _) -> Nothing

compactWords
  :: StrategyLayout
  -> Maybe (ToBriDocM BriDocNumbered)
  -> [ToBriDocM BriDocNumbered]
compactWords strategy viaDoc = case (strategy, viaDoc) of
  (NoStrategy, _) -> [docLitS "deriving"]
  (KeywordStrategy keyword, _) -> [docLitS "deriving", docLit keyword]
  (ViaStrategyType{}, Just renderedVia) ->
    [docLitS "deriving", docLitS "via", docForceSingleline renderedVia]
  (ViaStrategyType{}, Nothing) -> [docLitS "deriving", docLitS "via"]

multilineWords :: StrategyLayout -> [ToBriDocM BriDocNumbered]
multilineWords = \case
  NoStrategy -> [docLitS "deriving"]
  KeywordStrategy keyword -> [docLitS "deriving", docLit keyword]
  ViaStrategyType{} -> [docLitS "deriving via"]

spacedWords :: [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
spacedWords = docSeq . List.intersperse docSeparator

regularIndent
  :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
regularIndent = docEnsureIndent BrIndentRegular . docSetIndentLevel
