{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Instance
  ( layoutInstanceHead
  ) where

import qualified Data.Text as Text
import GHC (GenLocated(L), unLoc)
import GHC.Hs
import qualified GHC.OldList as List
import GHC.Types.Basic (OverlapMode(..))
import GHC.Types.SourceText (SourceText(..))
import Language.Haskell.Brittany.Internal.Fallbacks (FallbackId(..))
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.IE (toL)
import Language.Haskell.Brittany.Internal.Layouters.Type
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.TypeFallbacks (exactSourceTypes)
import Language.Haskell.Brittany.Internal.Types

layoutInstanceHead :: ToBriDoc ClsInstDecl
layoutInstanceHead lcid@(L _ cid) = case cid_poly_ty cid of
  L _ (HsSig _ binders body)
    | supportsNativeHead cid -> layoutNativeHead binders body
  _ -> layoutFallback
 where
  layoutNativeHead binders body = do
    bodyDoc <- docSharedWrapper layoutType (toL body)
    binderDocs <- case binders of
      HsOuterExplicit _ variables -> Just <$> layoutTyVarBndrs variables
      HsOuterImplicit{} -> pure Nothing
      XHsOuterTyVarBndrs{} -> pure Nothing
    let pragmaDocs = maybe [] (pure . docLit . Text.pack)
          (overlapPragma =<< cid_overlap_mode cid)
        keywordDocs = docLitS "instance" : pragmaDocs
        forallDocs = case binderDocs of
          Nothing -> []
          Just variables ->
            [docSeq $ docLitS "forall" : processTyVarBndrsSingleline variables]
        compactPrefix = docSeq $ fmap appSep $ keywordDocs ++ forallDocs
        compactSignature = case binderDocs of
          Nothing -> bodyDoc
          Just _ -> docSeq [docLitS ". ", bodyDoc]
        multilineForallDocs = case forallDocs of
          [] -> []
          [forallDoc] -> [docSeq [forallDoc, docLitS "."]]
          _ -> forallDocs
        multilinePrefix = docSeq $ List.intersperse docSeparator
          $ keywordDocs ++ multilineForallDocs
        multilineSignature = case binderDocs of
          Nothing -> bodyDoc
          Just _ -> bodyDoc
    docAlt
      [ docSeq
        [ compactPrefix
        , docForceSingleline compactSignature
        , docSeparator
        , docLitS "where"
        ]
      , docLines
        [ multilinePrefix
        , docEnsureIndent BrIndentRegular
          $ docSetIndentLevel multilineSignature
        , docEnsureIndent BrIndentRegular $ docLitS "where"
        ]
      ]

  layoutFallback =
    briDocByExactNoComment TypeClassDeclarationFallback
      $ InstD NoExtField
      . ClsInstD NoExtField
      . removeChildren
      <$> lcid

supportsNativeHead :: ClsInstDecl GhcPs -> Bool
supportsNativeHead cid = null (exactSourceTypes $ cid_poly_ty cid)
  && case unLoc <$> cid_overlap_mode cid of
    Just NonCanonical{} -> False
    _ -> True

overlapPragma :: GenLocated l OverlapMode -> Maybe String
overlapPragma = \case
  L _ (NoOverlap NoSourceText) -> Nothing
  L _ NoOverlap{} -> Just "{-# NO_OVERLAP #-}"
  L _ Overlappable{} -> Just "{-# OVERLAPPABLE #-}"
  L _ Overlapping{} -> Just "{-# OVERLAPPING #-}"
  L _ Overlaps{} -> Just "{-# OVERLAPS #-}"
  L _ Incoherent{} -> Just "{-# INCOHERENT #-}"
  L _ NonCanonical{} -> Nothing

removeChildren :: ClsInstDecl GhcPs -> ClsInstDecl GhcPs
removeChildren cid = cid
  { cid_binds = []
  , cid_sigs = []
  , cid_tyfam_insts = []
  , cid_datafam_insts = []
  }
