{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Expr.TypeAnnotation
  ( layoutExpressionTypeAnnotation
  , layoutExpressionSignature
  ) where

import qualified Data.List as List
import GHC (GhcPs, unLoc)
import GHC.Hs
import GHC.Types.Var (Specificity(..))
import Language.Haskell.Brittany.Internal.Layouters.IE (toL)
import Language.Haskell.Brittany.Internal.Layouters.Type (layoutType)
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types

layoutExpressionTypeAnnotation
  :: ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
layoutExpressionTypeAnnotation expression typeDocument = do
  let signature = docSeq [appSep $ docLitS "::", typeDocument]
  docAlt
    [ docForceSingleline $ docSeq [appSep expression, signature]
    -- A child break cannot rescue an overflowing expression/:: boundary.
    , docAddBaseY BrIndentRegular $ docPar expression signature
    ]

layoutExpressionSignature :: LHsSigType GhcPs -> ToBriDocM BriDocNumbered
layoutExpressionSignature signature = docWrapNode (toL signature) $ case unLoc signature of
  HsSig _ binders body -> do
    bodyDoc <- layoutType $ toL body
    case binders of
      HsOuterImplicit _ -> pure bodyDoc
      HsOuterExplicit _ explicitBinders -> do
        binderDocs <- mapM layoutSignatureBinder explicitBinders
        let compactPrefix = docSeq
              $ [docLitS "forall"]
              ++ List.concatMap (\binder -> [docSeparator, pure binder]) binderDocs
              ++ [docLitS "."]
        prefix <- docAlt
          [ docForceSingleline compactPrefix
          , docParIndented BrIndentRegular (docLitS "forall")
            $ docLines $ map pure binderDocs ++ [docLitS "."]
          ]
        docAlt
          [ docSeq [appSep $ pure prefix, pure bodyDoc]
          , docParIndented BrIndentRegular (pure prefix) $ pure bodyDoc
          ]
  _ -> unknownNodeError "expression signature" $ toL signature

layoutSignatureBinder :: LHsTyVarBndr Specificity GhcPs -> ToBriDocM BriDocNumbered
layoutSignatureBinder binder = docWrapNode (toL binder) $ case unLoc binder of
  HsTvb _ specificity (HsBndrVar _ name) kind -> do
    let nameDoc = docLit $ lrdrNameToText $ toL name
    content <- case kind of
      HsBndrNoKind _ -> nameDoc
      HsBndrKind _ kindType -> do
        kindDoc <- layoutType $ toL kindType
        docSeq [appSep nameDoc, appSep $ docLitS "::", pure kindDoc]
    let delimiters = case (specificity, kind) of
          (InferredSpec, _) -> Just ("{", "}")
          (_, HsBndrKind{}) -> Just ("(", ")")
          _ -> Nothing
    case delimiters of
      Nothing -> pure content
      Just (opening, closing) ->
        docSeq [docLitS opening, pure content, docLitS closing]
  _ -> unknownNodeError "expression signature binder" $ toL binder
