{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.DataDecl.Support
  ( documentedSingleH98Constructor
  , supportsCommentedDataDecl
  , supportsDocumentedSingleH98Comments
  ) where

import qualified Data.Maybe as Maybe
import GHC (GenLocated(L), Located)
import GHC.Hs
import GHC.Types.SrcLoc
  ( SrcSpan
  , getLoc
  , srcSpanEndCol
  , srcSpanEndLine
  , srcSpanStartCol
  , srcSpanStartLine
  )
import Language.Haskell.Brittany.Internal.ExactPrintCompat
  ( Comment(commentIdentifier)
  , srcSpanToRealSpan
  )
import Language.Haskell.Brittany.Internal.CommentPlan (lookupCommentPlacement)
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as ExactPrintCompat
import Language.Haskell.Brittany.Internal.Layouters.IE (toL)
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types

supportsCommentedDataDecl :: TyClDecl GhcPs -> Bool
supportsCommentedDataDecl = \case
  DataDecl _ _ _ _ HsDataDefn
    { dd_cType = Nothing
    , dd_kindSig = Nothing
    , dd_cons = constructors
    } -> case constructors of
      NewTypeCon _ -> False
      DataTypeCons _ dataConstructors -> case dataConstructors of
        _ : _ : _ ->
          all simpleH98 dataConstructors || all simpleGadt dataConstructors
        [constructor] -> simpleGadt constructor
        _ -> False
  _ -> False
 where
  simpleH98 = \case
    L _ (ConDeclH98 _ _ False [] context _ _) -> contextIsEmpty context
    _ -> False

  simpleGadt = \case
    L _ (ConDeclGADT _ (_ :| []) (L _ (HsOuterImplicit _)) [] Nothing
      (PrefixConGADT _ arguments) _ _) -> all simpleArgument arguments
    _ -> False

  simpleArgument = \case
    CDF _ _ _ (HsUnannotated _) _ Nothing -> True
    _ -> False

  contextIsEmpty Nothing = True
  contextIsEmpty (Just (L _ context)) = null context

documentedSingleH98Constructor
  :: TyClDecl GhcPs -> Maybe (LConDecl GhcPs, SrcSpan)
documentedSingleH98Constructor = \case
  DataDecl _ _ (HsQTvs _ binders) _ HsDataDefn
    { dd_ext = annotation
    , dd_ctxt = context
    , dd_cType = Nothing
    , dd_kindSig = Nothing
    , dd_cons = constructors
    } -> case constructors of
      NewTypeCon constructor
        | all simpleBinder binders
        , contextIsEmpty context
        , simpleH98 constructor ->
            Just (constructor, getEpTokenSrcSpan $ andd_equal annotation)
      DataTypeCons _ [constructor]
        | all simpleBinder binders
        , contextIsEmpty context
        , simpleH98 constructor ->
            Just (constructor, getEpTokenSrcSpan $ andd_equal annotation)
      _ -> Nothing
  _ -> Nothing
 where
  simpleH98 = \case
    L _ (ConDeclH98 _ _ False [] context _ _) -> contextIsEmpty context
    _ -> False

  simpleBinder = \case
    L _ (HsTvb _ _ (HsBndrVar _ _) (HsBndrNoKind _)) -> True
    _ -> False

  contextIsEmpty Nothing = True
  contextIsEmpty (Just (L _ context)) = null context

supportsDocumentedSingleH98Comments
  :: CommentPlan
  -> Located declaration
  -> SrcSpan
  -> Located (ConDecl GhcPs)
  -> [(Comment, ExactPrintCompat.DeltaPos)]
  -> Bool
supportsDocumentedSingleH98Comments commentPlan declaration equalsSpan constructor =
  all commentIsSupported
 where
  commentIsSupported (sourceComment, _)
    | isStructuralPlacement
        sourceComment
        (lookupCommentPlacement commentPlan sourceComment) = True
    | otherwise = case
        ( srcSpanToRealSpan $ getLoc declaration
        , srcSpanToRealSpan equalsSpan
        , srcSpanToRealSpan $ getLoc constructor
        , srcSpanToRealSpan $ commentIdentifier sourceComment
        ) of
          ( Just declarationSpan
            , Just equalSpan
            , Just constructorSpan
            , Just commentSpan
            ) ->
              let commentStart = spanStart commentSpan
                  commentEnd = spanEnd commentSpan
              in commentEnd <= spanStart declarationSpan
                || ( spanEnd equalSpan <= commentStart
                  && commentEnd <= spanStart constructorSpan
                   )
          _ -> False

  isStructuralPlacement sourceComment = \case
    Just CommentPlacement{placementRole = HaddockPostDoc{}} -> True
    Just CommentPlacement
      { placementOwner = NodeId ownerKey
      , placementRole = role
      } | role `elem` [LeadingOrdinary, Unattached] ->
          ownerIsInsideConstructor ownerKey
            && commentIsBetweenPrefixArguments sourceComment
    Just CommentPlacement
      { placementOwner = NodeId (ExactPrintCompat.AnnKey _ ownerName)
      , placementRole = BetweenChildren DerivingClause
      } -> ExactPrintCompat.unConName ownerName == "HsDerivingClause"
    Just CommentPlacement{placementRole = BetweenChildren TypeOperator} -> True
    Just CommentPlacement{placementRole = SectionComment} -> True
    _ -> False

  ownerIsInsideConstructor ownerKey = case
    ( ExactPrintCompat.annKeyRealSpan ownerKey
    , srcSpanToRealSpan $ getLoc constructor
    ) of
      (Just ownerSpan, Just constructorSpan) ->
        spanStart ownerSpan >= spanStart constructorSpan
          && spanEnd ownerSpan <= spanEnd constructorSpan
      _ -> False

  commentIsBetweenPrefixArguments sourceComment = case constructor of
    L _ (ConDeclH98 _ _ _ _ _ (PrefixCon arguments) _) -> case
      srcSpanToRealSpan $ commentIdentifier sourceComment of
      Just commentSpan ->
        let argumentSpans = Maybe.mapMaybe
              (srcSpanToRealSpan . getLoc . toL . cdf_type)
              arguments
        in any ((<= spanStart commentSpan) . spanEnd) argumentSpans
          && any ((spanEnd commentSpan <=) . spanStart) argumentSpans
      Nothing -> False
    _ -> False

  spanStart span' = (srcSpanStartLine span', srcSpanStartCol span')
  spanEnd span' = (srcSpanEndLine span', srcSpanEndCol span')
