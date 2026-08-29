{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.DataDecl.Deriving
  ( createDerivingPar
  ) where

import qualified Data.Map                                as Map
import           GHC                                      ( GenLocated(L)
                                                          , Located
                                                          , getLoc
                                                          , unLoc
                                                          )
import           GHC.Hs
import qualified GHC.OldList                             as List
import qualified GHC.Types.SrcLoc                        as SrcLoc
import           Language.Haskell.Brittany.Internal.ExactPrintCompat
                                                          ( realSpanToSrcSpan
                                                          , srcSpanToRealSpan
                                                          )
import           Language.Haskell.Brittany.Internal.ExactSource
                                                          ( sourceCommentFragment
                                                          )
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Layouters.IE
                                                          ( toL )
import           Language.Haskell.Brittany.Internal.Layouters.Type
                                                          ( layoutType )
import           Language.Haskell.Brittany.Internal.Prelude
import           Language.Haskell.Brittany.Internal.SourceComment.Types
import           Language.Haskell.Brittany.Internal.Types

createDerivingPar
  :: HsDeriving GhcPs -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
createDerivingPar derivs mainDoc = case derivs of
  [] -> mainDoc
  clauses ->
    docPar mainDoc
      $   docEnsureIndent BrIndentRegular
      $   docLines
      $   derivingClauseDoc
      <$> clauses

derivingClauseDoc :: LHsDerivingClause GhcPs -> ToBriDocM BriDocNumbered
derivingClauseDoc clause@(L _ (HsDerivingClause _ext mStrategy lTys)) = do
  let types = case lTys of
        L _ (DctSingle _ type'   ) -> [type']
        L _ (DctMulti  _ typeList) -> typeList
      typeCount          = length types
      whenMultiple value = if typeCount > 1 then docLitS value else docLitS ""
      (leftStrategy, rightStrategy) =
        maybe (docEmpty, docEmpty) strategyDocuments $ fmap toL mStrategy
  clauseComments <- derivingClauseComments $ toL clause
  let firstTypeStart = case types of
        [] -> Nothing
        firstType : _ ->
          fmap sourcePositionStart $ srcSpanToRealSpan $ getLoc $ toL firstType
      (leadingComments, trailingComments) = case firstTypeStart of
        Nothing        -> (clauseComments, [])
        Just typeStart -> List.partition
          ((<= typeStart) . sourcePositionEnd . sourceCommentSpan)
          clauseComments
      typeDocument = docSeq
        [ whenMultiple "("
        , docSeq $ List.intersperse docCommaSep $ layoutDerivingType <$> types
        , whenMultiple ")"
        ]
  if null types
    then docSeq []
    else if null clauseComments
      then docWrapNodePrior (toL clause) $ docSeq
        [ docDeriving
        , docWrapNodePrior (toL lTys) leftStrategy
        , docSeparator
        , whenMultiple "("
        , docWrapNodeRest (toL lTys)
        $   docSeq
        $   List.intersperse docCommaSep
        $   layoutDerivingType
        <$> types
        , whenMultiple ")"
        , rightStrategy
        ]
      else
        docWrapNodePrior (toL clause)
        $  docLines
        $  [docSeq [docDeriving, leftStrategy]]
        ++ (layoutDerivingComment <$> leadingComments ++ trailingComments)
        ++ [typeDocument, rightStrategy]

strategyDocuments
  :: Located (DerivStrategy GhcPs)
  -> (ToBriDocM BriDocNumbered, ToBriDocM BriDocNumbered)
strategyDocuments = \case
  L _ (StockStrategy    _) -> (docLitS " stock", docEmpty)
  L _ (AnyclassStrategy _) -> (docLitS " anyclass", docEmpty)
  L _ (NewtypeStrategy  _) -> (docLitS " newtype", docEmpty)
  via@(L _ (ViaStrategy (XViaStrategyPs _ viaType))) ->
    ( docEmpty
    , docSeq
      [ docWrapNode (toL via) $ docLitS " via"
      , docSeparator
      , layoutSignatureType viaType
      ]
    )

layoutSignatureType :: LHsSigType GhcPs -> ToBriDocM BriDocNumbered
layoutSignatureType (L _ (HsSig _ _ body)) = layoutType $ toL body

layoutDerivingType :: LHsSigType GhcPs -> ToBriDocM BriDocNumbered
layoutDerivingType type' = case unLoc type' of
  HsSig _ _ body -> layoutType $ toL body

derivingClauseComments
  :: Located (HsDerivingClause GhcPs) -> ToBriDocM [SourceComment]
derivingClauseComments clause = do
  commentPlan <- mAsk
  pure $ case srcSpanToRealSpan $ getLoc clause of
    Nothing         -> []
    Just clauseSpan -> List.sortOn
      (sourcePositionStart . sourceCommentSpan)
      [ sourceComment
      | (key, placement) <- Map.toList $ commentPlanPlacements commentPlan
      , placementRole placement == BetweenChildren DerivingClause
      , Just sourceComment <- [Map.lookup key $ commentPlanSources commentPlan]
      , sourcePositionStart (sourceCommentSpan sourceComment)
        >= sourcePositionStart clauseSpan
      , sourcePositionEnd (sourceCommentSpan sourceComment)
        <= sourcePositionEnd clauseSpan
      ]

layoutDerivingComment :: SourceComment -> ToBriDocM BriDocNumbered
layoutDerivingComment sourceComment = briDocBySourceFragmentNoComment
  (L (realSpanToSrcSpan $ sourceCommentSpan sourceComment) sourceComment)
  (sourceCommentFragment sourceComment)

sourcePositionStart :: SrcLoc.RealSrcSpan -> (Int, Int)
sourcePositionStart span' =
  (SrcLoc.srcSpanStartLine span', SrcLoc.srcSpanStartCol span')

sourcePositionEnd :: SrcLoc.RealSrcSpan -> (Int, Int)
sourcePositionEnd span' =
  (SrcLoc.srcSpanEndLine span', SrcLoc.srcSpanEndCol span')

docDeriving :: ToBriDocM BriDocNumbered
docDeriving = docLitS "deriving"
