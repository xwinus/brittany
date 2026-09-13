{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.SourceComment.ExpressionBoundary
  ( interruptsExpression
  , retainExpressionCommentBase
  , finishExpressionComment
  ) where

import qualified Data.Generics.Uniplate.Direct as Uniplate
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as ExactPrintCompat
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Types

-- A standalone source-column comment separates an expression from its follower.
-- Inline-seeded comment runs retain their rendered anchor in the backend.
interruptsExpression :: PlannedComment -> Bool
interruptsExpression planned =
  placementLineRelation placement == CommentOwnLine
    && placementAnchor placement == AfterNode
    && expressionOwner (placementOwner placement)
    && plannedCommentIndentPolicy planned == SourceColumnIndent
    && plannedCommentLineDelta planned > 0
    && commentBoundaryGap (plannedCommentBoundary planned) == WithinBoundary
 where
  placement = plannedCommentPlacement planned

-- Source-column placement is also used by types and patterns. Their existing
-- continuation rules must remain independent of expression boundaries.
expressionOwner :: NodeId -> Bool
expressionOwner (NodeId (ExactPrintCompat.AnnKey _ constructor)) =
  ExactPrintCompat.unConName constructor `elem`
    [ "HsVar", "HsApp", "HsAppType", "OpApp", "NegApp", "HsPar"
    , "HsLit", "HsOverLit", "HsRecFld", "HsGetField", "HsProjection"
    , "ExprWithTySig", "SectionL", "SectionR", "ExplicitList"
    , "RecordCon", "RecordUpd", "HsLam", "HsCase", "HsIf", "HsMultiIf"
    , "HsLet", "HsDo", "ExplicitTuple", "ExplicitSum"
    ]

-- Carry the continuation base to expression comments without shifting code
-- already rendered in an earlier column or paragraph.
retainExpressionCommentBase :: BrIndent -> BriDoc -> BriDoc
retainExpressionCommentBase indent document = case document of
  BDComment planned | interruptsExpression planned -> BDAddBaseY indent document
  BDDelimited{} -> document
  _ -> Uniplate.descend (retainExpressionCommentBase indent) document

finishExpressionComment :: Int -> LayoutState -> LayoutState
finishExpressionComment column state = state
  { _lstate_curYOrAddNewline = Right $ case _lstate_curYOrAddNewline state of
      Left{} -> 1
      Right pending -> max 1 pending
  , _lstate_addSepSpace = Just column
  , _lstate_commentCol = Nothing
  }
