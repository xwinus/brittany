{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.SourceComment.Indentation
  ( commentIndentPolicy
  , statementOwnerRelative
  , structuralOwnerRelative
  ) where

import qualified Data.Text as Text
import Language.Haskell.Brittany.Internal.ExactPrintCompat
  ( AnnKey(..)
  , unConName
  )
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types

commentIndentPolicy
  :: Int
  -> SourceComment
  -> CommentPlacement
  -> CommentBoundaryId
  -> CommentIndentPolicy
commentIndentPolicy lineDelta source placement boundary
  | Text.isPrefixOf (Text.singleton '#')
      (Text.stripStart $ sourceCommentText source) = SourceColumnIndent
  | placementLineRelation placement /= InlineComment
  , case placementRole placement of
      HaddockPostDoc{} -> True
      _ -> False = SourceColumnIndent
  | placementLineRelation placement == InlineComment = TokenRelativeIndent
  | lineDelta == 0, placementAnchor placement == AfterNode = TokenRelativeIndent
  | placementRole placement == BetweenChildren TypeOperator
  , ConstructorBoundaryPath{} <- commentBoundaryPath boundary = TokenRelativeIndent
  | placementRole placement == BetweenChildren DerivingClause
  , placementAnchor placement == BeforeNode = OwnerRelativeIndent
  | structuralOwnerRelative placement = OwnerRelativeIndent
  | statementOwnerRelative placement = OwnerRelativeIndent
  | DelimiterBoundaryPath{} <- commentBoundaryPath boundary
  , commentBoundaryGap boundary `elem`
      [AfterOpenBoundary, WithinBoundary, BetweenBoundary, BeforeCloseBoundary] =
      ContainerRelativeIndent
  | expressionBoundaryRelative placement boundary = RenderedAnchorIndent
  | otherwise = SourceColumnIndent

statementOwnerRelative :: CommentPlacement -> Bool
statementOwnerRelative placement = placementLineRelation placement
    == CommentOwnLine
  && placementAnchor placement == BeforeNode
  && case placementOwner placement of
    NodeId (AnnKey _ constructor) -> unConName constructor
      `elem` ["BodyStmt", "BindStmt", "LastStmt", "LetStmt"]

structuralOwnerRelative :: CommentPlacement -> Bool
structuralOwnerRelative placement = placementLineRelation placement
    == CommentOwnLine
  && placementAnchor placement == BeforeNode
  && placementRole placement `elem` [LeadingDoc, LeadingOrdinary]
  && case placementOwner placement of
    NodeId (AnnKey _ constructor) -> unConName constructor
      `elem` ["ConDeclH98", "ConDeclGADT", "VarPat"]

expressionBoundaryRelative :: CommentPlacement -> CommentBoundaryId -> Bool
expressionBoundaryRelative placement boundary =
  placementLineRelation placement == CommentOwnLine
    && placementAnchor placement == BeforeNode
    && placementRole placement == LeadingOrdinary
    && case (commentBoundaryPath boundary, commentBoundaryGap boundary) of
      (ExpressionBoundaryPath{}, WithinBoundary) -> True
      _ -> False
