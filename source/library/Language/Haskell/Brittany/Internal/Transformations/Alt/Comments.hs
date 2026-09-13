{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Transformations.Alt.Comments
  ( containsLineComment
  , containsExpressionBoundary
  , containsCommentLineBreak
  , sequenceRequiresCommentLineBreak
  ) where

import qualified Control.Monad.Trans.State.Strict as StateS
import qualified Data.IntSet as IntSet
import Language.Haskell.Brittany.Internal.Delimiter.Types
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.ExpressionBoundary
  ( interruptsExpression
  )
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Types

containsLineComment :: BriDocNumbered -> Bool
containsLineComment = containsComment $ \planned ->
  sourceCommentSyntax (plannedCommentSource planned) == LineComment

containsCommentLineBreak :: BriDocNumbered -> Bool
containsCommentLineBreak = containsComment $ \planned ->
  sourceCommentSyntax (plannedCommentSource planned) == LineComment
    || interruptsExpression planned

containsExpressionBoundary :: BriDocNumbered -> Bool
containsExpressionBoundary = containsComment interruptsExpression

containsComment :: (PlannedComment -> Bool) -> BriDocNumbered -> Bool
containsComment predicate document = StateS.evalState (visit document) IntSet.empty
 where
  visit (nodeId, node) = do
    visited <- StateS.get
    if IntSet.member nodeId visited
      then pure False
      else do
        StateS.put $ IntSet.insert nodeId visited
        case node of
          BDFComment planned -> pure $ predicate planned
          BDFSeq children -> anyM visit children
          BDFCols _ children -> anyM visit children
          BDFAddBaseY _ child -> visit child
          BDFBaseYPushCur child -> visit child
          BDFBaseYPop child -> visit child
          BDFIndentLevelPushCur child -> visit child
          BDFIndentLevelPop child -> visit child
          BDFPar _ line indented -> anyM visit [line, indented]
          BDFDelimited group -> anyM visit $ activeDelimitedDocuments group
          BDFAlt alternatives -> anyM visit alternatives
          BDFForwardLineMode child -> visit child
          BDFAnnotationPrior _ _ child -> visit child
          BDFAnnotationKW _ _ child -> visit child
          BDFAnnotationRest _ child -> visit child
          BDFMoveToKWDP _ _ _ child -> visit child
          BDFLines children -> anyM visit children
          BDFEnsureIndent _ child -> visit child
          BDFForceMultiline child -> visit child
          BDFForceSingleline child -> visit child
          BDFColumnsLimit _ child -> visit child
          BDFNonBottomSpacing _ child -> visit child
          BDFSetParSpacing child -> visit child
          BDFForceParSpacing child -> visit child
          BDFDebug _ child -> visit child
          _ -> pure False

sequenceRequiresCommentLineBreak :: Bool -> [BriDocNumbered] -> Bool
sequenceRequiresCommentLineBreak False _ = False
sequenceRequiresCommentLineBreak True documents = first
  $ foldr inspectDocument (False, False, False) documents
 where
  first (requiresBreak, _, _) = requiresBreak
  inspectDocument document (requiresBreak, hasLayoutToRight, hasCodeToRight) =
    ( requiresBreak
        || endsWithComment isInlineBoundary document && hasLayoutToRight
        || endsWithComment interruptsExpression document && hasCodeToRight
    , hasLayoutToRight || hasLayoutContent document
    , hasCodeToRight || hasCodeContent document
    )
  isInlineBoundary planned =
    sourceCommentSyntax (plannedCommentSource planned) == LineComment
      && (placementLineRelation (plannedCommentPlacement planned) == InlineComment
        || commentBoundaryGap (plannedCommentBoundary planned) == BeforeCloseBoundary)

endsWithComment :: (PlannedComment -> Bool) -> BriDocNumbered -> Bool
endsWithComment predicate (_, document) = case document of
  BDFComment planned -> predicate planned
  BDFSeq children -> maybe False (endsWithComment predicate)
    $ lastLayoutChild children
  BDFCols _ children -> maybe False (endsWithComment predicate)
    $ lastLayoutChild children
  BDFAddBaseY _ child -> endsWithComment predicate child
  BDFBaseYPushCur child -> endsWithComment predicate child
  BDFBaseYPop child -> endsWithComment predicate child
  BDFIndentLevelPushCur child -> endsWithComment predicate child
  BDFIndentLevelPop child -> endsWithComment predicate child
  BDFPar _ line indented -> endsWithComment predicate indented
    || not (hasLayoutContent indented) && endsWithComment predicate line
  BDFAlt alternatives -> any (endsWithComment predicate) alternatives
  BDFForwardLineMode child -> endsWithComment predicate child
  BDFAnnotationPrior _ _ child -> endsWithComment predicate child
  BDFAnnotationKW _ _ child -> endsWithComment predicate child
  BDFAnnotationRest _ child -> endsWithComment predicate child
  BDFMoveToKWDP _ _ _ child -> endsWithComment predicate child
  BDFLines children -> maybe False (endsWithComment predicate)
    $ lastLayoutChild children
  BDFEnsureIndent _ child -> endsWithComment predicate child
  BDFForceMultiline child -> endsWithComment predicate child
  BDFForceSingleline child -> endsWithComment predicate child
  BDFColumnsLimit _ child -> endsWithComment predicate child
  BDFNonBottomSpacing _ child -> endsWithComment predicate child
  BDFSetParSpacing child -> endsWithComment predicate child
  BDFForceParSpacing child -> endsWithComment predicate child
  BDFDebug _ child -> endsWithComment predicate child
  _ -> False

lastLayoutChild :: [BriDocNumbered] -> Maybe BriDocNumbered
lastLayoutChild = foldl' keepLast Nothing
 where
  keepLast previous document
    | hasLayoutContent document = Just document
    | otherwise = previous

hasLayoutContent :: BriDocNumbered -> Bool
hasLayoutContent = hasContent True

hasCodeContent :: BriDocNumbered -> Bool
hasCodeContent = hasContent False

hasContent :: Bool -> BriDocNumbered -> Bool
hasContent includeComments (_, document) = case document of
  BDFComment{} -> includeComments
  BDFEmpty -> False
  BDFSeparator -> False
  BDFSeq children -> any (hasContent includeComments) children
  BDFCols _ children -> any (hasContent includeComments) children
  BDFLines children -> any (hasContent includeComments) children
  BDFPar _ line indented -> any (hasContent includeComments) [line, indented]
  BDFAddBaseY _ child -> hasContent includeComments child
  BDFBaseYPushCur child -> hasContent includeComments child
  BDFBaseYPop child -> hasContent includeComments child
  BDFIndentLevelPushCur child -> hasContent includeComments child
  BDFIndentLevelPop child -> hasContent includeComments child
  BDFAlt alternatives -> any (hasContent includeComments) alternatives
  BDFForwardLineMode child -> hasContent includeComments child
  BDFAnnotationPrior _ _ child -> hasContent includeComments child
  BDFAnnotationKW _ _ child -> hasContent includeComments child
  BDFAnnotationRest _ child -> hasContent includeComments child
  BDFMoveToKWDP _ _ _ child -> hasContent includeComments child
  BDFEnsureIndent _ child -> hasContent includeComments child
  BDFForceMultiline child -> hasContent includeComments child
  BDFForceSingleline child -> hasContent includeComments child
  BDFColumnsLimit _ child -> hasContent includeComments child
  BDFNonBottomSpacing _ child -> hasContent includeComments child
  BDFSetParSpacing child -> hasContent includeComments child
  BDFForceParSpacing child -> hasContent includeComments child
  BDFDebug _ child -> hasContent includeComments child
  _ -> True
