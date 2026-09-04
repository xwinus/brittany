{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Transformations.Alt.Comments
  ( containsLineComment
  , sequenceRequiresCommentLineBreak
  ) where

import qualified Control.Monad.Trans.State.Strict as StateS
import qualified Data.IntSet as IntSet
import Language.Haskell.Brittany.Internal.Delimiter.Types
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Types

containsLineComment :: BriDocNumbered -> Bool
containsLineComment document = StateS.evalState (visit document) IntSet.empty
 where
  visit (nodeId, node) = do
    visited <- StateS.get
    if IntSet.member nodeId visited
      then pure False
      else do
        StateS.put $ IntSet.insert nodeId visited
        case node of
          BDFComment planned -> pure
            $ sourceCommentSyntax (plannedCommentSource planned) == LineComment
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
sequenceRequiresCommentLineBreak True documents = fst
  $ foldr inspectDocument (False, False) documents
 where
  inspectDocument document (requiresBreak, hasContentToRight) =
    ( requiresBreak
        || endsWithLineComment document && hasContentToRight
    , hasContentToRight || hasLayoutContent document
    )

endsWithLineComment :: BriDocNumbered -> Bool
endsWithLineComment (_, document) = case document of
  BDFComment planned -> sourceCommentSyntax (plannedCommentSource planned)
    == LineComment
    && ( placementLineRelation (plannedCommentPlacement planned) == InlineComment
      || commentBoundaryGap (plannedCommentBoundary planned) == BeforeCloseBoundary
      )
  BDFSeq children -> maybe False endsWithLineComment
    $ lastLayoutChild children
  BDFCols _ children -> maybe False endsWithLineComment
    $ lastLayoutChild children
  BDFAddBaseY _ child -> endsWithLineComment child
  BDFBaseYPushCur child -> endsWithLineComment child
  BDFBaseYPop child -> endsWithLineComment child
  BDFIndentLevelPushCur child -> endsWithLineComment child
  BDFIndentLevelPop child -> endsWithLineComment child
  BDFPar _ line indented -> endsWithLineComment indented
    || not (hasLayoutContent indented) && endsWithLineComment line
  BDFAlt alternatives -> any endsWithLineComment alternatives
  BDFForwardLineMode child -> endsWithLineComment child
  BDFAnnotationPrior _ _ child -> endsWithLineComment child
  BDFAnnotationKW _ _ child -> endsWithLineComment child
  BDFAnnotationRest _ child -> endsWithLineComment child
  BDFMoveToKWDP _ _ _ child -> endsWithLineComment child
  BDFLines children -> maybe False endsWithLineComment
    $ lastLayoutChild children
  BDFEnsureIndent _ child -> endsWithLineComment child
  BDFForceMultiline child -> endsWithLineComment child
  BDFForceSingleline child -> endsWithLineComment child
  BDFColumnsLimit _ child -> endsWithLineComment child
  BDFNonBottomSpacing _ child -> endsWithLineComment child
  BDFSetParSpacing child -> endsWithLineComment child
  BDFForceParSpacing child -> endsWithLineComment child
  BDFDebug _ child -> endsWithLineComment child
  _ -> False

lastLayoutChild :: [BriDocNumbered] -> Maybe BriDocNumbered
lastLayoutChild = foldl' keepLast Nothing
 where
  keepLast previous document
    | hasLayoutContent document = Just document
    | otherwise = previous

hasLayoutContent :: BriDocNumbered -> Bool
hasLayoutContent (_, document) = case document of
  BDFEmpty -> False
  BDFSeparator -> False
  BDFSeq children -> any hasLayoutContent children
  BDFCols _ children -> any hasLayoutContent children
  _ -> True
