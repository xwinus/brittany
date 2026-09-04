{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Delimiter.Comments
  ( extractBoundaryComments
  , isRecordEdgeBoundaryComment
  , rebaseInlineBoundaryComment
  , splitBoundaryComments
  ) where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as ExactPrintCompat
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Types

splitBoundaryComments
  :: CommentBoundaryGap
  -> BriDocNumbered
  -> ([BriDocNumbered], BriDocNumbered)
splitBoundaryComments gap document@(nodeId, value) = case value of
  BDFSeq children -> case List.span (isBoundaryComment gap) children of
    ([], _) -> ([], document)
    (comments, remaining) -> (comments, (nodeId, BDFSeq remaining))
  BDFAnnotationPrior mode key child ->
    let (comments, child') = splitBoundaryComments gap child
    in (comments, (nodeId, BDFAnnotationPrior mode key child'))
  BDFAnnotationRest key child ->
    let (comments, child') = splitBoundaryComments gap child
    in (comments, (nodeId, BDFAnnotationRest key child'))
  BDFAnnotationKW key keyword child ->
    let (comments, child') = splitBoundaryComments gap child
    in (comments, (nodeId, BDFAnnotationKW key keyword child'))
  _ -> ([], document)

extractBoundaryComments
  :: CommentBoundaryGap
  -> BriDocNumbered
  -> ([BriDocNumbered], BriDocNumbered)
extractBoundaryComments gap document@(nodeId, value) = case value of
  BDFComment planned
    | commentBoundaryGap (plannedCommentBoundary planned) == gap ->
        ([document], (nodeId, BDFEmpty))
  BDFSeq children -> childrenNode BDFSeq children
  BDFCols signature children -> childrenNode (BDFCols signature) children
  BDFAddBaseY indent child -> childNode (BDFAddBaseY indent) child
  BDFBaseYPushCur child -> childNode BDFBaseYPushCur child
  BDFBaseYPop child -> childNode BDFBaseYPop child
  BDFIndentLevelPushCur child -> childNode BDFIndentLevelPushCur child
  BDFIndentLevelPop child -> childNode BDFIndentLevelPop child
  BDFPar indent line indented ->
    let (lineComments, line') = extractBoundaryComments gap line
        (indentedComments, indented') = extractBoundaryComments gap indented
        comments = uniqueComments $ lineComments ++ indentedComments
    in if null comments
      then ([], document)
      else (comments, (nodeId, BDFPar indent line' indented'))
  BDFAlt alternatives -> childrenNode BDFAlt alternatives
  BDFForwardLineMode child -> childNode BDFForwardLineMode child
  BDFAnnotationPrior mode key child ->
    childNode (BDFAnnotationPrior mode key) child
  BDFAnnotationKW key keyword child ->
    childNode (BDFAnnotationKW key keyword) child
  BDFAnnotationRest key child -> childNode (BDFAnnotationRest key) child
  BDFMoveToKWDP key keyword restore child ->
    childNode (BDFMoveToKWDP key keyword restore) child
  BDFLines children -> childrenNode BDFLines children
  BDFEnsureIndent indent child -> childNode (BDFEnsureIndent indent) child
  BDFForceMultiline child -> childNode BDFForceMultiline child
  BDFForceSingleline child -> childNode BDFForceSingleline child
  BDFColumnsLimit limit child -> childNode (BDFColumnsLimit limit) child
  BDFNonBottomSpacing strict child ->
    childNode (BDFNonBottomSpacing strict) child
  BDFSetParSpacing child -> childNode BDFSetParSpacing child
  BDFForceParSpacing child -> childNode BDFForceParSpacing child
  BDFDebug label child -> childNode (BDFDebug label) child
  -- A nested group owns its own boundary comments.
  BDFDelimited{} -> ([], document)
  _ -> ([], document)
 where
  childNode constructor child =
    let (comments, child') = extractBoundaryComments gap child
    in if null comments
      then ([], document)
      else (comments, (nodeId, constructor child'))
  childrenNode constructor children =
    let extracted = extractBoundaryComments gap <$> children
        comments = uniqueComments $ List.concatMap fst extracted
    in if null comments
      then ([], document)
      else (comments, (nodeId, constructor $ snd <$> extracted))

isBoundaryComment :: CommentBoundaryGap -> BriDocNumbered -> Bool
isBoundaryComment gap (_, BDFComment planned) =
  commentBoundaryGap (plannedCommentBoundary planned) == gap
isBoundaryComment _ _ = False

uniqueComments :: [BriDocNumbered] -> [BriDocNumbered]
uniqueComments = List.nubBy sameComment
 where
  sameComment (_, BDFComment left) (_, BDFComment right) =
    sourceCommentKey (plannedCommentSource left)
      == sourceCommentKey (plannedCommentSource right)
  sameComment _ _ = False

rebaseInlineBoundaryComment :: BriDocNumbered -> BriDocNumbered
rebaseInlineBoundaryComment (nodeId, BDFComment planned) =
  ( nodeId
  , BDFComment planned { plannedCommentColumnDelta = 1 }
  )
rebaseInlineBoundaryComment document = document

isRecordEdgeBoundaryComment :: PlannedComment -> Bool
isRecordEdgeBoundaryComment planned = disabledRecordField || farFromOwner
 where
  source = plannedCommentSource planned
  disabledRecordField = Text.isPrefixOf (Text.pack "-- ,")
    $ Text.stripStart $ sourceCommentText source
  farFromOwner = case placementOwner $ plannedCommentPlacement planned of
    NodeId owner -> case ExactPrintCompat.annKeyRealSpan owner of
      Nothing -> False
      Just ownerSpan ->
        SrcLoc.srcSpanStartCol (sourceCommentSpan source)
          > SrcLoc.srcSpanEndCol ownerSpan
