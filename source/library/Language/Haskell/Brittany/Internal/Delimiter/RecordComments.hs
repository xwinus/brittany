{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Delimiter.RecordComments
  ( extractRecordBoundaryComments
  , hasStandaloneRecordComments
  , renderRecordBoundaryComment
  ) where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.CommentBoundary.Trailing
  ( plainLineCommentText )
import Language.Haskell.Brittany.Internal.Delimiter.Comments
import Language.Haskell.Brittany.Internal.Delimiter.Types (DelimiterIndent(..))
import Language.Haskell.Brittany.Internal.Delimiter.Render.Utils
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Continuation
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Types

extractRecordBoundaryComments
  :: [BriDocNumbered] -> [([BriDocNumbered], BriDocNumbered)]
extractRecordBoundaryComments documents = extractChild <$> childComments
 where
  childComments =
    [ (document, orderedComments
        [ planned
        | (_, BDFComment planned) <- fst $ extractCommentsMatching (const True) document
        ])
    | document <- documents
    ]
  comments = orderedComments $ List.concatMap snd childComments
  orderedComments = List.sortOn commentPosition . Map.elems . Map.fromList
    . map (\planned -> (commentKey planned, planned))
  commentPosition planned =
    let span' = sourceCommentSpan $ plannedCommentSource planned
    in (SrcLoc.srcSpanStartLine span', SrcLoc.srcSpanStartCol span')
  commentKey = sourceCommentKey . plannedCommentSource
  protected = Set.union (continuationKeys comments) (sensitiveGroupKeys comments)
  beforeClose planned =
    commentBoundaryGap (plannedCommentBoundary planned) == BeforeCloseBoundary
  extractable planned = beforeClose planned
    || (standaloneRecordComment planned && Set.notMember (commentKey planned) protected)
  extractChild (document, plannedComments) =
    let
      -- Moving a comment out of a field must not cross a later retained comment.
      suffixKeys = Set.fromList $ commentKey
        <$> takeWhile extractable (reverse plannedComments)
      selected planned = beforeClose planned
        || Set.member (commentKey planned) suffixKeys
    in extractCommentsMatching selected document

hasStandaloneRecordComments :: [BriDocNumbered] -> Bool
hasStandaloneRecordComments = any (any isStandalone . fst)
  . extractRecordBoundaryComments
 where
  isStandalone (_, BDFComment planned) = standaloneRecordComment planned
  isStandalone _ = False

standaloneRecordComment :: PlannedComment -> Bool
standaloneRecordComment planned =
  commentBoundaryGap (plannedCommentBoundary planned) == BetweenBoundary
    && plannedCommentIndentPolicy planned == ContainerRelativeIndent
    && placementLineRelation placement == CommentOwnLine
    && placementAnchor placement == AfterNode
    && placementRole placement == Unattached
    && ordinaryText (plannedCommentSource planned)
 where
  placement = plannedCommentPlacement planned

continuationKeys :: [PlannedComment] -> Set.Set SourceCommentKey
continuationKeys = fst . foldl' step (Set.empty, Nothing)
 where
  step (keys, previous) planned = case previous >>= continueTrailingCommentRun planned of
    Just continued ->
      (Set.insert (sourceCommentKey $ plannedCommentSource planned) keys, Just continued)
    Nothing -> (keys, startTrailingCommentRun planned 0)

-- A prose line adjacent to a source-sensitive example belongs to that example.
sensitiveGroupKeys :: [PlannedComment] -> Set.Set SourceCommentKey
sensitiveGroupKeys = Set.fromList . List.concatMap sensitiveKeys . adjacentGroups
 where
  sensitiveKeys comments
    | all (ordinaryText . plannedCommentSource) comments = []
    | otherwise = sourceCommentKey . plannedCommentSource <$> comments
  adjacentGroups [] = []
  adjacentGroups (first : rest) = collect [first] first rest
  collect accumulated _ [] = [reverse accumulated]
  collect accumulated previous remaining@(current : rest)
    | adjacent previous current = collect (current : accumulated) current rest
    | otherwise = reverse accumulated : adjacentGroups remaining
  adjacent previous current =
    let previousSpan = sourceCommentSpan $ plannedCommentSource previous
        currentSpan = sourceCommentSpan $ plannedCommentSource current
    in SrcLoc.srcSpanFile previousSpan == SrcLoc.srcSpanFile currentSpan
      && SrcLoc.srcSpanEndLine previousSpan + 1 == SrcLoc.srcSpanStartLine currentSpan

ordinaryText :: SourceComment -> Bool
ordinaryText source = supportedSyntax
  && plainLineCommentText ("--" ++ Text.unpack (Text.drop 2 text))
  && SrcLoc.srcSpanStartLine span' == SrcLoc.srcSpanEndLine span'
  && case content of
      [] -> True
      first : _ -> Char.isAlphaNum first || first == '('
 where
  supportedSyntax = case sourceCommentSyntax source of
    LineComment -> True
    BlockComment -> Text.isPrefixOf (Text.pack "{-") text
      && Text.isSuffixOf (Text.pack "-}") text
  span' = sourceCommentSpan source
  text = Text.stripStart $ sourceCommentText source
  content = case Text.unpack $ Text.drop 2 text of
    ' ' : rest -> rest
    rest -> rest

renderRecordBoundaryComment
  :: BriDocNumbered -> RenderM BriDocNumbered
renderRecordBoundaryComment (nodeId, BDFComment planned)
  | standaloneRecordComment planned = do
      emptyName <- emptyNode
      emptyValue <- emptyNode
      -- Keep the structural comment outside indentation floated into the value
      -- column, while preserving alignment across the surrounding field rows.
      columnsNode ColRec
        [ (nodeId, BDFComment planned { plannedCommentColumnDelta = 0 })
        , emptyName
        , emptyValue
        ]
renderRecordBoundaryComment document@(_, BDFComment planned)
  | isRecordEdgeBoundaryComment planned =
      addBaseY (DelimiterIndentFixed (-2)) document
renderRecordBoundaryComment document = pure document

emptyNode :: RenderM BriDocNumbered
emptyNode = do
  nodeId <- State.get
  State.put $ nodeId - 1
  pure (nodeId, BDFEmpty)
