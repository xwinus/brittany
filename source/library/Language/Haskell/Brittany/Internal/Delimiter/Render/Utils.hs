{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.Delimiter.Render.Utils
  ( RenderM
  , addBaseY
  , columnsNode
  , delimiterIndent
  , ensureIndent
  , forceSingleline
  , interleave
  , linesNode
  , openWithSpacing
  , parNode
  , rowColumns
  , separatorNode
  , sequenceNode
  , setBaseY
  , setIndentLevel
  , isSingletonList
  , splitLeadingComments
  , stripTrailingSpacing
  , token
  ) where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import Data.Kind (Type)
import Language.Haskell.Brittany.Internal.Delimiter.Types
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Types

type RenderM :: Type -> Type
type RenderM = State.State Int

interleave :: [document] -> [document] -> [document]
interleave [] _ = []
interleave (firstDocument : rest) separators = firstDocument
  : List.concat [ [separator, child] | (separator, child) <- zip separators rest ]

splitLeadingComments
  :: BriDocNumbered -> ([BriDocNumbered], BriDocNumbered)
splitLeadingComments document@(nodeId, value) = case value of
  BDFSeq children -> case List.span isOwnLineComment children of
    ([], _) -> ([], document)
    (comments, remaining) ->
      (comments, (nodeId, BDFSeq remaining))
  _ -> ([], document)
 where
  isOwnLineComment (_, BDFComment planned) =
    placementLineRelation (plannedCommentPlacement planned) == CommentOwnLine
  isOwnLineComment _ = False

stripTrailingSpacing :: BriDocNumbered -> BriDocNumbered
stripTrailingSpacing (nodeId, document) = (nodeId, case document of
  BDFSeq children -> BDFSeq $ stripLastDocument children
  BDFCols signature children -> BDFCols signature $ stripLastDocument children
  BDFAnnotationPrior mode key child ->
    BDFAnnotationPrior mode key $ stripTrailingSpacing child
  BDFAnnotationRest key child ->
    BDFAnnotationRest key $ stripTrailingSpacing child
  BDFAnnotationKW key keyword child ->
    BDFAnnotationKW key keyword $ stripTrailingSpacing child
  _ -> document)
 where
  stripLastDocument = reverse . stripReversed . reverse
  stripReversed [] = []
  stripReversed (emptyDocument@(_, BDFEmpty) : rest) =
    emptyDocument : stripReversed rest
  stripReversed ((_, BDFSeparator) : rest) = rest
  stripReversed (child : rest) = stripTrailingSpacing child : rest

fresh :: BriDocFInt -> RenderM BriDocNumbered
fresh document = do
  nodeId <- State.get
  State.put $ nodeId - 1
  pure (nodeId, document)

token :: Text -> RenderM BriDocNumbered
token = fresh . BDFLit

separatorNode :: RenderM BriDocNumbered
separatorNode = fresh BDFSeparator

openWithSpacing :: BriDocNumbered -> RenderM BriDocNumbered
openWithSpacing open = do
  afterOpen <- separatorNode
  sequenceNode [open, afterOpen]

sequenceNode :: [BriDocNumbered] -> RenderM BriDocNumbered
sequenceNode = fresh . BDFSeq

linesNode :: [BriDocNumbered] -> RenderM BriDocNumbered
linesNode = fresh . BDFLines

columnsNode :: ColSig -> [BriDocNumbered] -> RenderM BriDocNumbered
columnsNode signature = fresh . BDFCols signature

rowColumns :: DelimiterSequence document -> ColSig
rowColumns sequence' = case delimiterSequenceProfile sequence' of
  ListComprehensionDelimiter -> ColListComp
  TypeDelimiterSeparators -> ColTyOpPrefix
  RecordDelimiterFields -> ColRec
  _ -> case delimiterSequenceKind sequence' of
    SquareBracketsDelimiter -> ColList
    CurlyBracesDelimiter -> ColRec
    _ -> ColTuples

isSingletonList :: DelimiterSequence document -> Bool
isSingletonList sequence' = delimiterSequenceKind sequence'
  == SquareBracketsDelimiter
  && length (delimiterSequenceChildren sequence') == 1
  && delimiterSequenceProfile sequence' == LeadingDelimiterSeparators

forceSingleline :: BriDocNumbered -> RenderM BriDocNumbered
forceSingleline = fresh . BDFForceSingleline

setBaseY :: BriDocNumbered -> RenderM BriDocNumbered
setBaseY document = fresh . BDFBaseYPop =<< fresh (BDFBaseYPushCur document)

setIndentLevel :: BriDocNumbered -> RenderM BriDocNumbered
setIndentLevel document = fresh . BDFIndentLevelPop
  =<< fresh (BDFIndentLevelPushCur document)

addBaseY :: DelimiterIndent -> BriDocNumbered -> RenderM BriDocNumbered
addBaseY indent = fresh . BDFAddBaseY (toBrIndent indent)

parNode
  :: DelimiterIndent
  -> BriDocNumbered
  -> BriDocNumbered
  -> RenderM BriDocNumbered
parNode indent line indented = do
  indented' <- ensureIndent indent indented
  fresh $ BDFPar BrIndentNone line indented'

ensureIndent
  :: DelimiterIndent -> BriDocNumbered -> RenderM BriDocNumbered
ensureIndent indent = fresh . BDFEnsureIndent (toBrIndent indent)

toBrIndent :: DelimiterIndent -> BrIndent
toBrIndent = \case
  DelimiterIndentNone -> BrIndentNone
  DelimiterIndentRegular -> BrIndentRegular
  DelimiterIndentFixed amount -> BrIndentSpecial amount

delimiterIndent :: DelimiterSequence document -> DelimiterIndent
delimiterIndent = delimiterSequenceIndent
