{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Delimiter.Render
  ( renderLayout
  ) where

import qualified Data.List as List
import qualified Data.Text as Text
import Language.Haskell.Brittany.Internal.Delimiter.Comments
import Language.Haskell.Brittany.Internal.Delimiter.Render.Utils
import Language.Haskell.Brittany.Internal.Delimiter.Types
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types
  ( CommentBoundaryGap (AfterOpenBoundary, BeforeCloseBoundary)
  , PlannedComment (plannedCommentSource)
  , SourceComment (sourceCommentKey)
  )
import Language.Haskell.Brittany.Internal.Types

renderLayout
  :: DelimiterLayout
  -> DelimiterSequence BriDocNumbered
  -> RenderM BriDocNumbered
renderLayout layout sequence' = case layout of
  DelimiterCompact -> renderCompact sequence'
  DelimiterAttached -> case delimiterSequenceProfile sequence' of
    ImportExportDelimiter -> renderImportExport sequence'
    TightImportExportDelimiter -> renderImportExport sequence'
    ModuleExportDelimiter -> renderImportExport sequence'
    TypeDelimiterSeparators -> renderTypeDelimiters sequence'
    LeadingDelimiterSeparators -> renderLeading sequence'
    NestedIEDelimiter -> renderLeading sequence'
    PatternInlineDelimiter -> renderLeading sequence'
    PromotedListDelimiter -> renderPromotedList sequence'
    ListComprehensionDelimiter -> renderListComprehension sequence'
    RecordDelimiterFields -> renderRecordRows False sequence'
    TrailingDelimiterSeparators -> renderTrailing sequence'
    BlockDelimiterChild -> renderBlock sequence'
    PatternBlockDelimiterChild -> renderPatternBlock sequence'
    TypeBlockDelimiterChild -> renderTypeBlock sequence'
  DelimiterHanging -> case delimiterSequenceProfile sequence' of
    TypeDelimiterSeparators -> renderTypeDelimiters sequence'
    RecordDelimiterFields -> renderRecordRows True sequence'
    _ -> renderHanging sequence'
  DelimiterVertical -> renderVertical sequence'

renderCompact
  :: DelimiterSequence BriDocNumbered -> RenderM BriDocNumbered
renderCompact sequence' = do
  open <- token $ delimiterSequenceOpenToken sequence'
  close <- token $ delimiterSequenceCloseToken sequence'
  let structuralChildren = delimiterSequenceChildren sequence'
      structuralSeparators = delimiterSequenceSeparators sequence'
      prepareChild child following = forceSingleline
        $ case delimiterSeparatorKind <$> following of
            Just RepeatedDelimiterSeparator ->
              stripTrailingSpacing $ delimiterChildDocument child
            _ -> delimiterChildDocument child
  children <- sequence
    [ prepareChild child following
    | (child, following) <- zip structuralChildren
        (fmap Just structuralSeparators ++ [Nothing])
    ]
  separators <- traverse
    (if delimiterSequenceProfile sequence' == PromotedListDelimiter
      then spacedCompactSeparator
      else compactSeparator)
    structuralSeparators
  case children of
    [] | delimiterSequenceProfile sequence' == ImportExportDelimiter -> do
      between <- separatorNode
      sequenceNode [open, between, close]
    [] -> sequenceNode [open, close]
    _ -> do
      let body = interleave children separators
      case delimiterSequenceProfile sequence' of
        TrailingDelimiterSeparators -> do
          afterOpen <- separatorNode
          beforeClose <- separatorNode
          sequenceNode $ [open, afterOpen] ++ body ++ [beforeClose, close]
        ImportExportDelimiter -> do
          afterOpen <- separatorNode
          beforeClose <- separatorNode
          sequenceNode $ [open, afterOpen] ++ body ++ [beforeClose, close]
        TightImportExportDelimiter -> sequenceNode $ [open] ++ body ++ [close]
        ListComprehensionDelimiter -> do
          afterOpen <- separatorNode
          beforeClose <- separatorNode
          sequenceNode $ [open, afterOpen] ++ body ++ [beforeClose, close]
        _ | delimiterSequenceKind sequence' == UnboxedParenthesesDelimiter -> do
          afterOpen <- separatorNode
          beforeClose <- separatorNode
          sequenceNode $ [open, afterOpen] ++ body ++ [beforeClose, close]
        _ | delimiterSequenceKind sequence' == CurlyBracesDelimiter -> do
          afterOpen <- separatorNode
          beforeClose <- separatorNode
          sequenceNode $ [open, afterOpen] ++ body ++ [beforeClose, close]
        _
          | delimiterSequenceProfile sequence' == PatternInlineDelimiter ->
              renderCompactPattern open close children separators
          | delimiterSequenceKind sequence' == ParenthesesDelimiter
          , delimiterSequenceProfile sequence' == LeadingDelimiterSeparators
          , length children > 1 -> renderCompactTuple open close children separators
          | otherwise -> sequenceNode $ [open] ++ body ++ [close]

renderCompactTuple
  :: BriDocNumbered
  -> BriDocNumbered
  -> [BriDocNumbered]
  -> [BriDocNumbered]
  -> RenderM BriDocNumbered
renderCompactTuple open close (firstChild : remainingChildren) separators = do
  firstColumn <- sequenceNode [open, firstChild]
  remainingColumns <- traverse (uncurry sequenceNode2)
    $ zip separators remainingChildren
  case List.reverse remainingColumns of
    [] -> sequenceNode [firstColumn, close]
    lastColumn : reversedMiddle -> do
      finalColumn <- sequenceNode [lastColumn, close]
      columnsNode ColTuple
        $ firstColumn : List.reverse reversedMiddle ++ [finalColumn]
 where
  sequenceNode2 left right = sequenceNode [left, right]
renderCompactTuple open close [] _ = sequenceNode [open, close]

renderCompactPattern
  :: BriDocNumbered
  -> BriDocNumbered
  -> [BriDocNumbered]
  -> [BriDocNumbered]
  -> RenderM BriDocNumbered
renderCompactPattern open close children separators = case children of
  [] -> columnsNode ColPatterns [open, close]
  firstChild : remainingChildren -> do
    remaining <- traverse (uncurry sequenceNode2)
      $ zip separators remainingChildren
    columnsNode ColPatterns $ open : firstChild : remaining ++ [close]
 where
  sequenceNode2 left right = sequenceNode [left, right]

renderLeading
  :: DelimiterSequence BriDocNumbered -> RenderM BriDocNumbered
renderLeading sequence' = case delimiterSequenceChildren sequence' of
  [] -> renderCompact sequence'
  firstChild : remainingChildren -> do
    open <- token $ delimiterSequenceOpenToken sequence'
    close <- token $ delimiterSequenceCloseToken sequence'
    spacedOpen <- openWithSpacing open
    firstDocument <- if isSingletonList sequence'
      then setBaseY $ delimiterChildDocument firstChild
      else pure $ delimiterChildDocument firstChild
    firstLine <- columnsNode (rowColumns sequence')
      [spacedOpen, firstDocument]
    rows <- traverse (renderLeadingRow $ rowColumns sequence')
      $ zip (delimiterSequenceSeparators sequence') remainingChildren
    body <- linesNode $ firstLine : List.concat rows ++ [close]
    setBaseY body

renderLeadingRow
  :: ColSig
  -> (DelimiterSeparator, DelimiterChild BriDocNumbered)
  -> RenderM [BriDocNumbered]
renderLeadingRow columnStyle (separator, child) = do
  separatorDocument <- leadingSeparator separator
  let (comments, content) = splitLeadingComments
        $ delimiterChildDocument child
  contentLine <- columnsNode columnStyle [separatorDocument, content]
  pure $ comments ++ [contentLine]

renderPromotedList
  :: DelimiterSequence BriDocNumbered -> RenderM BriDocNumbered
renderPromotedList sequence' = case delimiterSequenceChildren sequence' of
  [] -> renderCompact sequence'
  firstChild : remainingChildren -> do
    open <- token $ delimiterSequenceOpenToken sequence'
    close <- token $ delimiterSequenceCloseToken sequence'
    closePadding <- token $ Text.pack " "
    closeLine <- sequenceNode [closePadding, close]
    spacedOpen <- openWithSpacing open
    firstLine <- columnsNode ColList
      [spacedOpen, delimiterChildDocument firstChild]
    rows <- traverse renderPromotedRow
      $ zip (delimiterSequenceSeparators sequence') remainingChildren
    setBaseY =<< linesNode (firstLine : List.concat rows ++ [closeLine])

renderPromotedRow
  :: (DelimiterSeparator, DelimiterChild BriDocNumbered)
  -> RenderM [BriDocNumbered]
renderPromotedRow (separator, child) = do
  before <- token $ Text.pack " "
  value <- token $ delimiterSeparatorToken separator
  after <- separatorNode
  prefix <- sequenceNode [before, value, after]
  let (comments, content) = splitLeadingComments
        $ delimiterChildDocument child
  contentLine <- columnsNode ColList [prefix, content]
  pure $ comments ++ [contentLine]

renderListComprehension
  :: DelimiterSequence BriDocNumbered -> RenderM BriDocNumbered
renderListComprehension sequence' = case delimiterSequenceChildren sequence' of
  [] -> renderCompact sequence'
  firstChild : remainingChildren -> do
    open <- token $ delimiterSequenceOpenToken sequence'
    close <- token $ delimiterSequenceCloseToken sequence'
    afterOpen <- separatorNode
    let (boundaryComments, remainingChildren') = mapAccumL
          extractAfterOpenComments [] remainingChildren
        firstDocument = delimiterChildDocument firstChild
    openingLines <- case boundaryComments of
      [] -> do
        spacedOpen <- sequenceNode [open, afterOpen]
        firstLine <- columnsNode ColListComp [spacedOpen, firstDocument]
        pure [firstLine]
      firstComment : otherComments -> do
        openingLine <- sequenceNode [open, rebaseInlineBoundaryComment firstComment]
        comments <- traverse (ensureIndent $ delimiterIndent sequence')
          otherComments
        resultLine <- ensureIndent (delimiterIndent sequence') firstDocument
        pure $ openingLine : comments ++ [resultLine]
    rows <- traverse (renderLeadingRow ColListComp)
      $ zip (delimiterSequenceSeparators sequence') remainingChildren'
    setBaseY =<< linesNode (openingLines ++ List.concat rows ++ [close])
 where
  extractAfterOpenComments comments child =
    let (found, document) = splitBoundaryComments
          AfterOpenBoundary
          (delimiterChildDocument child)
    in (comments ++ found, child { delimiterChildDocument = document })

renderImportExport
  :: DelimiterSequence BriDocNumbered -> RenderM BriDocNumbered
renderImportExport sequence' = do
  open <- token $ delimiterSequenceOpenToken sequence'
  close <- token $ delimiterSequenceCloseToken sequence'
  afterOpen <- separatorNode
  case delimiterSequenceChildren sequence' of
    [] -> do
      firstLine <- sequenceNode [open, afterOpen]
      parNode (delimiterIndent sequence') firstLine close
    firstChild : remainingChildren -> do
      firstLine <- setBaseY =<< sequenceNode
        [open, afterOpen, delimiterChildDocument firstChild]
      rows <- traverse (renderLeadingRow $ rowColumns sequence')
        $ zip (delimiterSequenceSeparators sequence') remainingChildren
      indented <- linesNode $ List.concat rows ++ [close]
      parNode (delimiterIndent sequence') firstLine indented

renderTypeDelimiters
  :: DelimiterSequence BriDocNumbered -> RenderM BriDocNumbered
renderTypeDelimiters sequence' = case delimiterSequenceChildren sequence' of
  [] -> renderCompact sequence'
  firstChild : remainingChildren -> do
    open <- token $ delimiterSequenceOpenToken sequence'
    close <- token $ delimiterSequenceCloseToken sequence'
    spacedOpen <- openWithSpacing open
    firstLine <- columnsNode ColTyOpPrefix
      [spacedOpen, delimiterChildDocument firstChild]
    firstLine' <- addBaseY (delimiterIndent sequence') firstLine
    rows <- traverse (renderLeadingRow ColTyOpPrefix)
      $ zip (delimiterSequenceSeparators sequence') remainingChildren
    indentedRows <- traverse (addBaseY $ delimiterIndent sequence')
      $ List.concat rows
    body <- linesNode $ indentedRows ++ [close]
    parNode DelimiterIndentNone firstLine' body

renderRecordRows
  :: Bool -> DelimiterSequence BriDocNumbered -> RenderM BriDocNumbered
renderRecordRows forceChildren sequence' = case delimiterSequenceChildren sequence' of
  [] -> renderCompact sequence'
  originalChildren -> do
    open <- token $ delimiterSequenceOpenToken sequence'
    close <- token $ delimiterSequenceCloseToken sequence'
    spacedOpen <- openWithSpacing open
    let extracted = extractBeforeClose <$> originalChildren
        (firstComments, firstChild) : remainingChildren = extracted
    firstDocument <- forceIfRequested $ delimiterChildDocument firstChild
    let firstRow = prependRecordColumn spacedOpen firstDocument
    firstBoundaryRows <- traverse renderRecordBoundaryComment firstComments
    remainingRows <- traverse renderChildAndBoundary
      $ zip (delimiterSequenceSeparators sequence') remainingChildren
    setBaseY =<< linesNode
      (firstRow : firstBoundaryRows ++ List.concat remainingRows ++ [close])
 where
  extractBeforeClose child =
    let (comments, document) = extractBoundaryComments
          BeforeCloseBoundary
          (delimiterChildDocument child)
    in ( List.nubBy samePlannedComment comments
       , child { delimiterChildDocument = document }
       )
  renderChildAndBoundary (separator, (comments, child)) = do
    row <- renderRecordRow forceChildren (separator, child)
    boundaryRows <- traverse renderRecordBoundaryComment comments
    pure $ row : boundaryRows
  forceIfRequested document
    | forceChildren = forceSingleline document
    | otherwise = pure document

renderRecordBoundaryComment
  :: BriDocNumbered -> RenderM BriDocNumbered
renderRecordBoundaryComment document@(_, BDFComment planned)
  | isRecordEdgeBoundaryComment planned =
      addBaseY (DelimiterIndentFixed (-2)) document
renderRecordBoundaryComment document = pure document

samePlannedComment :: BriDocNumbered -> BriDocNumbered -> Bool
samePlannedComment (_, BDFComment left) (_, BDFComment right) =
  sourceCommentKey (plannedCommentSource left)
    == sourceCommentKey (plannedCommentSource right)
samePlannedComment _ _ = False

renderRecordRow
  :: Bool
  -> (DelimiterSeparator, DelimiterChild BriDocNumbered)
  -> RenderM BriDocNumbered
renderRecordRow forceChild (separator, child) = do
  separatorDocument <- leadingSeparator separator
  childDocument <- if forceChild
    then forceSingleline $ delimiterChildDocument child
    else pure $ delimiterChildDocument child
  let (comments, content) = splitLeadingComments
        childDocument
      row = prependRecordColumn separatorDocument content
  case comments of
    [] -> pure row
    _ -> linesNode $ comments ++ [row]

prependRecordColumn
  :: BriDocNumbered -> BriDocNumbered -> BriDocNumbered
prependRecordColumn prefix (nodeId, document) = case document of
  BDFAnnotationPrior mode key child ->
    (nodeId, BDFAnnotationPrior mode key $ prependRecordColumn prefix child)
  BDFAnnotationRest key child ->
    (nodeId, BDFAnnotationRest key $ prependRecordColumn prefix child)
  BDFAnnotationKW key keyword child ->
    (nodeId, BDFAnnotationKW key keyword $ prependRecordColumn prefix child)
  BDFCols ColRec columns -> (nodeId, BDFCols ColRec $ prefix : columns)
  _ -> (nodeId, BDFCols ColRec [prefix, (nodeId, document)])

renderTrailing
  :: DelimiterSequence BriDocNumbered -> RenderM BriDocNumbered
renderTrailing sequence' = case delimiterSequenceChildren sequence' of
  [] -> renderCompact sequence'
  children -> do
    open <- token $ delimiterSequenceOpenToken sequence'
    close <- token $ delimiterSequenceCloseToken sequence'
    rows <- traverse renderTrailingRow
      $ zip children $ fmap Just (delimiterSequenceSeparators sequence')
        ++ [Nothing]
    case rows of
      [] -> sequenceNode [open, close]
      [onlyRow] -> do
        afterOpen <- separatorNode
        beforeClose <- separatorNode
        sequenceNode [open, afterOpen, onlyRow, beforeClose, close]
      firstRow : remainingRows -> case List.reverse remainingRows of
        [] -> sequenceNode [open, firstRow, close]
        lastRow : reversedMiddle -> do
          afterOpen <- separatorNode
          firstLine <- sequenceNode [open, afterOpen, firstRow]
          indentedMiddle <- traverse (ensureIndent $ delimiterIndent sequence')
            $ List.reverse reversedMiddle
          beforeClose <- separatorNode
          lastLine <- ensureIndent (delimiterIndent sequence')
            =<< sequenceNode [lastRow, beforeClose, close]
          setBaseY =<< linesNode
            (firstLine : indentedMiddle ++ [lastLine])

renderTrailingRow
  :: (DelimiterChild BriDocNumbered, Maybe DelimiterSeparator)
  -> RenderM BriDocNumbered
renderTrailingRow (child, Nothing) = pure $ delimiterChildDocument child
renderTrailingRow (child, Just separator) = do
  separatorDocument <- trailingSeparator separator
  sequenceNode [delimiterChildDocument child, separatorDocument]

renderBlock
  :: DelimiterSequence BriDocNumbered -> RenderM BriDocNumbered
renderBlock sequence' = case delimiterSequenceChildren sequence' of
  [child] -> do
    open <- token $ delimiterSequenceOpenToken sequence'
    close <- token $ delimiterSequenceCloseToken sequence'
    case delimiterIndent sequence' of
      DelimiterIndentFixed 2 -> do
        child' <- addBaseY (delimiterIndent sequence')
          $ delimiterChildDocument child
        firstLine <- columnsNode ColOpPrefix [open, child']
        setBaseY =<< linesNode [firstLine, close]
      _ -> do
        indented <- setIndentLevel $ delimiterChildDocument child
        line <- sequenceNode [open, indented]
        close' <- case delimiterIndent sequence' of
          DelimiterIndentFixed amount -> ensureIndent
            (DelimiterIndentFixed amount) close
          _ -> pure close
        parNode DelimiterIndentNone line close'
  _ -> renderLeading sequence'

renderTypeBlock
  :: DelimiterSequence BriDocNumbered -> RenderM BriDocNumbered
renderTypeBlock sequence' = case delimiterSequenceChildren sequence' of
  [child] -> do
    open <- token $ delimiterSequenceOpenToken sequence'
    close <- token $ delimiterSequenceCloseToken sequence'
    spacedOpen <- openWithSpacing open
    child' <- addBaseY (delimiterIndent sequence')
      $ delimiterChildDocument child
    line <- columnsNode ColTyOpPrefix [spacedOpen, child']
    parNode DelimiterIndentNone line close
  _ -> renderLeading sequence'

renderPatternBlock
  :: DelimiterSequence BriDocNumbered -> RenderM BriDocNumbered
renderPatternBlock sequence' = case delimiterSequenceChildren sequence' of
  [child] -> do
    open <- token $ delimiterSequenceOpenToken sequence'
    close <- token $ delimiterSequenceCloseToken sequence'
    child' <- setIndentLevel $ delimiterChildDocument child
    sequenceNode [open, child', close]
  _ -> renderLeading sequence'

renderHanging
  :: DelimiterSequence BriDocNumbered -> RenderM BriDocNumbered
renderHanging sequence' = case delimiterSequenceChildren sequence' of
  [] -> renderCompact sequence'
  firstChild : remainingChildren -> do
    open <- token $ delimiterSequenceOpenToken sequence'
    close <- token $ delimiterSequenceCloseToken sequence'
    afterOpen <- separatorNode
    line <- sequenceNode
      [open, afterOpen, delimiterChildDocument firstChild]
    rows <- traverse (renderLeadingRow $ rowColumns sequence')
      $ zip (delimiterSequenceSeparators sequence') remainingChildren
    indented <- linesNode $ List.concat rows ++ [close]
    parNode (delimiterIndent sequence') line indented

renderVertical
  :: DelimiterSequence BriDocNumbered -> RenderM BriDocNumbered
renderVertical sequence' = do
  open <- token $ delimiterSequenceOpenToken sequence'
  close <- token $ delimiterSequenceCloseToken sequence'
  rows <- case delimiterSequenceChildren sequence' of
    [] -> pure []
    firstChild : remainingChildren -> do
      firstDocument <- ensureIndent (delimiterIndent sequence')
        $ delimiterChildDocument firstChild
      rest <- traverse (renderLeadingRow $ rowColumns sequence')
        $ zip (delimiterSequenceSeparators sequence') remainingChildren
      traverse (ensureIndent $ delimiterIndent sequence')
        $ firstDocument : List.concat rest
  linesNode $ open : rows ++ [close]

compactSeparator :: DelimiterSeparator -> RenderM BriDocNumbered
compactSeparator separator = case delimiterSeparatorKind separator of
  ListComprehensionBar -> do
    before <- separatorNode
    value <- token $ delimiterSeparatorToken separator
    after <- separatorNode
    sequenceNode [before, value, after]
  RepeatedDelimiterSeparator -> do
    value <- token $ delimiterSeparatorToken separator
    after <- separatorNode
    sequenceNode [value, after]
  RangeDelimiterOperator -> token $ delimiterSeparatorToken separator

spacedCompactSeparator :: DelimiterSeparator -> RenderM BriDocNumbered
spacedCompactSeparator separator = do
  before <- separatorNode
  value <- compactSeparator separator
  sequenceNode [before, value]

leadingSeparator :: DelimiterSeparator -> RenderM BriDocNumbered
leadingSeparator separator = do
  value <- token $ delimiterSeparatorToken separator
  after <- separatorNode
  sequenceNode [value, after]

trailingSeparator :: DelimiterSeparator -> RenderM BriDocNumbered
trailingSeparator separator = do
  value <- token $ delimiterSeparatorToken separator
  after <- separatorNode
  sequenceNode [value, after]
