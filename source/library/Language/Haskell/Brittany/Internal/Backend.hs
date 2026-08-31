{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Brittany.Internal.Backend where

import qualified Control.Monad.Trans.State.Strict as StateS
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.IntMap.Lazy as IntMapL
import qualified Data.IntMap.Strict as IntMapS
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified GHC.OldList as List
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.Alignment
import Language.Haskell.Brittany.Internal.BackendUtils
import Language.Haskell.Brittany.Internal.ColumnAlignment
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Delimiter
import Language.Haskell.Brittany.Internal.ExactSource
  ( validateExactSourceFragment
  )
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Utils
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as ExactPrintCompat



type ColIndex = Int

data ColumnSpacing
  = ColumnSpacingLeaf Int
  | ColumnSpacingRef Int Int

type ColumnBlock a = [a]
type ColumnBlocks a = Seq [a]
type ColMap1
  = IntMapL.IntMap {- ColIndex -}
                   (Bool, ColumnBlocks ColumnSpacing)
type ColMap2
  = IntMapL.IntMap {- ColIndex -}
                   (Float, ColumnBlock Int, ColumnBlocks Int)
                                          -- (ratio of hasSpace, maximum, raw)

data ColInfo
  = ColInfoStart -- start value to begin the mapAccumL.
  | ColInfoNo BriDoc
  | ColInfo ColIndex ColSig [(Int, ColInfo)]

instance Show ColInfo where
  show ColInfoStart = "ColInfoStart"
  show (ColInfoNo bd) =
    "ColInfoNo " ++ show (take 30 (show (briDocToDoc bd)) ++ "..")
  show (ColInfo ind sig list) =
    "ColInfo " ++ show ind ++ " " ++ show sig ++ " " ++ show list

data ColBuildState = ColBuildState
  { _cbs_map :: ColMap1
  , _cbs_index :: ColIndex
  }

type LayoutConstraints m
  = ( MonadMultiReader Config m
    , MonadMultiReader ExactPrintCompat.Anns m
    , MonadMultiReader CommentPlan m
    , MonadMultiWriter Text.Builder.Builder m
    , MonadMultiWriter [BrittanyError] m
    , MonadMultiWriter (Seq String) m
    , MonadMultiState LayoutState m
    )

layoutBriDocM :: forall m . LayoutConstraints m => BriDoc -> m ()
layoutBriDocM = \case
  BDEmpty -> do
    return () -- can it be that simple
  BDBlankLine -> layoutWriteBlankLine
  BDLit t -> do
    layoutIndentRestorePostComment
    layoutRemoveIndentLevelLinger
    layoutWriteAppend t
  BDSeq list -> do
    list `forM_` layoutBriDocM
  -- in this situation, there is nothing to do about cols.
  -- i think this one does not happen anymore with the current simplifications.
  -- BDCols cSig list | BDPar sameLine lines <- List.last list ->
  --   alignColsPar $ BDCols cSig (List.init list ++ [sameLine]) : lines
  BDCols _ list -> do
    list `forM_` layoutBriDocM
  BDSeparator -> do
    layoutAddSepSpace
  BDAddBaseY indent bd -> do
    let
      indentF = case indent of
        BrIndentNone -> id
        BrIndentRegular -> layoutWithAddBaseCol
        BrIndentSpecial i -> layoutWithAddBaseColN i
    indentF $ layoutBriDocM bd
  BDBaseYPushCur bd -> do
    layoutBaseYPushCur
    layoutBriDocM bd
  BDBaseYPop bd -> do
    layoutBriDocM bd
    layoutBaseYPop
  BDIndentLevelPushCur bd -> do
    layoutIndentLevelPushCur
    layoutBriDocM bd
  BDIndentLevelPop bd -> do
    layoutBriDocM bd
    layoutIndentLevelPop
  BDEnsureIndent indent bd -> do
    let
      indentF = case indent of
        BrIndentNone -> id
        BrIndentRegular -> layoutWithAddBaseCol
        BrIndentSpecial i -> layoutWithAddBaseColN i
    indentF $ do
      layoutWriteEnsureBlock
      layoutBriDocM bd
  BDPar indent sameLine indented -> do
    layoutBriDocM sameLine
    let
      indentF = case indent of
        BrIndentNone -> id
        BrIndentRegular -> layoutWithAddBaseCol
        BrIndentSpecial i -> layoutWithAddBaseColN i
    indentF $ do
      layoutWriteNewlineBlock
      layoutBriDocM indented
  BDDelimited group -> case prepareSelectedDelimiter group of
    Left invariantError ->
      mTell [ErrorDelimiterInvariant $ show invariantError]
    Right (_, document) -> layoutBriDocM document
  BDLines lines -> alignColsLines lines
  BDAlt [] -> error "empty BDAlt"
  BDAlt (alt : _) -> layoutBriDocM alt
  BDForceMultiline bd -> layoutBriDocM bd
  BDForceSingleline bd -> layoutBriDocM bd
  BDColumnsLimit _ bd -> layoutBriDocM bd
  BDForwardLineMode bd -> layoutBriDocM bd
  BDExternal annKey shouldAddComment externalSource -> do
    let t = externalSourceText externalSource
    let
      tlines = Text.lines $ t <> Text.pack "\n"
    anns :: ExactPrintCompat.Anns <- mAsk
    when shouldAddComment $ do
      layoutWriteAppend
        $ Text.pack
        $ "{-"
        ++ show (annKey, Map.lookup annKey anns)
        ++ "-}"
    case (externalSource, tlines) of
      (SourceFragment fragment, firstLine : remainingLines) -> do
        forM_ (fragmentAbsoluteColumn fragment) $ \column ->
          layoutMoveToAbsoluteCommentPos 0 column (length tlines)
        initialState <- mGet
        let fragmentColumn = case _lstate_curYOrAddNewline initialState of
              Left column -> column
                + fromMaybe 0 (_lstate_addSepSpace initialState)
              Right{} -> fromMaybe 0 (_lstate_addSepSpace initialState)
        layoutWriteAppend firstLine
        remainingLines `forM_` \line -> do
          layoutWriteNewline
          unless (Text.null line) $ do
            when (fragmentRebaseContinuation fragment)
              $ layoutWriteAppendSpaces fragmentColumn
            layoutWriteAppend line
        when (sourceFragmentRequiresLineBoundary fragment)
          layoutFinishPriorCommentLine
      _ -> do
        let tlineCount = length tlines
        zip [1 ..] tlines `forM_` \(i, line) -> do
          layoutWriteAppend line
          unless (i == tlineCount) layoutWriteNewlineBlock
    do
      state <- mGet
      remainingComments <- case externalSource of
        ExactPrintSource subKeys _ -> pure
          $ Map.withoutKeys (_lstate_comments state) subKeys
        SourceFragment fragment -> case
          consumeExactSourceFragment fragment (_lstate_comments state) of
          Left fragmentError -> do
            mTell [ErrorUnusedComment fragmentError]
            pure $ _lstate_comments state
          Right comments -> pure comments
      mSet $ state { _lstate_comments = remainingComments }
  BDPlain _ t -> do
    layoutIndentRestorePostComment
    layoutRemoveIndentLevelLinger
    layoutWriteAppend t
  BDComment planned -> renderPlannedComment planned
  BDAnnotationPrior _ annKey bd -> do
    layoutRemoveIndentLevelLinger
    state <- mGet
    case _lstate_curYOrAddNewline state of
      Left{} -> pure ()
      Right{} -> moveToExactAnn annKey
    layoutBriDocM bd
  BDAnnotationKW _ _ bd -> layoutBriDocM bd
  BDAnnotationRest annKey bd -> do
    layoutBriDocM bd
    state <- mGet
    let annMay = Map.lookup annKey $ _lstate_comments state
    let
      semiCount = length
        [ ()
        | Just ann <- [annMay]
        , (ExactPrintCompat.AnnSemiSep, _) <- ExactPrintCompat.annsDP ann
        ]
    shouldAddSemicolonNewlines <-
      mAsk
      <&> _conf_layout
      .> _lconfig_experimentalSemicolonNewlines
      .> confUnpack
    when shouldAddSemicolonNewlines $
      [1 .. semiCount] `forM_` const layoutWriteNewline
  BDMoveToKWDP annKey keyword shouldRestoreIndent bd -> do
    mDP <- do
      state <- mGet
      let m = _lstate_comments state
      let mAnn = ExactPrintCompat.annsDP <$> Map.lookup annKey m
      let
        relevant =
          [ dp
          | Just ann <- [mAnn]
          , (ExactPrintCompat.G kw1, dp) <- ann
          , keyword == kw1
          ]
      -- mTell $ Seq.fromList [show keyword, "KWDP: " ++ show annKey ++ " " ++ show mAnn, show relevant]
      case relevant of
        [] -> pure Nothing
        (ExactPrintCompat.DP (y, x) : _) -> do
          mSet state { _lstate_commentNewlines = 0 }
          pure $ Just (y - _lstate_commentNewlines state, x)
    case mDP of
      Nothing -> pure ()
      Just (y, x) ->
        -- we abuse this, as we probably will print the KW next, which is
        -- _not_ a comment..
        layoutMoveToCommentPos y (if shouldRestoreIndent then x else 0) 1
    layoutBriDocM bd
  BDNonBottomSpacing _ bd -> layoutBriDocM bd
  BDSetParSpacing bd -> layoutBriDocM bd
  BDForceParSpacing bd -> layoutBriDocM bd
  BDDebug s bd -> do
    mTell $ Text.Builder.fromText $ Text.pack $ "{-" ++ s ++ "-}"
    layoutBriDocM bd

renderPlannedComment :: LayoutConstraints m => PlannedComment -> m ()
renderPlannedComment planned = do
  state <- mGet
  commentPlan :: CommentPlan <- mAsk
  let source = plannedCommentSource planned
      key = sourceCommentKey source
      commentText = sourceCommentText source
      commentLines = Text.lines commentText
      lineCount = max 1 $ length commentLines
      placement = plannedCommentPlacement planned
      ownLine = placementLineRelation placement == CommentOwnLine
        && (plannedCommentLineDelta planned > 0
          || plannedCommentIndentPolicy planned /= TokenRelativeIndent
        )
      sourceColumnAtNestedBase = ownLine
        && placementAnchor placement == BeforeNode
        && plannedCommentIndentPolicy planned == SourceColumnIndent
        && plannedCommentColumnDelta planned < lstate_baseY state
        && _lstate_indLevelLinger state == lstate_baseY state
      lineDelta
        | ownLine, placementAnchor placement == BeforeNode = case
            _lstate_curYOrAddNewline state of
              Left{} -> max 1 $ plannedCommentLineDelta planned
              Right pending -> max pending $ plannedCommentLineDelta planned
        | ownLine = max 1 $ plannedCommentLineDelta planned
        | otherwise = 0
  if Set.member key $ _lstate_emittedComments state
    then mTell
      [ ErrorCommentPlan
          $ "Duplicate planned comment emission at "
          ++ show (plannedCommentBoundary planned)
          ++ ": "
          ++ show key
      ]
    else do
      indentAmount <-
        mAsk
        <&> _conf_layout
        .> _lconfig_indentAmount
        .> confUnpack
      mSet state
        { _lstate_comments = removePlannedComment key
            $ _lstate_comments state
        , _lstate_emittedComments = Set.insert key
            $ _lstate_emittedComments state
        }
      when (not ownLine && placementRole placement == Unattached) $ do
        layoutSetCommentCol
        mModify $ \current -> current
          { _lstate_commentCol = (+ max 1 (plannedCommentColumnDelta planned))
              <$> _lstate_commentCol current
          }
      when (not ownLine && placementRole placement == SectionComment) $
        layoutSetCommentCol
      case plannedCommentIndentPolicy planned of
        OwnerRelativeIndent
          | ownLine, placementAnchor placement == BeforeNode ->
              layoutMoveToCommentPos lineDelta
                (max 0
                  $ lstate_baseY state - _lstate_indLevelLinger state
                  + plannedCommentColumnDelta planned
                )
                lineCount
          | ownLine -> layoutMoveToAbsoluteCommentPos
              lineDelta (lstate_baseY state) lineCount
          | otherwise -> layoutMoveToCommentPos 0 1 lineCount
        RenderedAnchorIndent -> do
          layoutSetCommentCol
          current <- mGet
          let anchorColumn = fromMaybe
                (lstate_baseY current)
                (_lstate_commentCol current)
          layoutMoveToAbsoluteCommentPos lineDelta
            (anchorColumn + max 0 (plannedCommentColumnDelta planned))
            lineCount
        ContainerRelativeIndent ->
          let containerColumn = lstate_baseY state
              contentColumn = containerColumn + indentAmount
              commentColumn
                | placementRole placement == SectionComment
                    || isExportPlacement placement = contentColumn
                | commentBoundaryGap (plannedCommentBoundary planned)
                    == BeforeCloseBoundary = containerColumn
                | otherwise = max containerColumn
                    $ min contentColumn
                    $ plannedCommentColumnDelta planned
          in layoutMoveToAbsoluteCommentPos
            lineDelta commentColumn lineCount
        TokenRelativeIndent
          | ownLine -> layoutMoveToAbsoluteCommentPos lineDelta
              (fromMaybe
                ( lstate_baseY state
                  + if placementRole placement == BetweenChildren TypeOperator
                    then 3
                    else indentAmount
                )
                (_lstate_commentCol state)
              )
              lineCount
          | Left{} <- _lstate_curYOrAddNewline state -> layoutWriteAppendSpaces
              $ max 0
              $ max 1 (plannedCommentColumnDelta planned)
              - if placementRole placement == SectionComment
                then fromMaybe 0 (_lstate_addSepSpace state)
                else 0
          | otherwise -> layoutMoveToCommentPos 0
              (max 0
                $ 1 - _lstate_indLevelLinger state
              )
              lineCount
        SourceColumnIndent
          | placementRole placement == HaddockPostDoc RecordField ->
              layoutMoveToAbsoluteCommentPos
                lineDelta (lstate_baseY state + indentAmount) lineCount
          | placementRole placement == HaddockPostDoc SignatureArgument ->
              layoutMoveToAbsoluteCommentPos
                lineDelta (lstate_baseY state) lineCount
          | placementRole placement == HaddockPostDoc SignatureResult ->
              layoutMoveToAbsoluteCommentPos lineDelta indentAmount lineCount
          | sourceColumnAtNestedBase ->
              layoutMoveToAbsoluteCommentPos
                lineDelta (lstate_baseY state) lineCount
          | otherwise -> layoutMoveToCommentPos lineDelta
              (plannedCommentColumnDelta planned - _lstate_indLevelLinger state)
              lineCount
      layoutWriteAppendMultiline commentLines
      when (sourceCommentSyntax source == LineComment) $ case
          (placementAnchor placement, placementOwner placement) of
            _ | placementLineRelation placement == InlineComment ->
              layoutFinishPriorCommentLine
            (BeforeNode, NodeId owner)
              | isLastCommentBeforeOwner commentPlan planned -> do
                  layoutFinishPriorCommentBoundary
                    $ sourceCommentBoundary owner (sourceCommentSpan source)
                  when
                    ( plannedCommentIndentPolicy planned == SourceColumnIndent
                    && commentBoundaryGap (plannedCommentBoundary planned)
                      == WithinBoundary
                    ) $ case ExactPrintCompat.annKeyRealSpan owner of
                      Nothing -> pure ()
                      Just ownerSpan -> mModify $ \current -> current
                        { _lstate_addSepSpace = Just
                            $ max
                              (if sourceColumnAtNestedBase
                                then lstate_baseY state
                                else 0
                              )
                            $ SrcLoc.srcSpanStartCol ownerSpan - 1
                        }
                  when
                    (plannedCommentIndentPolicy planned == ContainerRelativeIndent) $
                    let containerColumn = lstate_baseY state
                        contentColumn = containerColumn + indentAmount
                        commentColumn = max containerColumn
                          $ min contentColumn
                          $ plannedCommentColumnDelta planned
                    in mModify $ \current -> current
                      { _lstate_addSepSpace = Just
                          $ max indentAmount commentColumn
                      }
            (AfterNode, NodeId owner)
              | commentBoundaryGap (plannedCommentBoundary planned)
                  == WithinBoundary
              , plannedCommentIndentPolicy planned == SourceColumnIndent
              , plannedCommentLineDelta planned > 0
              , Maybe.isJust $ ExactPrintCompat.annKeyRealSpan owner -> pure ()
            _ -> layoutFinishPriorCommentLine

isExportPlacement :: CommentPlacement -> Bool
isExportPlacement placement = case placementOwner placement of
  NodeId (ExactPrintCompat.AnnKey _ constructor) ->
    "IE" `List.isPrefixOf` ExactPrintCompat.unConName constructor

isLastCommentBeforeOwner :: CommentPlan -> PlannedComment -> Bool
isLastCommentBeforeOwner plan planned = case placementOwner placement of
  NodeId owner -> case ExactPrintCompat.annKeyRealSpan owner of
    Just ownerSpan -> not $ any (isBetween currentSpan ownerSpan)
      $ Map.elems
      $ commentPlanSources plan
    Nothing -> noLaterSibling
 where
  placement = plannedCommentPlacement planned
  currentSpan = sourceCommentSpan $ plannedCommentSource planned
  noLaterSibling = not $ any isLaterSibling
    $ Map.elems
    $ commentPlanPlacements plan
  isLaterSibling other = placementOwner other == placementOwner placement
    && placementAnchor other == BeforeNode
    && placementRelativeOrder other > placementRelativeOrder placement
  isBetween current owner other = sourceCommentKey other
    /= sourceCommentKey (plannedCommentSource planned)
    && SrcLoc.srcSpanFile (sourceCommentSpan other) == SrcLoc.srcSpanFile current
    && SrcLoc.srcSpanStartLine (sourceCommentSpan other)
      > SrcLoc.srcSpanEndLine current
    && SrcLoc.srcSpanStartLine (sourceCommentSpan other)
      < SrcLoc.srcSpanStartLine owner

removePlannedComment
  :: SourceCommentKey -> ExactPrintCompat.Anns -> ExactPrintCompat.Anns
removePlannedComment key = fmap $ \annotation -> annotation
  { ExactPrintCompat.annPriorComments = filter keep
      $ ExactPrintCompat.annPriorComments annotation
  , ExactPrintCompat.annFollowingComments = filter keep
      $ ExactPrintCompat.annFollowingComments annotation
  , ExactPrintCompat.annsDP = filter keepKeyword
      $ ExactPrintCompat.annsDP annotation
  }
 where
  keep (comment, _) = commentIdentity comment /= key
  keepKeyword (ExactPrintCompat.AnnComment comment, _) =
    commentIdentity comment /= key
  keepKeyword _ = True
  commentIdentity = SourceCommentKey . ExactPrintCompat.commentIdentifier

briDocLineLength :: BriDoc -> Int
briDocLineLength briDoc = flip StateS.evalState False $ rec briDoc
                          -- the state encodes whether a separator was already
                          -- appended at the current position.
 where
  rec = \case
    BDEmpty -> return $ 0
    BDBlankLine -> return 0
    BDLit t -> StateS.put False $> Text.length t
    BDSeq bds -> sum <$> rec `mapM` bds
    BDCols _ bds -> sum <$> rec `mapM` bds
    BDSeparator -> StateS.get >>= \b -> StateS.put True $> if b then 0 else 1
    BDAddBaseY _ bd -> rec bd
    BDBaseYPushCur bd -> rec bd
    BDBaseYPop bd -> rec bd
    BDIndentLevelPushCur bd -> rec bd
    BDIndentLevelPop bd -> rec bd
    BDPar _ line _ -> rec line
    BDDelimited group -> either (const $ return 0) rec
      $ delimiterDocument group
    BDAlt{} -> error "briDocLineLength BDAlt"
    BDForceMultiline bd -> rec bd
    BDForceSingleline bd -> rec bd
    BDColumnsLimit _ bd -> rec bd
    BDForwardLineMode bd -> rec bd
    BDExternal _ _ source -> return $ Text.length $ externalSourceText source
    BDPlain _ t -> return $ Text.length t
    BDComment{} -> return 0
    BDAnnotationPrior _ _ bd -> rec bd
    BDAnnotationKW _ _ bd -> rec bd
    BDAnnotationRest _ bd -> rec bd
    BDMoveToKWDP _ _ _ bd -> rec bd
    BDLines ls@(_ : _) -> do
      x <- StateS.get
      return $ maximum $ ls <&> \l -> StateS.evalState (rec l) x
    BDLines [] -> error "briDocLineLength BDLines []"
    BDEnsureIndent _ bd -> rec bd
    BDSetParSpacing bd -> rec bd
    BDForceParSpacing bd -> rec bd
    BDNonBottomSpacing _ bd -> rec bd
    BDDebug _ bd -> rec bd

briDocIsMultiLine :: BriDoc -> Bool
briDocIsMultiLine briDoc = rec briDoc
 where
  rec :: BriDoc -> Bool
  rec = \case
    BDEmpty -> False
    BDBlankLine -> False
    BDLit _ -> False
    BDSeq bds -> any rec bds
    BDCols _ bds -> any rec bds
    BDSeparator -> False
    BDAddBaseY _ bd -> rec bd
    BDBaseYPushCur bd -> rec bd
    BDBaseYPop bd -> rec bd
    BDIndentLevelPushCur bd -> rec bd
    BDIndentLevelPop bd -> rec bd
    BDPar{} -> True
    BDDelimited group -> either (const True) rec $ delimiterDocument group
    BDAlt{} -> error "briDocIsMultiLine BDAlt"
    BDForceMultiline _ -> True
    BDForceSingleline bd -> rec bd
    BDColumnsLimit _ bd -> rec bd
    BDForwardLineMode bd -> rec bd
    BDExternal _ _ source
      | [_] <- Text.lines (externalSourceText source) -> False
    BDExternal{} -> True
    BDPlain _ t | [_] <- Text.lines t -> False
    BDPlain{} -> True
    BDComment planned -> plannedCommentLineDelta planned > 0
      || length (Text.lines $ sourceCommentText $ plannedCommentSource planned) > 1
    BDAnnotationPrior _ _ bd -> rec bd
    BDAnnotationKW _ _ bd -> rec bd
    BDAnnotationRest _ bd -> rec bd
    BDMoveToKWDP _ _ _ bd -> rec bd
    BDLines (_ : _ : _) -> True
    BDLines [_] -> False
    BDLines [] -> error "briDocIsMultiLine BDLines []"
    BDEnsureIndent _ bd -> rec bd
    BDSetParSpacing bd -> rec bd
    BDForceParSpacing bd -> rec bd
    BDNonBottomSpacing _ bd -> rec bd
    BDDebug _ bd -> rec bd

sourceFragmentRequiresLineBoundary :: ExactSourceFragment -> Bool
sourceFragmentRequiresLineBoundary fragment = case reverse
  $ Text.lines
  $ fragmentText fragment <> Text.singleton '\n' of
  lastLine : _ -> priorCommentRequiresLineBoundary $ Text.unpack lastLine
  [] -> False

consumeSourceFragment
  :: Set.Set ExactPrintCompat.AnnKey
  -> Set.Set SourceCommentKey
  -> ExactPrintCompat.AnnKey
  -> ExactPrintCompat.Annotation
  -> ExactPrintCompat.Annotation
consumeSourceFragment subKeys commentSpans key annotation = annotation
  { ExactPrintCompat.annsDP = filter keepKeyword
      $ ExactPrintCompat.annsDP annotation
  , ExactPrintCompat.annFollowingComments = filter keepComment
      $ ExactPrintCompat.annFollowingComments annotation
  , ExactPrintCompat.annPriorComments = filter keepComment
      $ ExactPrintCompat.annPriorComments annotation
  }
 where
  keyIsConsumed = key `Set.member` subKeys
  keepKeyword (ExactPrintCompat.AnnComment comment, _) =
    keepCommentValue comment
  keepKeyword _ = not keyIsConsumed
  keepComment (comment, _) = keepCommentValue comment
  keepCommentValue comment =
    SourceCommentKey (ExactPrintCompat.commentIdentifier comment)
      `Set.notMember` commentSpans

consumeExactSourceFragment
  :: ExactSourceFragment
  -> ExactPrintCompat.Anns
  -> Either String ExactPrintCompat.Anns
consumeExactSourceFragment fragment annotations = do
  validateExactSourceFragment fragment
  pure $ Map.mapWithKey
    ( consumeSourceFragment
      (fragmentAnnotationKeys fragment)
      (fragmentCommentKeys fragment)
    )
    annotations

sourceFragmentCommentKeys :: BriDoc -> Set.Set SourceCommentKey
sourceFragmentCommentKeys = \case
  BDEmpty -> Set.empty
  BDBlankLine -> Set.empty
  BDLit _ -> Set.empty
  BDSeq documents -> nested documents
  BDCols _ documents -> nested documents
  BDSeparator -> Set.empty
  BDAddBaseY _ document -> sourceFragmentCommentKeys document
  BDBaseYPushCur document -> sourceFragmentCommentKeys document
  BDBaseYPop document -> sourceFragmentCommentKeys document
  BDIndentLevelPushCur document -> sourceFragmentCommentKeys document
  BDIndentLevelPop document -> sourceFragmentCommentKeys document
  BDPar _ sameLine indented -> nested [sameLine, indented]
  BDDelimited group -> either (const Set.empty) sourceFragmentCommentKeys
    $ delimiterDocument group
  BDAlt [] -> Set.empty
  BDAlt (selected : _) -> sourceFragmentCommentKeys selected
  BDForwardLineMode document -> sourceFragmentCommentKeys document
  BDExternal _ _ (SourceFragment fragment) -> fragmentCommentKeys fragment
  BDExternal _ _ ExactPrintSource{} -> Set.empty
  BDPlain _ _ -> Set.empty
  BDComment _ -> Set.empty
  BDAnnotationPrior _ _ document -> sourceFragmentCommentKeys document
  BDAnnotationKW _ _ document -> sourceFragmentCommentKeys document
  BDAnnotationRest _ document -> sourceFragmentCommentKeys document
  BDMoveToKWDP _ _ _ document -> sourceFragmentCommentKeys document
  BDLines documents -> nested documents
  BDEnsureIndent _ document -> sourceFragmentCommentKeys document
  BDForceMultiline document -> sourceFragmentCommentKeys document
  BDForceSingleline document -> sourceFragmentCommentKeys document
  BDColumnsLimit _ document -> sourceFragmentCommentKeys document
  BDNonBottomSpacing _ document -> sourceFragmentCommentKeys document
  BDSetParSpacing document -> sourceFragmentCommentKeys document
  BDForceParSpacing document -> sourceFragmentCommentKeys document
  BDDebug _ document -> sourceFragmentCommentKeys document
 where
  nested = Set.unions . fmap sourceFragmentCommentKeys

reserveSourceFragmentComments
  :: BriDoc -> ExactPrintCompat.Anns -> ExactPrintCompat.Anns
reserveSourceFragmentComments document = Map.mapWithKey
  $ consumeSourceFragment Set.empty (sourceFragmentCommentKeys document)

-- In theory
-- =========

-- .. this algorithm works roughly in these steps:
--
-- 1. For each line, get the (nested) column info, descending as far as
--    BDCols nodes go. The column info is a (rose) tree where the leafs
--    are arbitrary (non-BDCols) BriDocs.
-- 2. Walk through the lines and compare its column info with that of its
--    predecessor. If both are non-leafs and the column "signatures" align
--    (they don't align e.g. when they are totally different syntactical
--    structures or the number of children differs), mark these parts of
--    the two tree structures as connected and recurse to its children
--    (i.e. again comparing the children in this line with the children in
--    the previous line).
-- 3. What we now have is one tree per line, and connections between "same"
--    nodes between lines. These connection can span multiple lines.
--    We next look at spacing information. This is available at the leafs,
--    but in this step we aggregate _over connections_. At the top level, this
--    gives us one piece of data: How long would each line be, if we fully
--    aligned everything (kept all connections "active"). In contrast to
--    just taking the sum of all leafs for each tree, this line length includes
--    the spaces used for alignment.
-- 4. Treat those lines where alignment would result in overflowing of the
--    column limit. This "treatment" is currently configurable, and can e.g.
--    mean:
--    a) we stop alignment alltogether,
--    b) we remove alignment just from the overflowing lines,
--    c) we reduce the number of spaces inserted in overflowing lines using
--       some technique to make them not overflow, but without reducing the
--       space insertion to zero,
--    d) don't do anything
-- 5. Actually print the lines, walking over each tree and inserting spaces
--    according to the info and decisions gathered in the previous steps.
--
-- Possible improvements
-- =====================
--
-- - If alignment is disabled for specific lines, the aggregated per-connection
--   info of those lines is still retained and not recalculated. This can
--   result in spaces being inserted to create alignment with a line that
--   would overflow and thus gets disabled entirely.
--   An better approach would be to repeat step 3 after marking overflowing
--   lines as such, and not include the overflowing spacings as references
--   for non-overflowing ones. In the simplest case one additional iteration
--   would suffice, e.g. 1-2-3-4-3-5, but it would also be possible to refine
--   this and first remove alignment in the deepest parts of the tree for
--   overflowing lines, repeating and moving upwards until no lines are
--   anymore overflowing.
--   Further, it may make sense to break up connections when overflowing would
--   occur.
-- - It may also make sense to not filter all overflowing lines, but remove
--   them one-by-one and in each step recalculate the aggregated connection
--   spacing info. Because removing one overflowing line from the calculation
--   may very well cause another previously overflowing line to not overflow
--   any longer.
--   There is also a nasty optimization problem hiding in there (find the
--   minimal amount of alignment disabling that results in no overflows)
--   but that is overkill.
--
--   (with both these improvements there would be quite some repetition between
--   steps 3 and 4, but it should be possible to ensure termination. Still,
--   performance might become an issue as such an approach is not necessarily
--   linear in bridoc size any more.)
--
-- In practice
-- ===========
--
-- .. the current implementation is somewhat sloppy. Steps 1 and 2
-- are executed in one step, step 3 already applies one strategy that disables
-- certain connections (see `_lconfig_alignmentLimit`) and step 4 does some
-- of the calculations one might expect to occur in step 3. Steps 4 and 5
-- are executed in the same recursion, too.
-- Also, _lconfig_alignmentLimit really is itself a hack that hides the issue
-- mentioned in the first "possible improvement".
alignColsLines :: LayoutConstraints m => [BriDoc] -> m ()
alignColsLines bridocs = do -- colInfos `forM_` \colInfo -> do
  -- tellDebugMess ("alignColsLines: at " ++ take 100 (show $ briDocToDoc $ head bridocs))
  curX <- do
    state <- mGet
    return $ Either.fromLeft 0 (_lstate_curYOrAddNewline state) + fromMaybe
      0
      (_lstate_addSepSpace state)
  colMax <- mAsk <&> _conf_layout .> _lconfig_cols .> confUnpack
  alignMax <- mAsk <&> _conf_layout .> _lconfig_alignmentLimit .> confUnpack
  dumpAlignmentPlan <-
    mAsk <&> _conf_debug .> _dconf_dump_bridoc_simpl_columns .> confUnpack
  alignBreak <-
    mAsk <&> _conf_layout .> _lconfig_alignmentBreakOnMultiline .> confUnpack
  case () of
    _ -> do
      case alignmentResult of
        Left plannerError -> mTell
          [ErrorAlignmentPlan $ show plannerError]
        Right{} -> pure ()
      when dumpAlignmentPlan $ alignmentPlans `forM_` \plan ->
        tellDebugMessShow ("column-alignment-plan", plan)
      -- tellDebugMess ("processedMap: " ++ show processedMap)
      sequence_
        $ List.intersperse layoutWriteEnsureNewlineBlock
        $ colInfos
        <&> processInfo colMax processedMap
     where
      (colInfos, finalState) =
        StateS.runState (mergeBriDocs bridocs) (ColBuildState IntMapS.empty 0)
      alignmentResult = columnAlignmentPlans
        alignMax (max 0 $ colMax - curX) bridocs
      alignmentPlans = either (const []) id alignmentResult
      alignmentBreaks = Set.fromList
        [ (rowIdentity, columnAlignmentPlanPath plan)
        | plan <- alignmentPlans
        , rowIdentity <- alignmentPlanBreaks $ columnAlignmentPlanResult plan
        ]
      -- maxZipper :: [Int] -> [Int] -> [Int]
      -- maxZipper [] ys = ys
      -- maxZipper xs [] = xs
      -- maxZipper (x:xr) (y:yr) = max x y : maxZipper xr yr
      colAggregation :: [Int] -> Int
      colAggregation [] = 0 -- this probably cannot happen the way we call
                            -- this function, because _cbs_map only ever
                            -- contains nonempty Seqs.
      colAggregation xs
        | maximum xs - minimum xs <= alignmentPaddingLimit alignMax = maximum xs
        | otherwise = minimum xs

      processedMap :: ColMap2
      processedMap = fix $ \result ->
        _cbs_map finalState <&> \(lastFlag, colSpacingss) ->
          let
            colss = colSpacingss <&> \spss -> case reverse spss of
              [] -> []
              (xN : xR) ->
                reverse $ (if lastFlag then fLast else fInit) xN : fmap fInit xR
             where
              fLast (ColumnSpacingLeaf len) = len
              fLast (ColumnSpacingRef len _) = len
              fInit (ColumnSpacingLeaf len) = len
              fInit (ColumnSpacingRef _ i) = case IntMapL.lookup i result of
                Nothing -> 0
                Just (_, maxs, _) -> sum maxs
            maxCols = {-Foldable.foldl1 maxZipper-}
              fmap colAggregation $ transpose $ Foldable.toList colss
            (_, posXs) = -- trace ("colss=" ++ show colss ++ ", maxCols=" ++ show maxCols ++ " for " ++ take 100 (show $ briDocToDoc $ head bridocs)) $
              mapAccumL (\acc x -> (acc + x, acc)) curX maxCols
            counter count l = if List.last posXs + List.last l <= colMax
              then count + 1
              else count
            ratio = fromIntegral (foldl counter (0 :: Int) colss)
              / fromIntegral (length colss)
          in (ratio, maxCols, colss)

      mergeBriDocs :: [BriDoc] -> StateS.State ColBuildState [ColInfo]
      mergeBriDocs bds = mergeBriDocsW 0 ColInfoStart bds

      mergeBriDocsW
        :: Int -> ColInfo -> [BriDoc] -> StateS.State ColBuildState [ColInfo]
      mergeBriDocsW _ _ [] = return []
      mergeBriDocsW rowIndex lastInfo (bd : bdr) = do
        let plannedLastInfo = if Set.member (rowIndex, []) alignmentBreaks
              then ColInfoStart
              else lastInfo
        info <- mergeInfoBriDoc True rowIndex [] plannedLastInfo bd
        infor <- mergeBriDocsW
          (rowIndex + 1)
          (if shouldBreakAfter bd then ColInfoStart else info)
          bdr
        return $ info : infor

      shouldBreakAfter :: BriDoc -> Bool
      shouldBreakAfter bd = alignBreak && briDocIsMultiLine bd && case bd of
        BDCols ColTyOpPrefix _ -> False
        BDCols ColPatternsFuncPrefix _ -> True
        BDCols ColPatternsFuncInfix _ -> True
        BDCols ColPatterns _ -> True
        BDCols ColCasePattern _ -> True
        BDCols ColBindingLine{} _ -> True
        BDCols ColGuard _ -> True
        BDCols ColGuardedBody _ -> True
        BDCols ColBindStmt _ -> True
        BDCols ColDoLet _ -> True
        BDCols ColRec _ -> False
        BDCols ColRecUpdate _ -> False
        BDCols ColRecDecl _ -> False
        BDCols ColListComp _ -> False
        BDCols ColList _ -> False
        BDCols ColApp{} _ -> True
        BDCols ColTuple _ -> False
        BDCols ColTuples _ -> False
        BDCols ColOpPrefix _ -> False
        BDDelimited group -> either (const True) shouldBreakAfter
          $ delimiterDocument group
        _ -> True

      mergeInfoBriDoc
        :: Bool
        -> Int
        -> ColumnPath
        -> ColInfo
        -> BriDoc
        -> StateS.StateT ColBuildState Identity ColInfo
      mergeInfoBriDoc lastFlag rowIndex path _ brdc
        | Set.member (rowIndex, path) alignmentBreaks =
            briDocToColInfo lastFlag brdc
      mergeInfoBriDoc lastFlag _ _ ColInfoStart brdc =
        briDocToColInfo lastFlag brdc
      mergeInfoBriDoc lastFlag _ _ ColInfoNo{} brdc =
        briDocToColInfo lastFlag brdc
      mergeInfoBriDoc lastFlag rowIndex path
          (ColInfo infoInd infoSig subLengthsInfos) currentDocument =
        case currentDocument of
          brdc@(BDCols colSig subDocs)
            | Just resetSubColumns <- colSigsMerge infoSig colSig
            , length subLengthsInfos == length subDocs -> do
              let
                isLastList = if lastFlag
                  then (== length subDocs) <$> [1 ..]
                  else repeat False
              infos <- zip [0 ..]
                (zip3 isLastList (snd <$> subLengthsInfos) subDocs)
                `forM` \(subIndex, (lf, info, bd)) -> if resetSubColumns
                  then briDocToColInfo lf bd
                  else mergeInfoBriDoc
                    lf rowIndex (path ++ [subIndex]) info bd
              let curLengths = briDocLineLength <$> subDocs
              let trueSpacings = getTrueSpacings (zip curLengths infos)
              do -- update map
                s <- StateS.get
                let m = _cbs_map s
                let (Just (_, spaces)) = IntMapS.lookup infoInd m
                StateS.put s
                  { _cbs_map = IntMapS.insert
                    infoInd
                    (lastFlag, spaces Seq.|> trueSpacings)
                    m
                  }
              return $ ColInfo infoInd colSig (zip curLengths infos)
            | otherwise -> briDocToColInfo lastFlag brdc
          delimited@(BDDelimited group) -> case delimiterDocument group of
            Right document -> mergeInfoBriDoc
              lastFlag rowIndex path
              (ColInfo infoInd infoSig subLengthsInfos)
              document
            Left{} -> return $ ColInfoNo delimited
          brdc -> return $ ColInfoNo brdc

      colSigsMerge :: ColSig -> ColSig -> Maybe Bool
      colSigsMerge (ColBindingLine previous) (ColBindingLine current)
        | candidatesOverlap structuralGroups previous current = Just False
        | candidatesOverlap optionalGroupsFor previous current =
            Just True
        | otherwise = Nothing
      colSigsMerge previous current
        | previous == current = Just False
        | otherwise = Nothing

      candidatesOverlap select previous current = not $ null
        $ select previous `List.intersect` select current

      structuralGroups candidates =
        [group | StructuralAffinity group <- candidates]

      optionalGroupsFor candidates =
        [group | OptionalAlignment group <- candidates]

briDocToColInfo :: Bool -> BriDoc -> StateS.State ColBuildState ColInfo
briDocToColInfo lastFlag = \case
  BDCols sig list -> withAlloc lastFlag $ \ind -> do
    let
      isLastList =
        if lastFlag then (== length list) <$> [1 ..] else repeat False
    subInfos <- zip isLastList list `forM` uncurry briDocToColInfo
    let lengthInfos = zip (briDocLineLength <$> list) subInfos
    let trueSpacings = getTrueSpacings lengthInfos
    return $ (Seq.singleton trueSpacings, ColInfo ind sig lengthInfos)
  delimited@(BDDelimited group) -> case delimiterDocument group of
    Right document -> briDocToColInfo lastFlag document
    Left{} -> return $ ColInfoNo delimited
  bd -> return $ ColInfoNo bd

getTrueSpacings :: [(Int, ColInfo)] -> [ColumnSpacing]
getTrueSpacings lengthInfos = lengthInfos <&> \case
  (len, ColInfo i _ _) -> ColumnSpacingRef len i
  (len, _) -> ColumnSpacingLeaf len

withAlloc
  :: Bool
  -> ( ColIndex
     -> StateS.State ColBuildState (ColumnBlocks ColumnSpacing, ColInfo)
     )
  -> StateS.State ColBuildState ColInfo
withAlloc lastFlag f = do
  cbs <- StateS.get
  let ind = _cbs_index cbs
  StateS.put $ cbs { _cbs_index = ind + 1 }
  (space, info) <- f ind
  StateS.get >>= \c -> StateS.put
    $ c { _cbs_map = IntMapS.insert ind (lastFlag, space) $ _cbs_map c }
  return info

processInfo :: LayoutConstraints m => Int -> ColMap2 -> ColInfo -> m ()
processInfo maxSpace m = \case
  ColInfoStart -> error "should not happen (TM)"
  ColInfoNo doc -> layoutBriDocM doc
  ColInfo ind _ list -> -- trace ("processInfo ind=" ++ show ind ++ ", list=" ++ show list ++ ", colmap=" ++ show m) $
                        do
    colMaxConf <- mAsk <&> _conf_layout .> _lconfig_cols .> confUnpack
    alignMode <- mAsk <&> _conf_layout .> _lconfig_columnAlignMode .> confUnpack
    curX <- do
      state <- mGet
      -- tellDebugMess ("processInfo: " ++ show (_lstate_curYOrAddNewline state) ++ " - " ++ show ((_lstate_addSepSpace state)))
      let spaceAdd = fromMaybe 0 $ _lstate_addSepSpace state
      return $ case _lstate_curYOrAddNewline state of
        Left i -> case _lstate_commentCol state of
          Nothing -> spaceAdd + i
          Just c -> c
        Right{} -> spaceAdd
    let colMax = min colMaxConf (curX + maxSpace)
    -- tellDebugMess $ show curX
    let Just (ratio, maxCols1, _colss) = IntMapS.lookup ind m
    let
      maxCols2 = list <&> \case
        (_, ColInfo i _ _) ->
          let Just (_, ms, _) = IntMapS.lookup i m in sum ms
        (l, _) -> l
    let maxCols = zipWith max maxCols1 maxCols2
    let (maxX, posXs) = mapAccumL (\acc x -> (acc + x, acc)) curX maxCols
    -- handle the cases that the vertical alignment leads to more than max
    -- cols:
    -- this is not a full fix, and we must correct individually in addition.
    -- because: the (at least) line with the largest element in the last
    -- column will always still overflow, because we just updated the column
    -- sizes in such a way that it works _if_ we have sizes (*factor)
    -- in each column. but in that line, in the last column, we will be
    -- forced to occupy the full vertical space, not reduced by any factor.
    let
      fixedPosXs = case alignMode of
        ColumnAlignModeAnimouslyScale i | maxX > colMax -> fixed <&> (+ curX)
         where
          factor :: Float =
            -- 0.0001 as an offering to the floating point gods.
                            min
            1.0001
            (fromIntegral (i + colMax - curX) / fromIntegral (maxX - curX))
          offsets = (subtract curX) <$> posXs
          fixed = offsets <&> fromIntegral .> (* factor) .> truncate
        _ -> posXs
    let
      spacings =
        zipWith (-) (List.tail fixedPosXs ++ [min maxX colMax]) fixedPosXs
    -- tellDebugMess $ "ind = " ++ show ind
    -- tellDebugMess $ "maxCols = " ++ show maxCols
    -- tellDebugMess $ "fixedPosXs = " ++ show fixedPosXs
    -- tellDebugMess $ "list = " ++ show list
    -- tellDebugMess $ "maxSpace = " ++ show maxSpace
    let
      alignAct = zip3 fixedPosXs spacings list `forM_` \(destX, s, x) -> do
        layoutWriteEnsureAbsoluteN destX
        processInfo s m (snd x)
      noAlignAct = list `forM_` (snd .> processInfoIgnore)
      animousAct = -- trace ("animousAct fixedPosXs=" ++ show fixedPosXs ++ ", list=" ++ show list ++ ", maxSpace=" ++ show maxSpace ++ ", colMax="++show colMax) $
                   if List.last fixedPosXs + fst (List.last list) > colMax
                                                                                                                                                                   -- per-item check if there is overflowing.
        then noAlignAct
        else alignAct
    case alignMode of
      ColumnAlignModeDisabled -> noAlignAct
      ColumnAlignModeUnanimously | maxX <= colMax -> alignAct
      ColumnAlignModeUnanimously -> noAlignAct
      ColumnAlignModeMajority limit | ratio >= limit -> animousAct
      ColumnAlignModeMajority{} -> noAlignAct
      ColumnAlignModeAnimouslyScale{} -> animousAct
      ColumnAlignModeAnimously -> animousAct
      ColumnAlignModeAlways -> alignAct

processInfoIgnore :: LayoutConstraints m => ColInfo -> m ()
processInfoIgnore = \case
  ColInfoStart -> error "should not happen (TM)"
  ColInfoNo doc -> layoutBriDocM doc
  ColInfo _ _ list -> list `forM_` (snd .> processInfoIgnore)
