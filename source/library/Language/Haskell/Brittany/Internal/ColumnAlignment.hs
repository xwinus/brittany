{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.ColumnAlignment
  ( ColumnPath
  , ColumnAlignmentKey(..)
  , ColumnAlignmentPlan(..)
  , ColumnAlignmentError(..)
  , columnAlignmentPlans
  ) where

import qualified Control.Monad.Trans.State.Strict as StateS
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Language.Haskell.Brittany.Internal.Alignment
import Language.Haskell.Brittany.Internal.Delimiter
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Types

type ColumnPath = [Int]

data ColumnAlignmentKey
  = ColumnSignature ColSig
  | BindingAffinity (Either Text.Text ())
  | OperatorLayoutAffinity Text.Text
  deriving (Eq, Ord, Show)

data ColumnAlignmentPlan = ColumnAlignmentPlan
  { columnAlignmentPlanPath :: ColumnPath
  , columnAlignmentPlanResult :: AlignmentPlan ColumnAlignmentKey
  }
  deriving (Eq, Show)

data ColumnAlignmentError = ColumnAlignmentError
  { columnAlignmentErrorPath :: ColumnPath
  , columnAlignmentErrorRow :: Int
  , columnAlignmentPlanError :: AlignmentPlanError
  }
  deriving (Eq, Show)

columnAlignmentPlans
  :: Int
  -> Int
  -> [BriDoc]
  -> Either ColumnAlignmentError [ColumnAlignmentPlan]
columnAlignmentPlans alignmentLimit configuredWidth documents =
  List.concat <$> mapM planPath paths
 where
  paths = List.sort $ List.nub $ List.concatMap columnPaths documents

  planPath path = collect path 0 documents

  collect _ _ [] = Right []
  collect path rowIndex remainingDocuments@(document : remaining) = case
      columnAlignmentRowAt path rowIndex document of
    Nothing -> collect path (rowIndex + 1) remaining
    Just{} -> do
      let (columnDocuments, rest) = List.span
            (Maybe.isJust . columnAlignmentRowAt path 0)
            remainingDocuments
          rows = markHeterogeneousOperatorRows
            $ Maybe.mapMaybe (uncurry $ columnAlignmentRowAt path)
            $ zip [rowIndex ..] columnDocuments
          nextIndex = rowIndex + length columnDocuments
      plan <- case planAlignmentWithin alignmentLimit configuredWidth rows of
        Left plannerError -> Left
          $ ColumnAlignmentError path rowIndex plannerError
        Right alignmentPlan -> Right alignmentPlan
      plans <- collect path nextIndex rest
      pure $ ColumnAlignmentPlan path plan : plans

columnPaths :: BriDoc -> [ColumnPath]
columnPaths = descend []
 where
  descend path document = case columnNode document of
    Nothing -> []
    Just (_, columns) -> path
      : List.concat
        [ descend (path ++ [index]) column
        | (index, column) <- zip [0 ..] columns
        ]

columnAlignmentRowAt
  :: ColumnPath
  -> Int
  -> BriDoc
  -> Maybe (AlignmentRow ColumnAlignmentKey)
columnAlignmentRowAt path identity document = do
  nestedDocument <- columnDocumentAt path document
  (signature, columns) <- columnNode nestedDocument
  firstColumn <- listToMaybe columns
  let prefixWidth = columnLineLength firstColumn
      contentWidth = sum $ columnLineLength <$> columns
  pure AlignmentRow
    { alignmentRowIdentity = identity
    , alignmentRowCandidates = columnCandidates signature firstColumn
    , alignmentRowWidth = prefixWidth
    , alignmentRowContentWidth = max prefixWidth contentWidth
    , alignmentRowBreakCost = 1
    }

columnDocumentAt :: ColumnPath -> BriDoc -> Maybe BriDoc
columnDocumentAt [] document = Just document
columnDocumentAt (index : remaining) document = do
  (_, columns) <- columnNode document
  column <- listAt index columns
  columnDocumentAt remaining column

listAt :: Int -> [value] -> Maybe value
listAt index values = case drop index values of
  value : _ -> Just value
  [] -> Nothing

columnNode :: BriDoc -> Maybe (ColSig, [BriDoc])
columnNode = \case
  BDCols signature columns -> Just (signature, columns)
  BDDelimited group -> either (const Nothing) columnNode
    $ delimiterDocument group
  BDAddBaseY _ nested -> columnNode nested
  BDBaseYPushCur nested -> columnNode nested
  BDBaseYPop nested -> columnNode nested
  BDIndentLevelPushCur nested -> columnNode nested
  BDIndentLevelPop nested -> columnNode nested
  BDForceMultiline nested -> columnNode nested
  BDForceSingleline nested -> columnNode nested
  BDColumnsLimit _ nested -> columnNode nested
  BDForwardLineMode nested -> columnNode nested
  BDAnnotationPrior _ _ nested -> columnNode nested
  BDAnnotationKW _ _ nested -> columnNode nested
  BDAnnotationRest _ nested -> columnNode nested
  BDMoveToKWDP _ _ _ nested -> columnNode nested
  BDEnsureIndent _ nested -> columnNode nested
  BDSetParSpacing nested -> columnNode nested
  BDForceParSpacing nested -> columnNode nested
  BDNonBottomSpacing _ nested -> columnNode nested
  BDDebug _ nested -> columnNode nested
  _ -> Nothing

columnCandidates
  :: ColSig
  -> BriDoc
  -> [AlignmentCandidate ColumnAlignmentKey]
columnCandidates signature firstColumn = case signature of
  ColBindingLine candidates -> translateBindingCandidate <$> candidates
  ColOpPrefix ->
    StructuralAffinity (ColumnSignature signature)
      : operatorLayoutCandidate firstColumn
  otherSignature
    | structurallyAtomic otherSignature ->
        [ StructuralAffinity $ ColumnSignature otherSignature
        , OptionalAlignment $ ColumnSignature otherSignature
        ]
    | otherwise -> [OptionalAlignment $ ColumnSignature otherSignature]

operatorLayoutCandidate
  :: BriDoc
  -> [AlignmentCandidate ColumnAlignmentKey]
operatorLayoutCandidate document = case columnLayoutText document of
  Just layout | not $ Text.null layout ->
    [OptionalAlignment $ OperatorLayoutAffinity layout]
  _ -> [UnalignedLayout]

markHeterogeneousOperatorRows
  :: [AlignmentRow ColumnAlignmentKey]
  -> [AlignmentRow ColumnAlignmentKey]
markHeterogeneousOperatorRows [] = []
markHeterogeneousOperatorRows remaining@(row : rows)
  | isOperatorRow row =
      let (operatorRows, rest) = List.span isOperatorRow remaining
      in markGroup operatorRows ++ markHeterogeneousOperatorRows rest
  | otherwise = row : markHeterogeneousOperatorRows rows
 where
  markGroup operatorRows
    | length (List.nub $ Maybe.mapMaybe operatorAffinity operatorRows) > 1 =
        addUnalignedLayout <$> operatorRows
    | otherwise = operatorRows

  addUnalignedLayout operatorRow
    | UnalignedLayout `elem` alignmentRowCandidates operatorRow = operatorRow
    | otherwise = operatorRow
        { alignmentRowCandidates = UnalignedLayout
            : alignmentRowCandidates operatorRow
        }

isOperatorRow :: AlignmentRow ColumnAlignmentKey -> Bool
isOperatorRow row = StructuralAffinity (ColumnSignature ColOpPrefix)
  `elem` alignmentRowCandidates row

operatorAffinity
  :: AlignmentRow ColumnAlignmentKey
  -> Maybe Text.Text
operatorAffinity row = listToMaybe
  [layout | OptionalAlignment (OperatorLayoutAffinity layout) <- candidates]
 where
  candidates = alignmentRowCandidates row

translateBindingCandidate
  :: AlignmentCandidate (Either Text.Text ())
  -> AlignmentCandidate ColumnAlignmentKey
translateBindingCandidate = \case
  StructuralAffinity key -> StructuralAffinity $ BindingAffinity key
  OptionalAlignment key -> OptionalAlignment $ BindingAffinity key
  ProhibitedAlignment key -> ProhibitedAlignment $ BindingAffinity key
  UnalignedLayout -> UnalignedLayout

structurallyAtomic :: ColSig -> Bool
structurallyAtomic = \case
  ColTyOpPrefix -> True
  ColApp{} -> True
  _ -> False

columnLayoutText :: BriDoc -> Maybe Text.Text
columnLayoutText = \case
  BDEmpty -> Just Text.empty
  BDLit text -> Just text
  BDSeq nested -> Text.concat <$> mapM columnLayoutText nested
  BDSeparator -> Just Text.empty
  BDAddBaseY _ nested -> columnLayoutText nested
  BDBaseYPushCur nested -> columnLayoutText nested
  BDBaseYPop nested -> columnLayoutText nested
  BDIndentLevelPushCur nested -> columnLayoutText nested
  BDIndentLevelPop nested -> columnLayoutText nested
  BDDelimited group -> either (const Nothing) columnLayoutText
    $ delimiterDocument group
  BDForceMultiline nested -> columnLayoutText nested
  BDForceSingleline nested -> columnLayoutText nested
  BDColumnsLimit _ nested -> columnLayoutText nested
  BDForwardLineMode nested -> columnLayoutText nested
  BDExternal _ _ source -> Just $ externalSourceText source
  BDPlain _ text -> Just text
  BDAnnotationPrior _ _ nested -> columnLayoutText nested
  BDAnnotationKW _ _ nested -> columnLayoutText nested
  BDAnnotationRest _ nested -> columnLayoutText nested
  BDMoveToKWDP _ _ _ nested -> columnLayoutText nested
  BDLines [nested] -> columnLayoutText nested
  BDEnsureIndent _ nested -> columnLayoutText nested
  BDSetParSpacing nested -> columnLayoutText nested
  BDForceParSpacing nested -> columnLayoutText nested
  BDNonBottomSpacing _ nested -> columnLayoutText nested
  BDDebug _ nested -> columnLayoutText nested
  _ -> Nothing

columnLineLength :: BriDoc -> Int
columnLineLength document = flip StateS.evalState False $ measure document
 where
  measure = \case
    BDEmpty -> pure 0
    BDBlankLine -> pure 0
    BDLit text -> StateS.put False $> Text.length text
    BDSeq nested -> sum <$> mapM measure nested
    BDCols _ nested -> sum <$> mapM measure nested
    BDSeparator -> do
      separatorPending <- StateS.get
      StateS.put True
      pure $ if separatorPending then 0 else 1
    BDAddBaseY _ nested -> measure nested
    BDBaseYPushCur nested -> measure nested
    BDBaseYPop nested -> measure nested
    BDIndentLevelPushCur nested -> measure nested
    BDIndentLevelPop nested -> measure nested
    BDPar _ sameLine _ -> measure sameLine
    BDDelimited group -> either (const $ pure 0) measure
      $ delimiterDocument group
    BDAlt{} -> pure 0
    BDForceMultiline nested -> measure nested
    BDForceSingleline nested -> measure nested
    BDColumnsLimit _ nested -> measure nested
    BDForwardLineMode nested -> measure nested
    BDExternal _ _ source -> pure $ Text.length $ externalSourceText source
    BDPlain _ text -> pure $ Text.length text
    BDComment{} -> pure 0
    BDAnnotationPrior _ _ nested -> measure nested
    BDAnnotationKW _ _ nested -> measure nested
    BDAnnotationRest _ nested -> measure nested
    BDMoveToKWDP _ _ _ nested -> measure nested
    BDLines [] -> pure 0
    BDLines lines -> do
      separatorPending <- StateS.get
      pure $ maximum
        [StateS.evalState (measure line) separatorPending | line <- lines]
    BDEnsureIndent _ nested -> measure nested
    BDSetParSpacing nested -> measure nested
    BDForceParSpacing nested -> measure nested
    BDNonBottomSpacing _ nested -> measure nested
    BDDebug _ nested -> measure nested
