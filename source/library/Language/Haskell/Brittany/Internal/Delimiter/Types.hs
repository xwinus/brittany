{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Delimiter.Types
  ( DelimiterKind(..)
  , DelimiterLayout(..)
  , DelimiterIndent(..)
  , DelimiterGroupId(..)
  , DelimiterChildId(..)
  , DelimiterSeparatorId(..)
  , DelimiterBoundaryId(..)
  , DelimiterElementId(..)
  , DelimiterChildKind(..)
  , DelimiterSeparatorKind(..)
  , DelimiterAttachment(..)
  , DelimiterRenderProfile(..)
  , DelimiterChild(..)
  , DelimiterSeparator(..)
  , DelimiterBoundary(..)
  , DelimiterSequence(..)
  , DelimiterSelection(..)
  , DelimitedGroup(..)
  , DelimiterInvariantError(..)
  , mkDelimitedGroup
  , validateDelimitedGroup
  , activeDelimitedDocuments
  , mapDelimitedGroup
  , replaceDelimitedDocuments
  , selectDelimitedDocument
  , selectedDelimiterDocument
  ) where

import qualified Data.Data as Data
import qualified Data.List as List
import qualified Data.Text as Text
import Language.Haskell.Brittany.Internal.ExactPrintCompat (AnnKey)
import Language.Haskell.Brittany.Internal.Prelude

data DelimiterKind
  = ParenthesesDelimiter
  | SquareBracketsDelimiter
  | CurlyBracesDelimiter
  | UnboxedParenthesesDelimiter
  | CustomDelimiter
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterLayout
  = DelimiterCompact
  | DelimiterAttached
  | DelimiterHanging
  | DelimiterVertical
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterIndent
  = DelimiterIndentNone
  | DelimiterIndentRegular
  | DelimiterIndentFixed Int
  deriving (Data.Data, Eq, Ord, Show)

newtype DelimiterGroupId = DelimiterGroupId Int
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterChildId = DelimiterChildId DelimiterGroupId Int
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterSeparatorId = DelimiterSeparatorId DelimiterGroupId Int
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterBoundaryId = DelimiterBoundaryId DelimiterGroupId Int
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterElementId
  = DelimiterOpenElement DelimiterGroupId
  | DelimiterChildElement DelimiterChildId
  | DelimiterSeparatorElement DelimiterSeparatorId
  | DelimiterCloseElement DelimiterGroupId
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterChildKind
  = PresentDelimiterChild
  | TupleHoleDelimiterChild
  | RangeHoleDelimiterChild
  | RecordWildcardDelimiterChild
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterSeparatorKind
  = RepeatedDelimiterSeparator
  | ListComprehensionBar
  | RangeDelimiterOperator
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterAttachment
  = AttachSeparatorLeft
  | AttachSeparatorRight
  | AttachSeparatorEitherNonStandalone
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterRenderProfile
  = LeadingDelimiterSeparators
  | TrailingDelimiterSeparators
  | BlockDelimiterChild
  | PatternBlockDelimiterChild
  | TypeBlockDelimiterChild
  | ImportExportDelimiter
  | TightImportExportDelimiter
  | ModuleExportDelimiter
  | NestedIEDelimiter
  | PatternInlineDelimiter
  | PromotedListDelimiter
  | ListComprehensionDelimiter
  | TypeDelimiterSeparators
  | RecordDelimiterFields
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterChild document = DelimiterChild
  { delimiterChildId :: DelimiterChildId
  , delimiterChildOwner :: Maybe AnnKey
  , delimiterChildKind :: DelimiterChildKind
  , delimiterChildDocument :: document
  }
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterSeparator = DelimiterSeparator
  { delimiterSeparatorId :: DelimiterSeparatorId
  , delimiterSeparatorKind :: DelimiterSeparatorKind
  , delimiterSeparatorToken :: Text
  , delimiterSeparatorLeft :: DelimiterChildId
  , delimiterSeparatorRight :: DelimiterChildId
  , delimiterSeparatorAttachment :: DelimiterAttachment
  }
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterBoundary = DelimiterBoundary
  { delimiterBoundaryId :: DelimiterBoundaryId
  , delimiterBoundaryLeft :: DelimiterElementId
  , delimiterBoundaryRight :: DelimiterElementId
  }
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterSequence document = DelimiterSequence
  { delimiterSequenceId :: DelimiterGroupId
  , delimiterSequenceKind :: DelimiterKind
  , delimiterSequenceOpenToken :: Text
  , delimiterSequenceCloseToken :: Text
  , delimiterSequenceOwner :: Maybe AnnKey
  , delimiterSequenceChildren :: [DelimiterChild document]
  , delimiterSequenceSeparators :: [DelimiterSeparator]
  , delimiterSequenceBoundaries :: [DelimiterBoundary]
  , delimiterSequenceIndent :: DelimiterIndent
  , delimiterSequenceProfile :: DelimiterRenderProfile
  }
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterSelection document
  = UnselectedDelimiter
  | SelectedDelimiter DelimiterLayout document
  deriving (Data.Data, Eq, Ord, Show)

data DelimitedGroup document = DelimitedGroup
  { delimitedSequence :: DelimiterSequence document
  , delimitedAllowedLayouts :: [DelimiterLayout]
  , delimitedSelection :: DelimiterSelection document
  }
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterInvariantError
  = InvalidDelimiterTokens DelimiterKind Text Text
  | MissingDelimiterLayout
  | DuplicateDelimiterLayout DelimiterLayout
  | DuplicateDelimiterChildId DelimiterChildId
  | DuplicateDelimiterSeparatorId DelimiterSeparatorId
  | DuplicateDelimiterBoundaryId DelimiterBoundaryId
  | ForeignDelimiterChildId DelimiterChildId
  | ForeignDelimiterSeparatorId DelimiterSeparatorId
  | ForeignDelimiterBoundaryId DelimiterBoundaryId
  | InvalidDelimiterSeparatorCount Int Int
  | InvalidDelimiterSeparatorLink
      DelimiterSeparatorId DelimiterChildId DelimiterChildId
  | InvalidDelimiterBoundaryCount Int Int
  | InvalidDelimiterBoundaryLink
      DelimiterBoundaryId DelimiterElementId DelimiterElementId
  | IllegalListComprehensionBar DelimiterSeparatorId
  | InvalidDelimiterAttachment
      DelimiterSeparatorId DelimiterRenderProfile DelimiterAttachment
  | DelimiterLayoutNotSelected
  | SelectedDelimiterLayoutNotAllowed DelimiterLayout
  | AccidentalStandaloneDelimiter DelimiterLayout Text
  | StandaloneStructuralPunctuation
      DelimiterGroupId DelimiterLayout DelimiterElementId Text
  deriving (Data.Data, Eq, Ord, Show)

mkDelimitedGroup
  :: Int
  -> DelimiterKind
  -> Text
  -> Text
  -> Maybe AnnKey
  -> [(Maybe AnnKey, DelimiterChildKind, document)]
  -> [(DelimiterSeparatorKind, Text, DelimiterAttachment)]
  -> DelimiterIndent
  -> DelimiterRenderProfile
  -> [DelimiterLayout]
  -> DelimitedGroup document
mkDelimitedGroup rawGroupId kind open close owner childInputs separatorInputs
    indent profile layouts = DelimitedGroup
  { delimitedSequence = sequence'
  , delimitedAllowedLayouts = layouts
  , delimitedSelection = UnselectedDelimiter
  }
 where
  groupId = DelimiterGroupId rawGroupId
  children = zipWith makeChild [0 ..] childInputs
  makeChild index (childOwner, childKind, document) = DelimiterChild
    { delimiterChildId = DelimiterChildId groupId index
    , delimiterChildOwner = childOwner
    , delimiterChildKind = childKind
    , delimiterChildDocument = document
    }
  separators = List.zipWith3 makeSeparator [0 ..]
    (zip children $ drop 1 children) separatorInputs
  makeSeparator index (left, right) (separatorKind, token, attachment) =
    DelimiterSeparator
      { delimiterSeparatorId = DelimiterSeparatorId groupId index
      , delimiterSeparatorKind = separatorKind
      , delimiterSeparatorToken = token
      , delimiterSeparatorLeft = delimiterChildId left
      , delimiterSeparatorRight = delimiterChildId right
      , delimiterSeparatorAttachment = attachment
      }
  elements = delimiterElements groupId children separators
  boundaries = zipWith makeBoundary [0 ..] $ zip elements $ drop 1 elements
  makeBoundary index (left, right) = DelimiterBoundary
    (DelimiterBoundaryId groupId index) left right
  sequence' = DelimiterSequence
    { delimiterSequenceId = groupId
    , delimiterSequenceKind = kind
    , delimiterSequenceOpenToken = open
    , delimiterSequenceCloseToken = close
    , delimiterSequenceOwner = owner
    , delimiterSequenceChildren = children
    , delimiterSequenceSeparators = separators
    , delimiterSequenceBoundaries = boundaries
    , delimiterSequenceIndent = indent
    , delimiterSequenceProfile = profile
    }

delimiterElements
  :: DelimiterGroupId
  -> [DelimiterChild document]
  -> [DelimiterSeparator]
  -> [DelimiterElementId]
delimiterElements groupId children separators = DelimiterOpenElement groupId
  : List.concat
    [ DelimiterChildElement (delimiterChildId child)
        : [ DelimiterSeparatorElement $ delimiterSeparatorId separator
          | separator <- take 1 $ drop index separators
          ]
    | (index, child) <- zip [0 ..] children
    ]
  ++ [DelimiterCloseElement groupId]

validateDelimitedGroup
  :: DelimitedGroup document -> Either DelimiterInvariantError ()
validateDelimitedGroup group = do
  validateTokens sequence'
  validateLayouts group
  validateIds sequence'
  validateSeparators sequence'
  validateBoundaries sequence'
  validateSelection group
 where
  sequence' = delimitedSequence group

validateTokens
  :: DelimiterSequence document -> Either DelimiterInvariantError ()
validateTokens sequence'
  | validTokenPair kind open close = Right ()
  | otherwise = Left $ InvalidDelimiterTokens kind open close
 where
  kind = delimiterSequenceKind sequence'
  open = delimiterSequenceOpenToken sequence'
  close = delimiterSequenceCloseToken sequence'

validTokenPair :: DelimiterKind -> Text -> Text -> Bool
validTokenPair kind open close = case kind of
  ParenthesesDelimiter -> pair "(" ")"
  SquareBracketsDelimiter -> pair "[" "]"
  CurlyBracesDelimiter -> pair "{" "}"
  UnboxedParenthesesDelimiter -> pair "(#" "#)"
  CustomDelimiter -> not (Text.null open) && not (Text.null close) && open /= close
 where
  pair expectedOpen expectedClose =
    open == Text.pack expectedOpen && close == Text.pack expectedClose

validateLayouts
  :: DelimitedGroup document -> Either DelimiterInvariantError ()
validateLayouts group = case delimitedAllowedLayouts group of
  [] -> Left MissingDelimiterLayout
  layouts -> firstDuplicate DuplicateDelimiterLayout layouts

validateIds
  :: DelimiterSequence document -> Either DelimiterInvariantError ()
validateIds sequence' = do
  firstDuplicate DuplicateDelimiterChildId childIds
  firstDuplicate DuplicateDelimiterSeparatorId separatorIds
  firstDuplicate DuplicateDelimiterBoundaryId boundaryIds
  childIds `forM_` validateChildGroup
  separatorIds `forM_` validateSeparatorGroup
  boundaryIds `forM_` validateBoundaryGroup
 where
  groupId = delimiterSequenceId sequence'
  childIds = delimiterChildId <$> delimiterSequenceChildren sequence'
  separatorIds = delimiterSeparatorId <$> delimiterSequenceSeparators sequence'
  boundaryIds = delimiterBoundaryId <$> delimiterSequenceBoundaries sequence'
  validateChildGroup childId@(DelimiterChildId actual _) =
    if actual == groupId then Right () else Left $ ForeignDelimiterChildId childId
  validateSeparatorGroup separatorId@(DelimiterSeparatorId actual _) =
    if actual == groupId
      then Right ()
      else Left $ ForeignDelimiterSeparatorId separatorId
  validateBoundaryGroup boundaryId@(DelimiterBoundaryId actual _) =
    if actual == groupId
      then Right ()
      else Left $ ForeignDelimiterBoundaryId boundaryId

firstDuplicate
  :: Ord value
  => (value -> DelimiterInvariantError)
  -> [value]
  -> Either DelimiterInvariantError ()
firstDuplicate constructor values = case
    [ value
    | group'@(value : _) <- List.group $ List.sort values
    , length group' > 1
    ] of
  duplicate : _ -> Left $ constructor duplicate
  [] -> Right ()

validateSeparators
  :: DelimiterSequence document -> Either DelimiterInvariantError ()
validateSeparators sequence'
  | actual /= expected = Left $ InvalidDelimiterSeparatorCount expected actual
  | otherwise = forM_
      (List.zip3 [0 :: Int ..] separators $ zip children $ drop 1 children)
      validateLink
 where
  children = delimiterSequenceChildren sequence'
  separators = delimiterSequenceSeparators sequence'
  actual = length separators
  expected = max 0 $ length children - 1
  validateLink (index, separator, (left, right))
    | delimiterSeparatorLeft separator /= delimiterChildId left
        || delimiterSeparatorRight separator /= delimiterChildId right = Left
          $ InvalidDelimiterSeparatorLink
              (delimiterSeparatorId separator)
              (delimiterChildId left)
              (delimiterChildId right)
    | delimiterSeparatorKind separator == ListComprehensionBar
        && (index /= 0 || any ((== ListComprehensionBar)
          . delimiterSeparatorKind) (drop 1 separators)) = Left
          $ IllegalListComprehensionBar $ delimiterSeparatorId separator
    | not $ validAttachment (delimiterSequenceProfile sequence') separator = Left
          $ InvalidDelimiterAttachment
              (delimiterSeparatorId separator)
              (delimiterSequenceProfile sequence')
              (delimiterSeparatorAttachment separator)
    | otherwise = Right ()
  validAttachment _ separator
    | delimiterSeparatorKind separator == RangeDelimiterOperator =
        delimiterSeparatorAttachment separator `elem`
          [AttachSeparatorLeft, AttachSeparatorEitherNonStandalone]
  validAttachment profile separator = case profile of
    LeadingDelimiterSeparators -> leadingAttachment attachment
    ImportExportDelimiter -> leadingAttachment attachment
    TightImportExportDelimiter -> leadingAttachment attachment
    ModuleExportDelimiter -> leadingAttachment attachment
    NestedIEDelimiter -> leadingAttachment attachment
    PatternInlineDelimiter -> leadingAttachment attachment
    PromotedListDelimiter -> leadingAttachment attachment
    ListComprehensionDelimiter -> leadingAttachment attachment
    TypeDelimiterSeparators -> leadingAttachment attachment
    RecordDelimiterFields -> leadingAttachment attachment
    TrailingDelimiterSeparators -> attachment `elem`
      [AttachSeparatorLeft, AttachSeparatorEitherNonStandalone]
    BlockDelimiterChild -> False
    PatternBlockDelimiterChild -> False
    TypeBlockDelimiterChild -> False
   where
    attachment = delimiterSeparatorAttachment separator
  leadingAttachment value = value `elem`
    [AttachSeparatorRight, AttachSeparatorEitherNonStandalone]

validateBoundaries
  :: DelimiterSequence document -> Either DelimiterInvariantError ()
validateBoundaries sequence'
  | actual /= expected = Left $ InvalidDelimiterBoundaryCount expected actual
  | otherwise = forM_ (zip boundaries expectedLinks) validateLink
 where
  groupId = delimiterSequenceId sequence'
  boundaries = delimiterSequenceBoundaries sequence'
  elements = delimiterElements groupId
    (delimiterSequenceChildren sequence')
    (delimiterSequenceSeparators sequence')
  expectedLinks = zip elements $ drop 1 elements
  actual = length boundaries
  expected = length expectedLinks
  validateLink (boundary, (left, right))
    | delimiterBoundaryLeft boundary == left
    , delimiterBoundaryRight boundary == right = Right ()
    | otherwise = Left $ InvalidDelimiterBoundaryLink
        (delimiterBoundaryId boundary) left right

validateSelection
  :: DelimitedGroup document -> Either DelimiterInvariantError ()
validateSelection group = case delimitedSelection group of
  UnselectedDelimiter -> Right ()
  SelectedDelimiter layout _
    | layout `elem` delimitedAllowedLayouts group -> Right ()
    | otherwise -> Left $ SelectedDelimiterLayoutNotAllowed layout

activeDelimitedDocuments :: DelimitedGroup document -> [document]
activeDelimitedDocuments group = case delimitedSelection group of
  SelectedDelimiter _ document -> [document]
  UnselectedDelimiter -> delimiterChildDocument
    <$> delimiterSequenceChildren (delimitedSequence group)

mapDelimitedGroup
  :: (left -> right) -> DelimitedGroup left -> DelimitedGroup right
mapDelimitedGroup transform group = DelimitedGroup
  { delimitedSequence = sequence'
      { delimiterSequenceChildren = mapChild
          <$> delimiterSequenceChildren sequence'
      }
  , delimitedAllowedLayouts = delimitedAllowedLayouts group
  , delimitedSelection = case delimitedSelection group of
      UnselectedDelimiter -> UnselectedDelimiter
      SelectedDelimiter layout document ->
        SelectedDelimiter layout $ transform document
  }
 where
  sequence' = delimitedSequence group
  mapChild child = DelimiterChild
    { delimiterChildId = delimiterChildId child
    , delimiterChildOwner = delimiterChildOwner child
    , delimiterChildKind = delimiterChildKind child
    , delimiterChildDocument = transform $ delimiterChildDocument child
    }

replaceDelimitedDocuments
  :: [document] -> DelimitedGroup document -> DelimitedGroup document
replaceDelimitedDocuments documents group = case delimitedSelection group of
  SelectedDelimiter layout _ -> case documents of
    document : _ -> group
      { delimitedSelection = SelectedDelimiter layout document }
    [] -> group
  UnselectedDelimiter -> group
    { delimitedSequence = sequence'
      { delimiterSequenceChildren = zipWith replace children documents }
    }
 where
  sequence' = delimitedSequence group
  children = delimiterSequenceChildren sequence'
  replace child document = child { delimiterChildDocument = document }

selectDelimitedDocument
  :: DelimiterLayout -> document -> DelimitedGroup document
  -> DelimitedGroup document
selectDelimitedDocument layout document group = group
  { delimitedSelection = SelectedDelimiter layout document }

selectedDelimiterDocument
  :: DelimitedGroup document
  -> Either DelimiterInvariantError (DelimiterLayout, document)
selectedDelimiterDocument group = case delimitedSelection group of
  UnselectedDelimiter -> Left DelimiterLayoutNotSelected
  SelectedDelimiter layout document -> Right (layout, document)
