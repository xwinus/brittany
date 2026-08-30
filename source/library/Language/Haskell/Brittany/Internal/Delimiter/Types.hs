{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Delimiter.Types
  ( DelimiterKind(..)
  , DelimiterLayout(..)
  , DelimiterIndent(..)
  , DelimiterBoundaryRef(..)
  , DelimiterSpec(..)
  , DelimitedAlternative(..)
  , DelimitedGroup(..)
  , DelimiterInvariantError(..)
  , mkDelimiterSpec
  , validateDelimitedGroup
  , mapDelimitedGroup
  , replaceDelimitedDocuments
  , selectedDelimiterAlternative
  ) where

import qualified Data.Data as Data
import qualified Data.Text as Text
import Language.Haskell.Brittany.Internal.ExactPrintCompat (AnnKey)
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types
  ( CommentBoundaryGap(..) )

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
  | DelimiterVertical
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterIndent
  = DelimiterIndentNone
  | DelimiterIndentRegular
  | DelimiterIndentFixed Int
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterBoundaryRef = DelimiterBoundaryRef
  { delimiterBoundaryOwner :: Maybe AnnKey
  , delimiterBoundaryGap :: CommentBoundaryGap
  }
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterSpec = DelimiterSpec
  { delimiterKind :: DelimiterKind
  , delimiterOpenToken :: Text
  , delimiterCloseToken :: Text
  , delimiterSourceOwner :: Maybe AnnKey
  , delimiterAfterOpen :: DelimiterBoundaryRef
  , delimiterBeforeClose :: DelimiterBoundaryRef
  , delimiterChildren :: [Maybe AnnKey]
  , delimiterSeparators :: [Text]
  , delimiterIndent :: DelimiterIndent
  , delimiterAllowsHanging :: Bool
  }
  deriving (Data.Data, Eq, Ord, Show)

data DelimitedAlternative document = DelimitedAlternative
  { delimitedAlternativeLayout :: DelimiterLayout
  , delimitedAlternativeDocument :: document
  }
  deriving (Data.Data, Eq, Ord, Show)

data DelimitedGroup document = DelimitedGroup
  { delimitedSpec :: DelimiterSpec
  , delimitedAlternatives :: [DelimitedAlternative document]
  }
  deriving (Data.Data, Eq, Ord, Show)

data DelimiterInvariantError
  = InvalidDelimiterTokens DelimiterKind Text Text
  | InvalidDelimiterBoundary CommentBoundaryGap CommentBoundaryGap
  | InvalidDelimiterSeparatorCount Int Int
  | MissingDelimiterAlternative
  | DuplicateDelimiterLayout DelimiterLayout
  | UnselectedDelimiterAlternatives Int
  | AccidentalStandaloneDelimiter DelimiterLayout Text
  deriving (Data.Data, Eq, Ord, Show)

mkDelimiterSpec
  :: DelimiterKind
  -> Text
  -> Text
  -> Maybe AnnKey
  -> [Maybe AnnKey]
  -> [Text]
  -> DelimiterSpec
mkDelimiterSpec kind open close owner children separators = DelimiterSpec
  { delimiterKind = kind
  , delimiterOpenToken = open
  , delimiterCloseToken = close
  , delimiterSourceOwner = owner
  , delimiterAfterOpen = DelimiterBoundaryRef owner AfterOpenBoundary
  , delimiterBeforeClose = DelimiterBoundaryRef owner BeforeCloseBoundary
  , delimiterChildren = children
  , delimiterSeparators = separators
  , delimiterIndent = DelimiterIndentRegular
  , delimiterAllowsHanging = True
  }

validateDelimitedGroup
  :: DelimitedGroup document -> Either DelimiterInvariantError ()
validateDelimitedGroup group = do
  validateTokens spec
  validateBoundaries spec
  validateSeparators spec
  validateAlternatives $ delimitedAlternatives group
 where
  spec = delimitedSpec group

validateTokens :: DelimiterSpec -> Either DelimiterInvariantError ()
validateTokens spec
  | validTokenPair (delimiterKind spec)
      (delimiterOpenToken spec)
      (delimiterCloseToken spec) = Right ()
  | otherwise = Left $ InvalidDelimiterTokens
      (delimiterKind spec)
      (delimiterOpenToken spec)
      (delimiterCloseToken spec)

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

validateBoundaries :: DelimiterSpec -> Either DelimiterInvariantError ()
validateBoundaries spec
  | afterGap == AfterOpenBoundary
  , beforeGap == BeforeCloseBoundary = Right ()
  | otherwise = Left $ InvalidDelimiterBoundary afterGap beforeGap
 where
  afterGap = delimiterBoundaryGap $ delimiterAfterOpen spec
  beforeGap = delimiterBoundaryGap $ delimiterBeforeClose spec

validateSeparators :: DelimiterSpec -> Either DelimiterInvariantError ()
validateSeparators spec
  | actual == expected = Right ()
  | otherwise = Left $ InvalidDelimiterSeparatorCount expected actual
 where
  actual = length $ delimiterSeparators spec
  expected = max 0 $ length (delimiterChildren spec) - 1

validateAlternatives
  :: [DelimitedAlternative document] -> Either DelimiterInvariantError ()
validateAlternatives [] = Left MissingDelimiterAlternative
validateAlternatives alternatives = go [] $ delimitedAlternativeLayout <$> alternatives
 where
  go _ [] = Right ()
  go seen (layout : remaining)
    | layout `elem` seen = Left $ DuplicateDelimiterLayout layout
    | otherwise = go (seen ++ [layout]) remaining

mapDelimitedGroup
  :: (left -> right) -> DelimitedGroup left -> DelimitedGroup right
mapDelimitedGroup transform group = group
  { delimitedAlternatives = fmap mapAlternative $ delimitedAlternatives group }
 where
  mapAlternative alternative = alternative
    { delimitedAlternativeDocument =
        transform $ delimitedAlternativeDocument alternative }

replaceDelimitedDocuments
  :: [right] -> DelimitedGroup left -> DelimitedGroup right
replaceDelimitedDocuments documents group = DelimitedGroup
  { delimitedSpec = delimitedSpec group
  , delimitedAlternatives = zipWith replace
      (delimitedAlternatives group)
      documents
  }
 where
  replace alternative document = DelimitedAlternative
    { delimitedAlternativeLayout = delimitedAlternativeLayout alternative
    , delimitedAlternativeDocument = document
    }

selectedDelimiterAlternative
  :: DelimitedGroup document
  -> Either DelimiterInvariantError (DelimitedAlternative document)
selectedDelimiterAlternative group = case delimitedAlternatives group of
  [alternative] -> Right alternative
  alternatives -> Left $ UnselectedDelimiterAlternatives $ length alternatives
