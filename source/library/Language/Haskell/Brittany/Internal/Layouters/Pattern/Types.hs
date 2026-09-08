{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.Layouters.Pattern.Types
  ( PatternLayout(..)
  , colsWrapPat
  , patternCompactDocument
  , patternDocument
  , omitPatternTrailingLineBreak
  ) where

import qualified Data.Foldable                            as Foldable
import           Data.Kind                                ( Type )
import qualified Data.Sequence                            as Seq
import           Language.Haskell.Brittany.Internal.Delimiter.Types
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Prelude
import           Language.Haskell.Brittany.Internal.Types

type PatternLayout :: Type
data PatternLayout = PatternLayout
  { patternCompactColumns     :: Seq.Seq BriDocNumbered
  , patternStructuralDocument :: Maybe BriDocNumbered
  }

colsWrapPat :: Seq.Seq BriDocNumbered -> ToBriDocM BriDocNumbered
colsWrapPat documents = case Foldable.toList documents of
  [document@(_, BDFDelimited group)]
    | delimiterSequenceProfile (delimitedSequence group)
        == PatternInlineDelimiter -> pure document
  flattened -> docCols ColPatterns $ pure <$> flattened

patternCompactDocument :: PatternLayout -> ToBriDocM BriDocNumbered
patternCompactDocument = colsWrapPat . patternCompactColumns

patternDocument :: PatternLayout -> ToBriDocM BriDocNumbered
patternDocument layout = do
  compactDocument <- patternCompactDocument layout
  case patternStructuralDocument layout of
    Nothing -> pure compactDocument
    Just structuralDocument -> docAlt
      [ docForceSingleline $ pure compactDocument
      , pure structuralDocument
      ]

-- A commented invisible pattern terminates its compact document explicitly.
-- The parent argument list supplies that boundary when another line follows.
omitPatternTrailingLineBreak :: BriDocNumbered -> ToBriDocM BriDocNumbered
omitPatternTrailingLineBreak document = fromMaybe document <$> removeBreak document
 where
  removeBreak :: BriDocNumbered -> ToBriDocM (Maybe BriDocNumbered)
  removeBreak (_, BDFLines [commented, (_, BDFBlankLine)]) =
    pure $ Just commented
  removeBreak (_, BDFCols signature documents) =
    replaceLast (BDFCols signature) documents
  removeBreak (_, BDFSeq documents) = replaceLast BDFSeq documents
  removeBreak (_, BDFAnnotationPrior mode key child) =
    replaceChild (BDFAnnotationPrior mode key) child
  removeBreak (_, BDFAnnotationRest key child) =
    replaceChild (BDFAnnotationRest key) child
  removeBreak (_, BDFAnnotationKW key keyword child) =
    replaceChild (BDFAnnotationKW key keyword) child
  removeBreak _ = pure Nothing

  replaceChild wrap child = do
    replacement <- removeBreak child
    traverse (allocateNode . wrap) replacement

  replaceLast wrap documents = case reverse documents of
    [] -> pure Nothing
    lastDocument : preceding -> do
      replacement <- removeBreak lastDocument
      traverse (allocateNode . wrap . reverse . (: preceding)) replacement
