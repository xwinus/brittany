{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.Layouters.Pattern.Types
  ( PatternLayout(..)
  , colsWrapPat
  , patternCompactDocument
  , patternDocument
  ) where

import qualified Data.Foldable                            as Foldable
import           Data.Kind                                ( Type )
import qualified Data.Sequence                            as Seq
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Prelude
import           Language.Haskell.Brittany.Internal.Types

type PatternLayout :: Type
data PatternLayout = PatternLayout
  { patternCompactColumns     :: Seq.Seq BriDocNumbered
  , patternStructuralDocument :: Maybe BriDocNumbered
  }

colsWrapPat :: Seq.Seq BriDocNumbered -> ToBriDocM BriDocNumbered
colsWrapPat = docCols ColPatterns . fmap pure . Foldable.toList

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
