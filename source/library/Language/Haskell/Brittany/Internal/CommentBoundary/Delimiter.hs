{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.CommentBoundary.Delimiter
  ( delimiterBoundary
  ) where

import qualified Data.Generics as Generics
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import GHC (GenLocated(L), unLoc)
import GHC.Hs
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.ExactPrintCompat (srcSpanToRealSpan)
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types

data DelimiterRegion = DelimiterRegion
  { regionSpan :: SrcLoc.RealSrcSpan
  , regionChildren :: [SrcLoc.RealSrcSpan]
  }

delimiterBoundary
  :: HsModule GhcPs
  -> SrcLoc.RealSrcSpan
  -> Maybe CommentBoundaryId
delimiterBoundary module' commentSpan = do
  (index, matchingRegion) <- smallestContainingRegion commentSpan indexedRegions
  pure $ CommentBoundaryId
    (DelimiterBoundaryPath index)
    (regionGap commentSpan matchingRegion)
 where
  regions = List.sortOn regionOrder $ delimiterRegions module'
  indexedRegions = zip [0 ..] regions

delimiterRegions :: HsModule GhcPs -> [DelimiterRegion]
delimiterRegions = Generics.everything (++) query
 where
  query :: Generics.GenericQ [DelimiterRegion]
  query = Generics.mkQ [] expressionRegion
    `Generics.extQ` typeRegion
    `Generics.extQ` patternRegion
    `Generics.extQ` ieListRegion

expressionRegion :: LHsExpr GhcPs -> [DelimiterRegion]
expressionRegion expression@(L _ value) = case value of
  HsPar _ child -> region expression [child]
  ExplicitList _ children -> region expression children
  ExplicitTuple _ arguments _ -> region expression
    [ child | Present _ child <- arguments ]
  RecordCon _ _ (HsRecFields _ fields _) -> region expression fields
  RecordUpd _ _ (RegularRecUpdFields _ fields) -> region expression fields
  HsDo _ flavour (L _ statements) -> case flavour of
    ListComp -> region expression statements
    MonadComp -> region expression statements
    _ -> []
  _ -> []

typeRegion :: LHsType GhcPs -> [DelimiterRegion]
typeRegion type'@(L _ value) = case value of
  HsParTy _ child -> region type' [child]
  HsListTy _ child -> region type' [child]
  HsTupleTy _ _ children -> region type' children
  HsExplicitListTy _ _ children -> region type' children
  HsExplicitTupleTy _ _ children -> region type' children
  _ -> []

patternRegion :: LPat GhcPs -> [DelimiterRegion]
patternRegion pattern'@(L _ value) = case value of
  ParPat _ child -> region pattern' [child]
  ListPat _ children -> region pattern' children
  TuplePat _ children _ -> region pattern' children
  ConPat _ _ (RecCon (HsRecFields _ fields _)) -> region pattern' fields
  _ -> []

ieListRegion :: XRec GhcPs [LIE GhcPs] -> [DelimiterRegion]
ieListRegion list' = region list' $ unLoc list'

region
  :: (HasLoc outerLocation, HasLoc childLocation)
  => GenLocated outerLocation outer
  -> [GenLocated childLocation child]
  -> [DelimiterRegion]
region outer children = case locatedSpan outer of
  Nothing -> []
  Just outerSpan -> [DelimiterRegion
    { regionSpan = outerSpan
    , regionChildren = List.sortOn spanStart $ Maybe.mapMaybe locatedSpan children
    }]

smallestContainingRegion
  :: SrcLoc.RealSrcSpan
  -> [(Int, DelimiterRegion)]
  -> Maybe (Int, DelimiterRegion)
smallestContainingRegion commentSpan = listToMaybe
  . List.sortOn (regionSize . regionSpan . snd)
  . filter (contains commentSpan . regionSpan . snd)

regionGap :: SrcLoc.RealSrcSpan -> DelimiterRegion -> CommentBoundaryGap
regionGap commentSpan region' = case regionChildren region' of
  [] -> AfterOpenBoundary
  firstChild : remainingChildren
    | spanEnd commentSpan <= spanStart firstChild -> AfterOpenBoundary
    | spanStart commentSpan >= spanEnd lastChild -> BeforeCloseBoundary
    | any (contains commentSpan) children -> WithinBoundary
    | otherwise -> BetweenBoundary
   where
    children = firstChild : remainingChildren
    lastChild = foldl (const id) firstChild remainingChildren

regionOrder :: DelimiterRegion -> ((Int, Int), (Int, Int))
regionOrder region' = (spanStart span', spanEnd span')
 where
  span' = regionSpan region'

regionSize :: SrcLoc.RealSrcSpan -> (Int, Int)
regionSize span' =
  ( SrcLoc.srcSpanEndLine span' - SrcLoc.srcSpanStartLine span'
  , SrcLoc.srcSpanEndCol span' - SrcLoc.srcSpanStartCol span'
  )

locatedSpan
  :: HasLoc location
  => GenLocated location value
  -> Maybe SrcLoc.RealSrcSpan
locatedSpan value = srcSpanToRealSpan $ getLocA value

contains :: SrcLoc.RealSrcSpan -> SrcLoc.RealSrcSpan -> Bool
contains child parent =
  spanStart child >= spanStart parent
    && spanStart child < spanEnd parent
    && spanEnd child <= spanEnd parent

spanStart :: SrcLoc.RealSrcSpan -> (Int, Int)
spanStart span' =
  (SrcLoc.srcSpanStartLine span', SrcLoc.srcSpanStartCol span')

spanEnd :: SrcLoc.RealSrcSpan -> (Int, Int)
spanEnd span' = (SrcLoc.srcSpanEndLine span', SrcLoc.srcSpanEndCol span')
