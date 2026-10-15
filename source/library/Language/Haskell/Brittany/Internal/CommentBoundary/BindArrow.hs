{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.CommentBoundary.BindArrow
  ( BindArrowRegion(..)
  , BindArrowIndex
  , bindArrowRegion
  , buildBindArrowIndex
  , bindArrowBoundary
  , preArrowComment
  , postArrowComment
  , materializeBindArrowComments
  ) where

import qualified Data.Generics as Generics
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC (GenLocated(L), HsModule)
import GHC.Hs (StmtLR(..), Stmt, LHsExpr)
import GHC.Parser.Annotation (getHasLoc, getLocA)
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.ExactPrintCompat
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types

data BindArrowRegion = BindArrowRegion
  { bindPatternSpan :: SrcLoc.RealSrcSpan
  , bindArrowSpan :: SrcLoc.RealSrcSpan
  , bindRhsSpan :: SrcLoc.RealSrcSpan
  , bindRhsOwner :: AnnKey
  }

newtype BindArrowIndex = BindArrowIndex [(Int, BindArrowRegion)]

bindArrowRegion :: Stmt GhcPs (LHsExpr GhcPs) -> Maybe BindArrowRegion
bindArrowRegion (BindStmt arrow pattern' rhs@(L _ expression)) = do
  patternSpan <- srcSpanToRealSpan $ getLocA pattern'
  arrowSpan <- srcSpanToRealSpan $ getHasLoc arrow
  rhsSpan <- srcSpanToRealSpan $ getLocA rhs
  pure $ BindArrowRegion patternSpan arrowSpan rhsSpan
    (mkAnnKey $ L (getLocA rhs) expression)
bindArrowRegion _ = Nothing

buildBindArrowIndex :: HsModule GhcPs -> BindArrowIndex
buildBindArrowIndex = BindArrowIndex . zip [0 ..]
  . List.sortOn (spanStart . bindArrowSpan)
  . Generics.everything (++) (Generics.mkQ [] (maybeToList . bindArrowRegion))

bindArrowBoundary :: BindArrowIndex -> SrcLoc.RealSrcSpan -> Maybe CommentBoundaryId
bindArrowBoundary (BindArrowIndex regions) commentSpan = listToMaybe
  [ CommentBoundaryId (BindArrowBoundaryPath index) gap
  | (index, region) <- regions
  , gap <- if preArrowComment region commentSpan
      then [BeforeBoundary]
      else [AfterOpenBoundary | postArrowComment region commentSpan]
  ]

preArrowComment :: BindArrowRegion -> SrcLoc.RealSrcSpan -> Bool
preArrowComment region = between (bindPatternSpan region) (bindArrowSpan region)

postArrowComment :: BindArrowRegion -> SrcLoc.RealSrcSpan -> Bool
postArrowComment region = between (bindArrowSpan region) (bindRhsSpan region)

between :: SrcLoc.RealSrcSpan -> SrcLoc.RealSrcSpan -> SrcLoc.RealSrcSpan -> Bool
between left right commentSpan =
  SrcLoc.srcSpanFile left == SrcLoc.srcSpanFile commentSpan
    && spanEnd left <= spanStart commentSpan
    && spanEnd commentSpan <= spanStart right

-- Punctuation is not an AST child. Move only comments in the arrow/RHS gap
-- away from the preceding pattern before generic annotation lowering sees it.
materializeBindArrowComments :: HsModule GhcPs -> Anns -> Anns
materializeBindArrowComments module' annotations =
  foldl relocate annotations regions
 where
  BindArrowIndex regions = buildBindArrowIndex module'
  allEntries = List.nubBy
    (\left right -> commentIdentifier (fst left) == commentIdentifier (fst right))
    $ List.sortOn (spanStart . snd)
    [ (comment, sourceSpan)
    | annotation <- Map.elems annotations
    , comment <- fmap fst (annPriorComments annotation)
        ++ fmap fst (annFollowingComments annotation)
        ++ [source | (AnnComment source, _) <- annsDP annotation]
    , Just sourceSpan <- [srcSpanToRealSpan $ commentIdentifier comment]
    ]
  relocate current (_, region)
    | null entries = current
    | otherwise = Map.alter
        (Just . attach . fromMaybe emptyAnnotation)
        (bindRhsOwner region) $ Map.map remove current
   where
    entries = filter (postArrowComment region . snd) allEntries
    comments = fst <$> entries
    keys = Set.fromList $ SourceCommentKey . commentIdentifier <$> comments
    keep comment = Set.notMember (SourceCommentKey $ commentIdentifier comment) keys
    remove annotation = annotation
      { annPriorComments = filter (keep . fst) $ annPriorComments annotation
      , annFollowingComments = filter (keep . fst) $ annFollowingComments annotation
      , annsDP = filter (\(keyword, _) -> case keyword of
          AnnComment comment -> keep comment
          _ -> True) $ annsDP annotation
      }
    attach annotation = annotation
      { annPriorComments = snd (mapAccumL rebase (spanEnd $ bindArrowSpan region) entries)
          ++ annPriorComments annotation }
    rebase previous (comment, currentSpan) =
      let (line, column) = spanStart currentSpan
          (previousLine, previousColumn) = previous
          delta = if line == previousLine then DP (0, column - previousColumn)
            else DP (line - previousLine, column - 1)
      in (spanEnd currentSpan, (comment, delta))

emptyAnnotation :: Annotation
emptyAnnotation = Ann Nothing Nothing [] [] [] (DP (0, 0))

spanStart :: SrcLoc.RealSrcSpan -> (Int, Int)
spanStart span' = (SrcLoc.srcSpanStartLine span', SrcLoc.srcSpanStartCol span')

spanEnd :: SrcLoc.RealSrcSpan -> (Int, Int)
spanEnd span' = (SrcLoc.srcSpanEndLine span', SrcLoc.srcSpanEndCol span')
