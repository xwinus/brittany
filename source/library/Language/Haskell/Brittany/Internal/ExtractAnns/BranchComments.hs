{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.ExtractAnns.BranchComments
  ( branchCommentAnnotation
  ) where

import qualified Data.List as List
import GHC.Hs (LHsExpr)
import GHC.Parser.Annotation (getLocA)
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.ExactPrintCompat
import Language.Haskell.Brittany.Internal.Prelude

branchCommentAnnotation
  :: Maybe (Int, Int)
  -> [((Int, Int), (String, SrcLoc.RealSrcSpan))]
  -> LHsExpr GhcPs
  -> Maybe Annotation
branchCommentAnnotation _ [] _ = Nothing
branchCommentAnnotation keywordPosition comments expression = do
  expressionSpan <- srcSpanToRealSpan $ getLocA expression
  let start = spanStart expressionSpan
      end = spanEnd expressionSpan
      (following, preceding) = List.partition ((>= end) . fst) comments
      initial = fromMaybe start keywordPosition
      priors = snd $ List.mapAccumL (commentDelta True) initial preceding
      follows = snd $ List.mapAccumL (commentDelta False) end following
      entryDelta = case preceding of
        [] -> DP (0, 0)
        _ -> positionDelta (spanEnd $ snd $ snd $ List.last preceding) start
  pure Ann
    { annCapturedSpan = Nothing
    , annSortKey = Nothing
    , annsDP = []
    , annFollowingComments = follows
    , annPriorComments = priors
    , annEntryDelta = entryDelta
    }

-- Leading comments retain the keyword-relative layout. Trailing comments use
-- the expression end, so an inline note stays beside the expression it explains.
commentDelta
  :: Bool
  -> (Int, Int)
  -> ((Int, Int), (String, SrcLoc.RealSrcSpan))
  -> ((Int, Int), (Comment, DeltaPos))
commentDelta leading previous (position, (content, sourceSpan)) =
  ( spanEnd sourceSpan
  , ( Comment
        { commentOrigin = Nothing
        , commentIdentifier = realSpanToSrcSpan sourceSpan
        , commentContents = content
        }
    , if leading
        then DP (if fst position == fst previous then 0
          else max 1 $ fst position - fst previous, 0)
        else positionDelta previous position
    )
  )

positionDelta :: (Int, Int) -> (Int, Int) -> DeltaPos
positionDelta (previousLine, previousColumn) (line, column)
  | previousLine == line = DP (0, column - previousColumn)
  | otherwise = DP (line - previousLine, column - 1)

spanStart :: SrcLoc.RealSrcSpan -> (Int, Int)
spanStart sourceSpan =
  (SrcLoc.srcSpanStartLine sourceSpan, SrcLoc.srcSpanStartCol sourceSpan)

spanEnd :: SrcLoc.RealSrcSpan -> (Int, Int)
spanEnd sourceSpan =
  (SrcLoc.srcSpanEndLine sourceSpan, SrcLoc.srcSpanEndCol sourceSpan)
