{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.LocalComments
  ( leadingLocalComments
  , prependLocalComments
  ) where

import qualified Data.List as List
import GHC (GenLocated(L))
import GHC.Types.SrcLoc
  ( RealSrcSpan, SrcSpan, srcSpanEndCol, srcSpanEndLine
  , srcSpanStartCol, srcSpanStartLine )
import Language.Haskell.Brittany.Internal.ExactPrintCompat
  ( realSpanToSrcSpan, srcSpanToRealSpan )
import Language.Haskell.Brittany.Internal.ExactSource (sourceCommentFragment)
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Types

leadingLocalComments
  :: [SourceComment] -> Maybe SrcSpan -> SrcSpan -> [SourceComment]
leadingLocalComments comments previous current = List.sortOn commentStart
  $ filter isLeading comments
 where
  isLeading comment = case srcSpanToRealSpan current of
    Just currentSpan -> commentEnd comment <= spanStart currentSpan
      && maybe True
        (\previousSpan -> srcSpanEndLine previousSpan
          < srcSpanStartLine (sourceCommentSpan comment))
        (previous >>= srcSpanToRealSpan)
    Nothing -> False

prependLocalComments
  :: [SourceComment]
  -> Maybe SrcSpan
  -> [SourceComment]
  -> (SrcSpan, BriDocNumbered)
  -> ToBriDocM BriDocNumbered
prependLocalComments comments previous leading (itemSpan, formatted) = do
  let (_, commentDocuments) = mapAccumL renderComment previousEnd leading
      lastEnd = case reverse leading of
        comment : _ -> Just $ srcSpanEndLine $ sourceCommentSpan comment
        [] -> previousEnd
      beforeItem = blankBefore lastEnd
        $ srcSpanStartLine <$> srcSpanToRealSpan itemSpan
  case List.concat commentDocuments ++ beforeItem of
    [] -> pure formatted
    preceding -> docLines $ preceding ++ [pure formatted]
 where
  previousSpan = previous >>= srcSpanToRealSpan
  trailing = filter (followsPreviousLine previousSpan) comments
  -- Only comment-bearing boundaries gain spacing; ordinary local layout stays intact.
  previousEnd
    | null leading && null trailing = Nothing
    | otherwise = case previousSpan of
        Nothing -> Nothing
        Just span' -> Just $ maximum
          $ srcSpanEndLine span'
          : (srcSpanEndLine . sourceCommentSpan <$> trailing)
  renderComment priorEnd comment =
    ( Just $ srcSpanEndLine $ sourceCommentSpan comment
    , blankBefore priorEnd (Just $ srcSpanStartLine $ sourceCommentSpan comment)
      ++ [briDocBySourceFragmentNoComment
        (L (realSpanToSrcSpan $ sourceCommentSpan comment) comment)
        (sourceCommentFragment comment)]
    )
  blankBefore :: Maybe Int -> Maybe Int -> [ToBriDocM BriDocNumbered]
  blankBefore (Just end) (Just start) | start - end > 1 = [docBlankLine]
  blankBefore _ _ = []

followsPreviousLine :: Maybe RealSrcSpan -> SourceComment -> Bool
followsPreviousLine Nothing _ = False
followsPreviousLine (Just previous) comment =
  srcSpanEndLine previous == srcSpanStartLine (sourceCommentSpan comment)
    && spanEnd previous <= commentStart comment

spanStart :: RealSrcSpan -> (Int, Int)
spanStart span' = (srcSpanStartLine span', srcSpanStartCol span')

spanEnd :: RealSrcSpan -> (Int, Int)
spanEnd span' = (srcSpanEndLine span', srcSpanEndCol span')

commentStart :: SourceComment -> (Int, Int)
commentStart = spanStart . sourceCommentSpan

commentEnd :: SourceComment -> (Int, Int)
commentEnd = spanEnd . sourceCommentSpan
