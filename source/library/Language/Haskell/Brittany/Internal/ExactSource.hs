{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.ExactSource
  ( nodeSourceSlice
  , nodeSourceFragment
  , sourceCommentKeys
  , sourceCommentFragment
  , sourceRangeContainsComment
  , validateExactSourceFragment
  ) where

import Data.Data (Data)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified GHC
import qualified GHC.Data.FastString as FastString
import GHC.Parser.Annotation (getLocA)
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.ExactPrintCompat
  ( Anns
  , commentIdentifier
  )
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as EP
import Language.Haskell.Brittany.Internal.ExactPrintUtils (foldedAnnKeys)
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types

nodeSourceSlice
  :: Data ast
  => Text.Text
  -> GHC.Located ast
  -> Anns
  -> CommentPlan
  -> Maybe ExactSourceFragment
nodeSourceSlice source node anns commentPlan = do
  nodeSpan <- EP.srcSpanToRealSpan $ getLocA node
  let
    commentSpans =
      [ span'
      | annotation <- Map.elems anns
      , comment <- annotationComments annotation
      , Just span' <- [EP.srcSpanToRealSpan $ commentIdentifier $ fst comment]
      ]
    spans = nodeSpan : commentSpans
    firstLine = minimum $ GHC.srcSpanStartLine <$> spans
    lastLine = maximum $ GHC.srcSpanEndLine <$> spans
    selectedLines =
      take (lastLine - firstLine + 1)
        $ drop (firstLine - 1)
        $ Text.splitOn (Text.singleton '\n') source
  guard $ not $ null selectedLines
  let lastColumn = maybe 1 ((+ 1) . Text.length)
        $ Maybe.listToMaybe $ reverse selectedLines
      range = SourceRange
        (FastString.unpackFS $ SrcLoc.srcSpanFile nodeSpan)
        firstLine
        1
        lastLine
        lastColumn
  pure $ exactSourceFragment node anns commentPlan range
    $ Text.intercalate (Text.singleton '\n') selectedLines

nodeSourceFragment
  :: Data ast
  => Text.Text
  -> GHC.Located ast
  -> Anns
  -> CommentPlan
  -> Maybe ExactSourceFragment
nodeSourceFragment source node anns commentPlan = do
  nodeSpan <- EP.srcSpanToRealSpan $ getLocA node
  let startLine = GHC.srcSpanStartLine nodeSpan
      endLine = GHC.srcSpanEndLine nodeSpan
      startColumn = GHC.srcSpanStartCol nodeSpan
      endColumn = GHC.srcSpanEndCol nodeSpan
      selectedLines = take (endLine - startLine + 1)
        $ drop (startLine - 1)
        $ Text.splitOn (Text.singleton '\n') source
  case selectedLines of
    [] -> Nothing
    [line] -> pure $ exactSourceFragment node anns commentPlan (rangeFromSpan nodeSpan)
      $ Text.take (endColumn - startColumn)
      $ Text.drop (startColumn - 1) line
    firstLine : remainingLines -> case reverse remainingLines of
      [] -> Nothing
      lastLineSource : reversedMiddleLines -> do
        let lastLine = Text.take (endColumn - 1) lastLineSource
            middleLines = reverse reversedMiddleLines
            sourceIndent = startColumn - 1
            continuationLines = middleLines ++ [lastLine]
            continuationIndent = minimum
              $ sourceIndent
              : [ leadingSpaceCount line
                | line <- continuationLines
                , not $ Text.null $ Text.strip line
                ]
            rebase = dropLeadingSpaces continuationIndent
        pure $ exactSourceFragment node anns commentPlan (rangeFromSpan nodeSpan)
          $ Text.intercalate (Text.singleton '\n')
          $ Text.drop sourceIndent firstLine
          : fmap rebase continuationLines

sourceCommentFragment :: SourceComment -> ExactSourceFragment
sourceCommentFragment sourceComment = ExactSourceFragment
  { fragmentText = sourceCommentText sourceComment
  , fragmentRange = rangeFromSpan $ sourceCommentSpan sourceComment
  , fragmentAnnotationKeys = Set.empty
  , fragmentCommentKeys = Set.singleton $ sourceCommentKey sourceComment
  , fragmentAbsoluteColumn = Nothing
  }

sourceCommentKeys
  :: GHC.Located ast -> CommentPlan -> Set.Set SourceCommentKey
sourceCommentKeys node commentPlan = case EP.srcSpanToRealSpan $ getLocA node of
  Nothing -> Set.empty
  Just nodeSpan -> commentKeysInRange (rangeFromSpan nodeSpan) commentPlan

sourceRangeContainsComment :: SourceRange -> SourceCommentKey -> Bool
sourceRangeContainsComment range (SourceCommentKey span') = case
  EP.srcSpanToRealSpan span' of
    Nothing -> False
    Just realSpan -> rangeContainsSpan range realSpan

validateExactSourceFragment :: ExactSourceFragment -> Either String ()
validateExactSourceFragment fragment = case filter
  (not . sourceRangeContainsComment (fragmentRange fragment))
  (Set.toList $ fragmentCommentKeys fragment) of
    [] -> Right ()
    invalidKeys -> Left
      $ "exact-source fragment contains comment keys outside its range: "
      ++ show invalidKeys

exactSourceFragment
  :: Data ast
  => GHC.Located ast
  -> Anns
  -> CommentPlan
  -> SourceRange
  -> Text.Text
  -> ExactSourceFragment
exactSourceFragment node anns commentPlan range text = ExactSourceFragment
  { fragmentText = text
  , fragmentRange = range
  , fragmentAnnotationKeys = foldedAnnKeys node <> Set.fromList
      [ key
      | key <- Map.keys anns
      , Just keySpan <- [EP.annKeyRealSpan key]
      , rangeContainsSpan range keySpan
      ]
  , fragmentCommentKeys = commentKeysInRange range commentPlan
  , fragmentAbsoluteColumn = Nothing
  }

commentKeysInRange :: SourceRange -> CommentPlan -> Set.Set SourceCommentKey
commentKeysInRange range commentPlan = Set.fromList
    [ key
    | (key, sourceComment) <- Map.toList $ commentPlanSources commentPlan
    , let commentSpan = sourceCommentSpan sourceComment
    , rangeContainsSpan range commentSpan
    ]

annotationComments
  :: EP.Annotation -> [(EP.Comment, EP.DeltaPos)]
annotationComments annotation =
  EP.annPriorComments annotation
    ++ EP.annFollowingComments annotation
    ++ [ (comment, delta)
       | (EP.AnnComment comment, delta) <- EP.annsDP annotation
       ]

rangeFromSpan :: SrcLoc.RealSrcSpan -> SourceRange
rangeFromSpan span' = SourceRange
  (FastString.unpackFS $ SrcLoc.srcSpanFile span')
  (SrcLoc.srcSpanStartLine span')
  (SrcLoc.srcSpanStartCol span')
  (SrcLoc.srcSpanEndLine span')
  (SrcLoc.srcSpanEndCol span')

rangeContainsSpan :: SourceRange -> SrcLoc.RealSrcSpan -> Bool
rangeContainsSpan range span' =
  sourceRangeFile range == FastString.unpackFS (SrcLoc.srcSpanFile span')
    && (sourceRangeStartLine range, sourceRangeStartColumn range)
      <= (SrcLoc.srcSpanStartLine span', SrcLoc.srcSpanStartCol span')
    && (SrcLoc.srcSpanEndLine span', SrcLoc.srcSpanEndCol span')
      <= (sourceRangeEndLine range, sourceRangeEndColumn range)

dropLeadingSpaces :: Int -> Text.Text -> Text.Text
dropLeadingSpaces count line = Text.drop removable line
 where
  removable = min count $ leadingSpaceCount line

leadingSpaceCount :: Text.Text -> Int
leadingSpaceCount = Text.length . Text.takeWhile (== ' ')
