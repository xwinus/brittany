{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.ExactSource
  ( nodeSourceSlice
  , nodeSourceFragment
  , sourceCommentKeys
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified GHC
import GHC.Parser.Annotation (getLocA)
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.ExactPrintCompat
  ( Anns
  , commentIdentifier
  )
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as EP
import Language.Haskell.Brittany.Internal.LayouterBasics
  ( extractAllComments
  , isRegularComment
  )
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types (SourceCommentKey(..))

nodeSourceSlice
  :: Text.Text -> GHC.Located ast -> Anns -> Maybe Text.Text
nodeSourceSlice source node anns = do
  nodeSpan <- EP.srcSpanToRealSpan $ getLocA node
  let
    commentSpans =
      [ span'
      | annotation <- Map.elems anns
      , comment <- extractAllComments annotation
      , isRegularComment comment
      , Just span' <- [EP.srcSpanToRealSpan $ commentIdentifier $ fst comment]
      ]
    spans = nodeSpan : commentSpans
    firstLine = minimum $ GHC.srcSpanStartLine <$> spans
    lastLine = maximum $ GHC.srcSpanEndLine <$> spans
    selectedLines =
      take (lastLine - firstLine + 1)
        $ drop (firstLine - 1)
        $ Text.lines source
  guard $ not $ null selectedLines
  pure $ Text.intercalate (Text.singleton '\n') selectedLines

nodeSourceFragment :: Text.Text -> GHC.Located ast -> Maybe Text.Text
nodeSourceFragment source node = do
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
    [line] -> pure $ Text.take (endColumn - startColumn)
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
        pure $ Text.intercalate (Text.singleton '\n')
          $ Text.drop sourceIndent firstLine
          : fmap rebase continuationLines

sourceCommentKeys
  :: GHC.Located ast -> Anns -> Set.Set SourceCommentKey
sourceCommentKeys node anns = case EP.srcSpanToRealSpan $ getLocA node of
  Nothing -> Set.empty
  Just nodeSpan -> Set.fromList
    [ SourceCommentKey $ commentIdentifier $ fst comment
    | annotation <- Map.elems anns
    , comment <- extractAllComments annotation
    , isRegularComment comment
    , Just commentSpan <-
        [EP.srcSpanToRealSpan $ commentIdentifier $ fst comment]
    , SrcLoc.realSrcSpanStart nodeSpan <= SrcLoc.realSrcSpanStart commentSpan
    , SrcLoc.realSrcSpanEnd commentSpan <= SrcLoc.realSrcSpanEnd nodeSpan
    ]

dropLeadingSpaces :: Int -> Text.Text -> Text.Text
dropLeadingSpaces count line = Text.drop removable line
 where
  removable = min count $ leadingSpaceCount line

leadingSpaceCount :: Text.Text -> Int
leadingSpaceCount = Text.length . Text.takeWhile (== ' ')
