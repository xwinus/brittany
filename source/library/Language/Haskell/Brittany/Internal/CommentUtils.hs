{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.CommentUtils
  ( collectCommentContents
  , collectCommentPositions
  ) where

import qualified Data.Generics as SYB
import qualified Data.List as List
import GHC (LEpaComment, ParsedSource)
import GHC.Parser.Annotation
  ( EpAnnComments
  , epaLocationRealSrcSpan
  , getFollowingComments
  , priorComments
  )
import GHC.Types.SrcLoc (srcSpanStartCol, srcSpanStartLine)
import Language.Haskell.Brittany.Internal.Prelude
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrintTypes
import qualified Language.Haskell.GHC.ExactPrint.Utils as ExactPrintUtils

-- | Collect source comments from the GHC AST, deduplicating annotations that
-- refer to the same comment location.
collectCommentContents :: ParsedSource -> [String]
collectCommentContents parsedSource =
  List.sort $ snd <$> collectSourceComments parsedSource

collectCommentPositions :: ParsedSource -> [(Int, Int)]
collectCommentPositions = fmap fst . collectSourceComments

collectSourceComments :: ParsedSource -> [((Int, Int), String)]
collectSourceComments parsedSource = foldl addTransport []
  [ (commentPosition exactComment, ExactPrintTypes.commentContents exactComment)
  | comment <- collectComments parsedSource
  , exactComment <- ExactPrintUtils.tokComment comment
  ]
 where
  addTransport comments sourceComment
    | sourceComment `elem` comments = comments
    | otherwise = comments ++ [sourceComment]
  commentPosition comment =
    let span' = epaLocationRealSrcSpan $ ExactPrintTypes.commentLoc comment
    in (srcSpanStartLine span', srcSpanStartCol span')

collectComments :: ParsedSource -> [LEpaComment]
collectComments = SYB.everything (++) query
 where
  query :: SYB.GenericQ [LEpaComment]
  query = const [] `SYB.extQ` fromEpAnnComments

  fromEpAnnComments :: EpAnnComments -> [LEpaComment]
  fromEpAnnComments epComments =
    priorComments epComments ++ getFollowingComments epComments
