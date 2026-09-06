{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.CommentBoundary.Trailing
  ( takeTrailingContinuationRun
  , plainLineCommentText
  , normalizeFinalTrailingRun
  ) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import GHC (GenLocated(L), unLoc)
import GHC.Hs (HsDecl(..), LHsDecl)
import GHC.Parser.Annotation (getLocA)
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.ExactPrintCompat
import Language.Haskell.Brittany.Internal.Prelude

normalizeFinalTrailingRun :: [LHsDecl GhcPs] -> Anns -> Anns
normalizeFinalTrailingRun declarations annotations = case reverse declarations of
  declaration : _ | ValD{} <- unLoc declaration ->
    let key = mkAnnKey $ L (getLocA declaration) $ unLoc declaration
    in case annKeyRealSpan key of
      Just declarationSpan -> Map.adjust (normalize declarationSpan) key annotations
      Nothing -> annotations
  _ -> annotations
 where
  normalize declarationSpan annotation =
    let sorted = List.sortBy (\(left, _) (right, _) ->
          compareSrcSpan (commentIdentifier left) (commentIdentifier right))
          $ annFollowingComments annotation
        (seeds, following) = List.partition
          (\comment -> maybe False
            ((== SrcLoc.srcSpanEndLine declarationSpan) . SrcLoc.srcSpanStartLine)
            $ srcSpanToRealSpan $ commentIdentifier comment)
          $ fst <$> sorted
        (continuations, _) = takeTrailingContinuationRun
          declarationSpan seeds following
    in if null continuations
      then annotation
      else annotation
        { annFollowingComments = snd $ mapAccumL rebase
            (SrcLoc.srcSpanEndLine declarationSpan, SrcLoc.srcSpanEndCol declarationSpan)
            sorted
        }
  rebase previous unchanged@(comment, _) = case
      srcSpanToRealSpan $ commentIdentifier comment of
    Nothing -> (previous, unchanged)
    Just span' ->
      let (previousLine, previousColumn) = previous
          line = SrcLoc.srcSpanStartLine span'
          column = SrcLoc.srcSpanStartCol span'
          delta = if line == previousLine
            then DP (0, column - previousColumn)
            else DP (line - previousLine, column - 1)
      in ((SrcLoc.srcSpanEndLine span', SrcLoc.srcSpanEndCol span'), (comment, delta))

takeTrailingContinuationRun
  :: SrcLoc.RealSrcSpan
  -> [Comment]
  -> [Comment]
  -> ([Comment], [Comment])
takeTrailingContinuationRun declarationSpan seeds comments =
  case (filter isTrailingSeed seeds, comments) of
    ([seed], firstComment : _)
      | Just firstSpan <- ordinaryLineSpan firstComment
      , SrcLoc.srcSpanStartCol firstSpan
          > SrcLoc.srcSpanStartCol declarationSpan ->
          takeRun (SrcLoc.srcSpanStartCol firstSpan) seed comments
    _ -> ([], comments)
 where
  isTrailingSeed comment = case ordinaryLineSpan comment of
    Just span' -> sameFile span'
      && SrcLoc.srcSpanStartLine span' == SrcLoc.srcSpanEndLine declarationSpan
      && SrcLoc.srcSpanStartCol span' >= SrcLoc.srcSpanEndCol declarationSpan
    Nothing -> False
  takeRun _ _ [] = ([], [])
  takeRun column previous remaining@(current : rest) = case
      (ordinaryLineSpan previous, ordinaryLineSpan current) of
    (Just previousSpan, Just currentSpan)
      | sameFile currentSpan
      , SrcLoc.srcSpanStartLine currentSpan
          == SrcLoc.srcSpanEndLine previousSpan + 1
      , SrcLoc.srcSpanStartCol currentSpan == column ->
          let (continuations, following) = takeRun column current rest
          in (current : continuations, following)
    _ -> ([], remaining)
  sameFile span' = SrcLoc.srcSpanFile span'
    == SrcLoc.srcSpanFile declarationSpan

ordinaryLineSpan :: Comment -> Maybe SrcLoc.RealSrcSpan
ordinaryLineSpan comment = do
  span' <- srcSpanToRealSpan $ commentIdentifier comment
  guard $ SrcLoc.srcSpanStartLine span' == SrcLoc.srcSpanEndLine span'
  guard $ plainLineCommentText $ commentContents comment
  pure span'

plainLineCommentText :: String -> Bool
plainLineCommentText text = case dropWhile Char.isSpace text of
    '-' : '-' : rest ->
      let content = dropWhile Char.isSpace rest
          ordinaryMarker = case content of
            marker : _ -> marker `notElem` ['|', '^', '*', '$', '#']
            [] -> True
      in ordinaryMarker && not (any (`List.isPrefixOf` content)
        ["BRITTANY", "brittany", "{-#"])
    _ -> False
