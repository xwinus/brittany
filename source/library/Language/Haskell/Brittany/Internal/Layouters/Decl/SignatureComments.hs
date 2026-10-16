{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Decl.SignatureComments
  ( appendSignatureSuffix ) where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Haskell.Brittany.Internal.ExactPrintCompat
import Language.Haskell.Brittany.Internal.SourceComment.Types (SourceCommentKey(..))
import Language.Haskell.Brittany.Internal.LayouterBasics (allocateNode)
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types

-- Measure trailing comments on the final type row, rather than adding their
-- width to every row of a function type. Preserve all layout alternatives and
-- keep delimiter groups intact so comments cannot move inside a closing token.
appendSignatureSuffix
  :: Set SourceCommentKey
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
appendSignatureSuffix claimedKeys document suffix = do
  annotations <- mAsk
  body <- document
  ending <- suffix
  State.evalStateT (append annotations ending body) Map.empty
 where
  append
    :: Anns
    -> BriDocNumbered
    -> BriDocNumbered
    -> State.StateT (Map.Map Int BriDocNumbered) ToBriDocM BriDocNumbered
  append annotations ending original@(nodeId, node) = do
    cached <- State.gets $ Map.lookup nodeId
    case cached of
      Just result -> pure result
      Nothing -> do
        let build = Trans.lift . allocateNode
            rec = append annotations ending
            wrap constructor child = build . constructor =<< rec child
            fallback = build $ BDFSeq [original, ending]
            lastChild constructor children = case reverse children of
              [] -> fallback
              child : previous -> do
                final <- rec child
                build $ constructor $ reverse previous ++ [final]
        result <- case node of
          BDFAlt alternatives -> build . BDFAlt =<< mapM rec alternatives
          BDFPar indent first rest -> wrap (BDFPar indent first) rest
          BDFLines rows -> lastChild BDFLines rows
          BDFCols signature columns -> lastChild (BDFCols signature) columns
          BDFSeq children -> lastChild BDFSeq children
          BDFAddBaseY indent child -> wrap (BDFAddBaseY indent) child
          BDFBaseYPushCur child -> wrap BDFBaseYPushCur child
          BDFBaseYPop child -> wrap BDFBaseYPop child
          BDFIndentLevelPushCur child -> wrap BDFIndentLevelPushCur child
          BDFIndentLevelPop child -> wrap BDFIndentLevelPop child
          BDFForwardLineMode child -> wrap BDFForwardLineMode child
          BDFAnnotationPrior mode key child -> wrap (BDFAnnotationPrior mode key) child
          BDFAnnotationKW key keyword child
            | any unclaimed $ keywordComments annotations key keyword -> fallback
            | otherwise -> wrap (BDFAnnotationKW key keyword) child
          BDFAnnotationRest key child
            | any unclaimed $ restComments annotations key -> fallback
            | otherwise -> wrap (BDFAnnotationRest key) child
          BDFMoveToKWDP key keyword offset child -> wrap (BDFMoveToKWDP key keyword offset) child
          BDFEnsureIndent indent child -> wrap (BDFEnsureIndent indent) child
          BDFForceMultiline child -> wrap BDFForceMultiline child
          BDFForceSingleline child -> wrap BDFForceSingleline child
          BDFColumnsLimit columns child -> wrap (BDFColumnsLimit columns) child
          BDFNonBottomSpacing spacing child -> wrap (BDFNonBottomSpacing spacing) child
          BDFSetParSpacing child -> wrap BDFSetParSpacing child
          BDFForceParSpacing child -> wrap BDFForceParSpacing child
          BDFDebug label child -> wrap (BDFDebug label) child
          _ -> fallback
        State.modify $ Map.insert nodeId result
        pure result

  -- These wrappers emit native comments after their child. Keep the suffix
  -- outside unless those comments are already supplied by the explicit suffix.
  unclaimed comment = Set.notMember
    (SourceCommentKey $ commentIdentifier comment) claimedKeys
  restComments annotations key = case Map.lookup key annotations of
    Nothing -> []
    Just annotation -> (fst <$> annFollowingComments annotation)
      ++ [comment | (AnnComment comment, _) <- annsDP annotation]
  keywordComments annotations key keyword = case Map.lookup key annotations of
    Nothing -> []
    Just annotation ->
      let entries = annsDP annotation
          following = case keyword of
            Nothing -> entries
            Just expected -> case dropWhile ((/= G expected) . fst) entries of
              [] -> []
              _ : remaining -> remaining
      in [comment | (AnnComment comment, _) <- takeWhile isComment following]
  isComment (AnnComment{}, _) = True
  isComment _ = False
