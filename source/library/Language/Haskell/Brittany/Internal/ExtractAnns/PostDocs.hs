{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.ExtractAnns.PostDocs
  (reassignClassFinalPostDocs) where

import           Data.Data                                ( Data )
import           Data.Foldable                            ( toList )
import qualified Data.List                               as List
import qualified Data.Map                                as Map
import           Data.Maybe                               ( mapMaybe )
import           GHC                                      ( GenLocated(L)
                                                          , unLoc
                                                          )
import           GHC.Hs                                   ( HsDecl(..)
                                                          , LHsDecl
                                                          , LSig
                                                          , Sig(..)
                                                          , TyClDecl(..)
                                                          )
import           GHC.Parser.Annotation                    ( HasLoc
                                                          , getLocA
                                                          )
import qualified GHC.Types.SrcLoc                        as SrcLoc
import           Language.Haskell.Brittany.Internal.ExactPrintCompat
import           Language.Haskell.Brittany.Internal.Prelude

reassignClassFinalPostDocs :: [LHsDecl GhcPs] -> Anns -> Anns
reassignClassFinalPostDocs declarations annotations =
  List.foldl' reassignPair annotations $ zip declarations $ drop 1 declarations

reassignPair :: Anns -> (LHsDecl GhcPs, LHsDecl GhcPs) -> Anns
reassignPair annotations (previous, next) =
  case (finalClassSignature previous, nodeStart next) of
    (Just (signatureKey, signatureEnd), Just nextStart) -> movePostDocs
      signatureKey
      signatureEnd
      (mkAnnKeyL next)
      nextStart
      annotations
    _ -> annotations

finalClassSignature :: LHsDecl GhcPs -> Maybe (AnnKey, (Int, Int))
finalClassSignature (L _ declaration) = case declaration of
  TyClD _ (ClassDecl _ _ _ _ _ _ signatures methods _ _ _) ->
    case reverse $ List.sortOn snd $ mapMaybe signatureTarget signatures of
      [] -> Nothing
      target@(_, signatureEnd) : _
        | all (< signatureEnd) $ mapMaybe nodeEnd $ toList methods -> Just
          target
        | otherwise -> Nothing
  _ -> Nothing
 where
  signatureTarget :: LSig GhcPs -> Maybe (AnnKey, (Int, Int))
  signatureTarget signature@(L _ signature') = do
    signatureEnd <- nodeEnd signature
    let targetKey = case signature' of
          ClassOpSig _ _ _ signatureType -> mkAnnKeyL signatureType
          _ -> mkAnnKeyL signature
    pure (targetKey, signatureEnd)

movePostDocs :: AnnKey -> (Int, Int) -> AnnKey -> (Int, Int) -> Anns -> Anns
movePostDocs signatureKey signatureEnd nextKey nextStart annotations =
  case Map.lookup nextKey annotations of
    Nothing -> annotations
    Just nextAnnotation ->
      case List.partition shouldMove $ annPriorComments nextAnnotation of
        ([], _) -> annotations
        (moved, remaining) ->
          Map.alter (Just . addToSignature moved) signatureKey $ Map.insert
            nextKey
            (nextAnnotation
              { annPriorComments = rebasePriors remaining
              , annEntryDelta    = entryDeltaAfterPriors nextStart remaining
              }
            )
            annotations
 where
  shouldMove (comment, _) = isPostDoc comment && case commentRange comment of
    Nothing -> False
    Just (commentStart, commentEnd) ->
      signatureEnd < commentStart && commentEnd < nextStart
  addToSignature moved maybeAnnotation =
    let annotation = fromMaybe emptyAnnotation maybeAnnotation
        following =
          List.sortOn commentStartPosition
            $ annFollowingComments annotation
            ++ moved
    in  annotation
          { annFollowingComments = rebaseComments signatureEnd following
          }

rebasePriors :: [(Comment, DeltaPos)] -> [(Comment, DeltaPos)]
rebasePriors comments = case List.sortOn commentStartPosition comments of
  [] -> []
  sorted@(firstComment : _) -> case commentStartPosition firstComment of
    Nothing         -> sorted
    Just firstStart -> rebaseComments firstStart sorted

entryDeltaAfterPriors :: (Int, Int) -> [(Comment, DeltaPos)] -> DeltaPos
entryDeltaAfterPriors nextStart comments =
  case reverse $ List.sortOn commentStartPosition comments of
    []               -> DP (0, 0)
    (comment, _) : _ -> case commentRange comment of
      Nothing              -> DP (0, 0)
      Just (_, commentEnd) -> positionDelta commentEnd nextStart

rebaseComments :: (Int, Int) -> [(Comment, DeltaPos)] -> [(Comment, DeltaPos)]
rebaseComments reference comments = snd
  $ List.mapAccumL rebase reference comments
 where
  rebase previous (comment, oldDelta) = case commentRange comment of
    Nothing -> (previous, (comment, oldDelta))
    Just (commentStart, commentEnd) ->
      (commentEnd, (comment, positionDelta previous commentStart))

isPostDoc :: Comment -> Bool
isPostDoc comment = case dropWhile (== ' ') $ commentContents comment of
  '-' : '-' : rest -> startsWithCaret rest
  '{' : '-' : rest -> startsWithCaret rest
  _                -> False
 where
  startsWithCaret = List.isPrefixOf "^" . dropWhile (== ' ')

commentRange :: Comment -> Maybe ((Int, Int), (Int, Int))
commentRange comment = do
  span' <- srcSpanToRealSpan $ commentIdentifier comment
  pure
    ( (SrcLoc.srcSpanStartLine span', SrcLoc.srcSpanStartCol span')
    , (SrcLoc.srcSpanEndLine span', SrcLoc.srcSpanEndCol span')
    )

commentStartPosition :: (Comment, DeltaPos) -> Maybe (Int, Int)
commentStartPosition = fmap fst . commentRange . fst

nodeStart :: HasLoc l => GenLocated l a -> Maybe (Int, Int)
nodeStart node = do
  span' <- srcSpanToRealSpan $ getLocA node
  pure (SrcLoc.srcSpanStartLine span', SrcLoc.srcSpanStartCol span')

nodeEnd :: HasLoc l => GenLocated l a -> Maybe (Int, Int)
nodeEnd node = do
  span' <- srcSpanToRealSpan $ getLocA node
  pure (SrcLoc.srcSpanEndLine span', SrcLoc.srcSpanEndCol span')

positionDelta :: (Int, Int) -> (Int, Int) -> DeltaPos
positionDelta (previousLine, previousColumn) (currentLine, currentColumn)
  | currentLine == previousLine = DP (0, currentColumn - previousColumn)
  | otherwise = DP (currentLine - previousLine, currentColumn - 1)

emptyAnnotation :: Annotation
emptyAnnotation = Ann Nothing Nothing [] [] [] $ DP (0, 0)

mkAnnKeyL :: (Data a, HasLoc l) => GenLocated l a -> AnnKey
mkAnnKeyL node = mkAnnKey $ L (getLocA node) (unLoc node)
