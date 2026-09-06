{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.ExtractAnns.PostDocs
  ( reassignClassFinalPostDocs
  , reassignTypeSynonymFinalPostDocs
  ) where

import           Data.Data                                ( Data )
import           Data.Foldable                            ( toList )
import qualified Data.List                               as List
import qualified Data.Map                                as Map
import           Data.Maybe                               ( mapMaybe )
import           GHC                                      ( GenLocated(L)
                                                          , unLoc
                                                          )
import           GHC.Hs                                   ( HsDecl(..)
                                                          , HsType(..)
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

-- | Keep a post-doc after a class body attached to its final signature.
reassignClassFinalPostDocs :: [LHsDecl GhcPs] -> Anns -> Anns
reassignClassFinalPostDocs declarations annotations =
  List.foldl' reassignPair annotations $ zip declarations $ drop 1 declarations

-- | Keep a trailing function-result post-doc inside a type synonym RHS.
reassignTypeSynonymFinalPostDocs :: [LHsDecl GhcPs] -> Anns -> Anns
reassignTypeSynonymFinalPostDocs declarations annotations =
  List.foldl' reassignTypeSynonym annotations
    $ zip declarations
    $ (Just <$> drop 1 declarations)
    ++ [Nothing]

reassignTypeSynonym :: Anns -> (LHsDecl GhcPs, Maybe (LHsDecl GhcPs)) -> Anns
reassignTypeSynonym annotations (declaration, nextDeclaration) =
  case typeSynonymTarget declaration of
    Nothing -> annotations
    Just (declarationKey, declarationEnd, targetKey, targetEnd) ->
      let nextTarget = do
            next      <- nextDeclaration
            nextStart <- nodeStart next
            pure (mkAnnKeyL next, nextStart)
          afterFollowing = moveFollowingPostDocs declarationKey
                                                 declarationEnd
                                                 targetKey
                                                 targetEnd
                                                 (snd <$> nextTarget)
                                                 annotations
      in  case nextTarget of
            Nothing -> afterFollowing
            Just (nextKey, nextStart) -> movePriorPostDocs targetKey
                                                           targetEnd
                                                           nextKey
                                                           nextStart
                                                           afterFollowing

reassignPair :: Anns -> (LHsDecl GhcPs, LHsDecl GhcPs) -> Anns
reassignPair annotations (previous, next) =
  case (finalClassSignature previous, nodeStart next) of
    (Just (signatureKey, signatureEnd), Just nextStart) -> movePriorPostDocs
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

typeSynonymTarget
  :: LHsDecl GhcPs -> Maybe (AnnKey, (Int, Int), AnnKey, (Int, Int))
typeSynonymTarget declaration@(L _ declaration') = case declaration' of
  TyClD _ SynDecl { tcdRhs = rhs@(L _ HsFunTy{}) } -> do
    declarationEnd <- nodeEnd declaration
    targetEnd      <- nodeEnd rhs
    pure (mkAnnKeyL declaration, declarationEnd, mkAnnKeyL rhs, targetEnd)
  _ -> Nothing

movePriorPostDocs
  :: AnnKey -> (Int, Int) -> AnnKey -> (Int, Int) -> Anns -> Anns
movePriorPostDocs targetKey targetEnd nextKey nextStart annotations =
  case Map.lookup nextKey annotations of
    Nothing -> annotations
    Just nextAnnotation ->
      case List.partition shouldMove $ annPriorComments nextAnnotation of
        ([], _) -> annotations
        (moved, remaining) ->
          Map.alter (Just . addFollowing targetEnd moved) targetKey $ Map.insert
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
      targetEnd < commentStart && commentEnd < nextStart

moveFollowingPostDocs
  :: AnnKey
  -> (Int, Int)
  -> AnnKey
  -> (Int, Int)
  -> Maybe (Int, Int)
  -> Anns
  -> Anns
moveFollowingPostDocs ownerKey ownerEnd targetKey targetEnd upperBound annotations
  = case Map.lookup ownerKey annotations of
    Nothing -> annotations
    Just ownerAnnotation ->
      case List.partition shouldMove $ annFollowingComments ownerAnnotation of
        ([], _) -> annotations
        (moved, remaining) ->
          Map.alter (Just . addFollowing targetEnd moved) targetKey $ Map.insert
            ownerKey
            (ownerAnnotation
              { annFollowingComments = rebaseComments ownerEnd remaining
              }
            )
            annotations
 where
  shouldMove (comment, _) = isPostDoc comment && case commentRange comment of
    Nothing -> False
    Just (commentStart, commentEnd) ->
      ownerEnd < commentStart && maybe True (commentEnd <) upperBound

addFollowing
  :: (Int, Int) -> [(Comment, DeltaPos)] -> Maybe Annotation -> Annotation
addFollowing targetEnd moved maybeAnnotation =
  let annotation = fromMaybe emptyAnnotation maybeAnnotation
      following =
        List.sortOn commentStartPosition
          $ annFollowingComments annotation
          ++ moved
  in  annotation { annFollowingComments = rebaseComments targetEnd following }

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
