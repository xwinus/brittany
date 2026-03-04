{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | Extract brittany's Anns from GHC 9.14 parsed AST (EpAnn).
-- Traverses the AST to collect EpAnn from module, imports, decls, and
-- nested expr/bind/stmt nodes, building the Map AnnKey Annotation format
-- expected by layouters.
module Language.Haskell.Brittany.Internal.ExtractAnns where

import Data.Data (Data, gmapQ, gmapQi, toConstr)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Typeable (Typeable)
import Data.Maybe (mapMaybe)
import Data.Foldable (asum)
import qualified Data.Generics as SYB
import qualified Data.List as List
import qualified Data.Map as Map
import GHC
  ( GenLocated(L)
  , LEpaComment
  , ParsedSource
  , hsmodAnn
  , unLoc
  )
import GHC.Hs
  ( GhcPs
  , HsExpr(..)
  , HsModule(..)
  , IE(..)
  , ImportDecl(..)
  , LIE
  , LImportDecl
  , LHsDecl
  , LHsExpr
  , LHsBind
  , LMatch
  , LGRHS
  )
import GHC.Hs.ImpExp (ideclAnn, ideclExt)
import GHC.Parser.Annotation (HasLoc(..))
import GHC.Parser.Annotation
  ( AnnContext
  , AnnList
  , AnnListItem
  , EpAnn(..)
  , EpAnnComments(..)
  , EpaLocation
  , epaLocationRealSrcSpan
  , getFollowingComments
  , getLocA
  , NoEpAnns
  , priorComments
  )
import GHC.Types.SrcLoc (RealSrcSpan)
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.ExactPrintCompat
import Language.Haskell.Brittany.Internal.Prelude
import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Types as EPTypes
import qualified Language.Haskell.GHC.ExactPrint.Utils as EPUtils

-- | Convert GHC 9.14 parsed module to brittany's Anns format.
extractAnnsFromModule :: ParsedSource -> Anns
extractAnnsFromModule lmod =
  let mod' = unLoc lmod
      modAnns = extractModuleHeaderAnns lmod mod'
      importAnns = extractImportAnns (hsmodImports mod')
      exportAnns = case hsmodExports mod' of
        Nothing -> Map.empty
        Just llies -> extractIEListAnns llies
      declAnns = extractDeclAnns (hsmodDecls mod')
      nestedAnns = extractNestedEpAnns (hsmodDecls mod')
      -- Move module's following comments to the last declaration's
      -- following comments. GHC 9.14 attaches trailing comments of the
      -- last declaration to the module's EpAnn rather than the declaration's.
      theDecls = hsmodDecls mod'
      modFollowingRaw = List.concatMap annFollowingComments (Map.elems modAnns)
      lastDeclInfo = case reverse theDecls of
        (ld : _) -> case maybeDeclEpAnn ld of
          Just (anc, _) -> Just (mkAnnKeyL ld, ss2posEnd (epaLocationRealSrcSpan anc))
          Nothing -> Just (mkAnnKeyL ld, (1, 1))
        [] -> Nothing
      -- Recompute DPs for module following comments relative to last decl end
      recomputeFollowing lastEnd = recomputeComDPs lastEnd modFollowingRaw
      declAnns' = case lastDeclInfo of
        Just (ldk, lastEnd) | not (null modFollowingRaw) ->
          let recomputed = recomputeFollowing lastEnd
          in Map.adjust (\ann -> ann { annFollowingComments = annFollowingComments ann ++ recomputed }) ldk declAnns
        _ -> declAnns
      modAnns' = if null modFollowingRaw then modAnns
                 else Map.map (\ann -> ann { annFollowingComments = [] }) modAnns
      result = modAnns' <> importAnns <> exportAnns <> declAnns' <> nestedAnns
  in result

extractModuleHeaderAnns :: ParsedSource -> HsModule GhcPs -> Anns
extractModuleHeaderAnns lmod mod' =
  case hsmodAnn (hsmodExt mod') of
    EpAnn anc _ cs ->
      let key = mkAnnKeyL lmod
          ancSpan = epaLocationRealSrcSpan anc
          startOfFileRef = (1, 1)
          ann =
            Ann
              { annCapturedSpan = Nothing
              , annSortKey = Nothing
              , annsDP = []
              , annFollowingComments = lepaToCommentsWithDP startOfFileRef (getFollowingComments cs)
              , annPriorComments = lepaToCommentsWithDP startOfFileRef (priorComments cs)
              , annEntryDelta = posToDP startOfFileRef (ss2pos ancSpan)
              }
      in Map.singleton key ann
    _ -> Map.empty

extractImportAnns :: [LImportDecl GhcPs] -> Anns
extractImportAnns imports = mconcat $ snd $ List.mapAccumL extractOne (1, 1) imports
  where
    extractOne prevEnd limport =
      let idecl = unLoc limport
      in case maybeImportEpAnn idecl of
        Nothing -> (prevEnd, Map.empty)
        Just (anc, cs) ->
          let ancSpan = epaLocationRealSrcSpan anc
              key = mkAnnKeyL limport
              ann = buildAnnotation prevEnd ancSpan cs
              prevEnd' = ss2posEnd ancSpan
              -- Also extract IE list annotations from import items
              ieAnns = case ideclImportList idecl of
                Nothing -> Map.empty
                Just (_, llies) -> extractIEListAnns llies
          in (prevEnd', Map.singleton key ann <> ieAnns)

extractDeclAnns :: [LHsDecl GhcPs] -> Anns
extractDeclAnns decls =
  let initial = ((1, 1), Nothing)  -- (prevEnd, prevKey)
      (_, rawResults) = List.mapAccumL extractOne initial decls
      mainAnns = mconcat [main | (main, _) <- rawResults]
      -- Collect trailing comment patches: (prevKey -> trailing comments)
      trailingPatches = Map.fromListWith (++)
        [(k, coms) | (_, Just (k, coms)) <- rawResults]
      -- Apply patches: add trailing comments as followingComments
      merged = Map.mapWithKey (\k ann -> case Map.lookup k trailingPatches of
        Nothing -> ann
        Just coms -> ann { annFollowingComments = annFollowingComments ann ++ coms }
        ) mainAnns
  in merged
  where
    extractOne (prevEnd, prevKey) ldecl =
      case maybeDeclEpAnn ldecl of
        Nothing -> ((prevEnd, prevKey), (Map.empty, Nothing))
        Just (anc, cs) ->
          let ancSpan = epaLocationRealSrcSpan anc
              key = mkAnnKeyL ldecl
              declStart = ss2pos ancSpan
              declEnd = ss2posEnd ancSpan
              rawPriors = priorComments cs
              rawFollowing = getFollowingComments cs
              -- Split prior comments: trailing comments from the previous
              -- declaration (on prevEnd line) vs actual prior comments.
              allPriorSpans = List.sortOn fst (List.concatMap lepaToSpanAndContent rawPriors)
              (trailingPrev, actualPrior) = List.partition
                (\((line, _), _) -> line == fst prevEnd && fst prevEnd /= fst declStart)
                allPriorSpans
              -- Actual prior comments: use first comment's own position as
              -- reference so it gets DP(0,0) = starts at cursor position.
              priorRef = case actualPrior of
                ((pos, _) : _) -> pos
                [] -> declStart
              priorComs = snd $ List.mapAccumL buildComDP priorRef actualPrior
              -- Entry delta: if there are prior comments, delta from
              -- last prior comment end to declaration start.
              entryDelta = case actualPrior of
                [] -> DP (0, 0)
                _ -> let (_, (_, spanR)) = List.last actualPrior
                         afterRef = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
                     in posToDP afterRef declStart
              -- Following comments: relative to declaration end
              followComs = lepaToCommentsWithDP declEnd rawFollowing
              -- Build trailing comments for previous declaration
              trailingComs = snd $ List.mapAccumL buildComDP prevEnd trailingPrev
              trailingPatch = case prevKey of
                Just pk | not (null trailingComs) -> Just (pk, trailingComs)
                _ -> Nothing
              ann = Ann
                { annCapturedSpan = Nothing
                , annSortKey = Nothing
                , annsDP = []
                , annFollowingComments = followComs
                , annPriorComments = priorComs
                , annEntryDelta = entryDelta
                }
          in ((declEnd, Just key), (Map.singleton key ann, trailingPatch))

    buildComDP prev ((line, col), (content, spanR)) =
      let dp = posToDP prev (line, col)
          nextPos = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
          bComment = Comment
            { commentOrigin = Nothing
            , commentIdentifier = realSpanToSrcSpan spanR
            , commentContents = content
            }
      in (nextPos, (bComment, dp))

-- | Traverse decls and nested AST (exprs, binds, stmts, etc.) to extract
-- EpAnn from every annotated node. Enables hasAnyCommentsBelow and comment
-- preservation for layout decisions.
extractNestedEpAnns :: [LHsDecl GhcPs] -> Anns
extractNestedEpAnns decls =
  let extractLHsExpr :: LHsExpr GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)]
      extractLHsExpr lexpr@(L loc x) =
        if hasLocationOnlyEpAnn x
        then case tryEpAnnFromLocation loc of
               Just (anc, cs) -> [(mkAnnKeyL lexpr, epaLocationRealSrcSpan anc, cs)]
               Nothing -> extractFromLocated lexpr
        else extractFromLocated lexpr
      extractLHsDecl :: LHsDecl GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)]
      extractLHsDecl = extractFromLocated
      extractLHsBind :: LHsBind GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)]
      extractLHsBind = extractFromLocated
      extractLMatch :: LMatch GhcPs (LHsExpr GhcPs) -> [(AnnKey, RealSrcSpan, EpAnnComments)]
      extractLMatch = extractFromLocated
      extractLGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> [(AnnKey, RealSrcSpan, EpAnnComments)]
      extractLGRHS = extractFromLocated
      extract :: SYB.GenericQ [(AnnKey, RealSrcSpan, EpAnnComments)]
      extract =
        (const []
          `SYB.extQ` extractLHsExpr
          `SYB.extQ` extractLHsDecl
          `SYB.extQ` extractLHsBind
          `SYB.extQ` extractLMatch
          `SYB.extQ` extractLGRHS
        )
      raw = SYB.everything (++) extract decls
      sorted = List.sortOn (\(_, sp, _) -> (SrcLoc.srcSpanStartLine sp, SrcLoc.srcSpanStartCol sp)) raw
      anns = map buildNested sorted
  in Map.fromList anns
  where
    buildNested (key, ancSpan, cs) =
      let nodeStart = ss2pos ancSpan
          nodeEnd = (SrcLoc.srcSpanEndLine ancSpan, SrcLoc.srcSpanEndCol ancSpan)
          rawPriors = priorComments cs
          -- For nested nodes, compute prior comment DPs from the node's
          -- own start to keep deltas small and positive (these comments
          -- are typically on or just before the same line).
          priorComs = lepaToCommentsWithDP nodeStart rawPriors
          entryDelta = case lastNestedCommentEnd rawPriors of
            Just afterRef -> posToDP afterRef nodeStart
            Nothing -> DP (0, 0)
          followComs = lepaToCommentsWithDP nodeEnd (getFollowingComments cs)
          ann = Ann
            { annCapturedSpan = Nothing
            , annSortKey = Nothing
            , annsDP = []
            , annFollowingComments = followComs
            , annPriorComments = priorComs
            , annEntryDelta = entryDelta
            }
      in (key, ann)

    lastNestedCommentEnd :: [GHC.LEpaComment] -> Maybe (Int, Int)
    lastNestedCommentEnd lcs =
      let withSpans = List.concatMap lepaToSpanAndContent lcs
          sorted = List.sortOn fst withSpans
      in case sorted of
           [] -> Nothing
           _ -> let (_, (_, spanR)) = List.last sorted
                in Just (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)

-- | Extract annotations for IE (import/export) list container and items.
-- Handles both import lists (ideclImportList) and export lists (hsmodExports).
extractIEListAnns :: GenLocated (EpAnn ann) [LIE GhcPs] -> Anns
extractIEListAnns llies@(L epann lies) =
  let containerAnns = case epann of
        EpAnn anc _ cs ->
          let ancSpan = epaLocationRealSrcSpan anc
              key = mkAnnKeyL llies
              ref = ss2pos ancSpan
              ann = buildAnnotation ref ancSpan cs
          in Map.singleton key ann
        _ -> Map.empty
      containerRef = case epann of
        EpAnn anc _ _ -> ss2pos (epaLocationRealSrcSpan anc)
        _ -> (1, 1)
      (_, itemAnnsList) = List.mapAccumL extractIEItem containerRef lies
  in containerAnns <> mconcat itemAnnsList
  where
    extractIEItem :: (Int, Int) -> LIE GhcPs -> ((Int, Int), Anns)
    extractIEItem prevEnd lie@(L lieEpann _) =
      case lieEpann of
        EpAnn anc _ cs ->
          let ancSpan = epaLocationRealSrcSpan anc
              key = mkAnnKeyL lie
              ann = buildAnnotation prevEnd ancSpan cs
              newEnd = ss2posEnd ancSpan
          in (newEnd, Map.singleton key ann)
        _ -> (prevEnd, Map.empty)

tryEpAnnFromLocation :: (Data l, Typeable l) => l -> Maybe (EpaLocation, EpAnnComments)
tryEpAnnFromLocation loc =
  tryEpAnnFromDynamic (toDyn loc)
    <|> (tryEpAnnFromDynamic . gmapQi 0 toDyn $ loc)
    <|> asum (map tryEpAnnFromDynamic (gmapQ toDyn loc))
    <|> asum (map tryEpAnnFromDynamic (SYB.everything (++) (\x -> [toDyn x]) loc))

tryEpAnnFromDynamic :: Dynamic -> Maybe (EpaLocation, EpAnnComments)
tryEpAnnFromDynamic dyn =
  (\e -> (entry e, comments e)) <$> fromDynamic @(EpAnn AnnContext) dyn
    <|> (\e -> (entry e, comments e)) <$> fromDynamic @(EpAnn AnnListItem) dyn
    <|> (\e -> (entry e, comments e)) <$> fromDynamic @(EpAnn NoEpAnns) dyn
    <|> (\e -> (entry e, comments e)) <$> fromDynamic @(EpAnn (AnnList ())) dyn
    <|> (\e -> (entry e, comments e)) <$> fromDynamic @(EpAnn ()) dyn
    <|> (\e -> (entry e, comments e)) <$> fromDynamic @(EpAnn [()]) dyn

hasLocationOnlyEpAnn :: HsExpr GhcPs -> Bool
hasLocationOnlyEpAnn e = case e of
  HsLit {} -> True
  HsOverLit {} -> True
  HsVar {} -> True
  HsOverLabel {} -> True
  HsIPVar {} -> True
  _ -> False

extractFromLocated
  :: (Data a, HasLoc l, HasLoc (GenLocated l a), Typeable l)
  => GenLocated l a
  -> [(AnnKey, RealSrcSpan, EpAnnComments)]
extractFromLocated ln@(L loc x) =
  let key = mkAnnKeyL ln
      children = gmapQ toDyn x
      fromPayload =
        mapMaybe
          (\dyn -> fmap (\(anc, cs) -> (key, epaLocationRealSrcSpan anc, cs)) (tryEpAnnFromDynamic dyn))
          children
      fromLoc = fmap (\(anc, cs) -> (key, epaLocationRealSrcSpan anc, cs)) (tryEpAnnFromDynamic (toDyn loc))
      locOnly = case fromDynamic @(HsExpr GhcPs) (toDyn x) of
        Just e -> hasLocationOnlyEpAnn e
        Nothing -> False
  in if null fromPayload && locOnly then maybe [] pure fromLoc else fromPayload

maybeImportEpAnn :: ImportDecl GhcPs -> Maybe (EpaLocation, EpAnnComments)
maybeImportEpAnn idecl =
  case ideclAnn (ideclExt idecl) of
    EpAnn anc _ cs -> Just (anc, cs)
    _ -> Nothing

maybeDeclEpAnn :: LHsDecl GhcPs -> Maybe (EpaLocation, EpAnnComments)
maybeDeclEpAnn (L loc _) = tryEpAnnFromLocation loc

mkAnnKeyL :: (Data a, HasLoc l, HasLoc (GenLocated l a)) => GenLocated l a -> AnnKey
mkAnnKeyL x = mkAnnKey (L (getLocA x) (unLoc x))

buildAnnotation :: (Int, Int) -> RealSrcSpan -> EpAnnComments -> Annotation
buildAnnotation prevEnd ancSpan cs =
  Ann
    { annCapturedSpan = Nothing
    , annSortKey = Nothing
    , annsDP = []
    , annFollowingComments = lepaToCommentsWithDP prevEnd (getFollowingComments cs)
    , annPriorComments = lepaToCommentsWithDP prevEnd (priorComments cs)
    , annEntryDelta = posToDP prevEnd (ss2pos ancSpan)
    }

lepaToCommentsWithDP :: (Int, Int) -> [GHC.LEpaComment] -> [(Comment, DeltaPos)]
lepaToCommentsWithDP ref lcs =
  let withSpans = List.concatMap lepaToSpanAndContent lcs
      sorted = List.sortOn fst withSpans
  in snd $ List.mapAccumL go ref sorted
  where
    go prev ((line, col), (content, spanR)) =
      let dp = posToDP prev (line, col)
          nextPos = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
          bComment =
            Comment
              { commentOrigin = Nothing
              , commentIdentifier = realSpanToSrcSpan spanR
              , commentContents = content
              }
      in (nextPos, (bComment, dp))

lepaToSpanAndContent :: GHC.LEpaComment -> [((Int, Int), (String, RealSrcSpan))]
lepaToSpanAndContent lc@(GHC.L _loc _) =
  let epComments = EPUtils.tokComment lc
  in [ (ss2pos $ epaLocationRealSrcSpan (EPTypes.commentLoc x), (EPTypes.commentContents x, epaLocationRealSrcSpan (EPTypes.commentLoc x)))
     | x <- epComments
     ]

-- | Recompute DeltaPos for comments relative to a new reference point.
-- Extracts actual source positions from commentIdentifier and chains DPs.
recomputeComDPs :: (Int, Int) -> [(Comment, DeltaPos)] -> [(Comment, DeltaPos)]
recomputeComDPs ref coms = snd $ List.mapAccumL go ref coms
  where
    go prev (com, _oldDP) =
      case srcSpanToRealSpan (commentIdentifier com) of
        Just rspan ->
          let pos = ss2pos rspan
              dp = posToDP prev pos
              nextPos = (SrcLoc.srcSpanEndLine rspan, SrcLoc.srcSpanEndCol rspan)
          in (nextPos, (com, dp))
        Nothing -> (prev, (com, DP (0, 1)))  -- fallback

posToDP :: (Int, Int) -> (Int, Int) -> DeltaPos
posToDP (prevL, prevC) (curL, curC)
  | curL == prevL = DP (0, curC - prevC)
  | otherwise = DP (curL - prevL, curC - 1)

ss2pos :: RealSrcSpan -> (Int, Int)
ss2pos s = (SrcLoc.srcSpanStartLine s, SrcLoc.srcSpanStartCol s)

ss2posEnd :: RealSrcSpan -> (Int, Int)
ss2posEnd s = (SrcLoc.srcSpanEndLine s, SrcLoc.srcSpanEndCol s)
