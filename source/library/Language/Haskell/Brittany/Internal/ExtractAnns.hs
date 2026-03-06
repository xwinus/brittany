{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | Extract brittany's Anns from GHC 9.14 parsed AST (EpAnn).
-- Traverses the AST to collect EpAnn from module, imports, decls, and
-- nested expr/bind/stmt nodes, building the Map AnnKey Annotation format
-- expected by layouters.
module Language.Haskell.Brittany.Internal.ExtractAnns where

import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Data.Data (Data, gmapQ, gmapQi)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Typeable (Typeable)
import Data.Maybe (catMaybes, mapMaybe)
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
  , GrhsAnn
  , HsExpr(..)
  , HsModule(..)
  , HsSigType
  , HsType
  , IE(..)
  , ImportDecl(..)
  , LIE
  , LImportDecl
  , LHsDecl
  , LHsExpr
  , LHsBind
  , LHsSigType
  , LHsType
  , LMatch
  , LPat
  , LGRHS
  , AnnsIf(..)
  , ExprLStmt
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
  , EpToken(..)
  , emptyComments
  , epaLocationRealSrcSpan
  , getFollowingComments
  , getLocA
  , NoEpAnns
  , priorComments
  , SrcSpanAnnLW
  )
import qualified GHC.Parser.Lexer
import GHC.Types.SrcLoc (EpaLocation'(..), RealSrcSpan, SrcSpan(..))
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.ExactPrintCompat
import Language.Haskell.Brittany.Internal.Prelude
import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Types as EPTypes
import qualified Language.Haskell.GHC.ExactPrint.Utils as EPUtils

-- | Convert GHC 9.14 parsed module to brittany's Anns format.
extractAnnsFromModule :: ParsedSource -> Anns
extractAnnsFromModule lmod =
  -- Step 1: Redistribute intra-declaration comments from module annotation to
  -- individual AST nodes. In GHC 9.14, ALL file comments are stored on the
  -- module header's EpAnn. We use ghc-exactprint's bottom-up traversal to claim
  -- comments within each node's span, but keep module-level comments (between
  -- imports/decls, trailing, pragmas) on the module annotation for our own
  -- distribution logic.
  let lmod' = redistributeIntraDeclComments lmod
      mod' = unLoc lmod'
      -- Step 2: Get remaining module-level comments (not claimed by nested nodes)
      modPriorComments = case hsmodAnn (hsmodExt mod') of
        EpAnn _anc _ cs -> List.concatMap lepaToSpanAndContent (priorComments cs)
        _ -> []
      modFollowingComments = case hsmodAnn (hsmodExt mod') of
        EpAnn _anc _ cs -> List.concatMap lepaToSpanAndContent (getFollowingComments cs)
        _ -> []
      -- Step 3: Build timeline of child nodes for distributing remaining comments
      importTargets = mapMaybe (\limport ->
        let idecl = unLoc limport
        in case maybeImportEpAnn idecl of
          Nothing -> Nothing
          Just (anc, _) ->
            let ancSpan = epaLocationRealSrcSpan anc
            in Just (mkAnnKeyL limport, ss2pos ancSpan, ss2posEnd ancSpan)
        ) (hsmodImports mod')
      declTargets = mapMaybe (\ldecl ->
        case maybeDeclEpAnn ldecl of
          Nothing -> Nothing
          Just (anc, _) ->
            let ancSpan = epaLocationRealSrcSpan anc
            in Just (mkAnnKeyL ldecl, ss2pos ancSpan, ss2posEnd ancSpan)
        ) (hsmodDecls mod')
      allTargets = List.sortOn (\(_, start, _) -> start) (importTargets ++ declTargets)
      -- Step 4: Distribute remaining module comments to child nodes
      sortedModComs = List.sortOn fst (modPriorComments ++ modFollowingComments)
      (modOwnComs, childComAssignments) = distributeModuleComments allTargets sortedModComs
      childPriorPatches = Map.fromListWith (++)
        [(k, coms) | (k, PriorCom, coms) <- childComAssignments]
      childFollowingPatches = Map.fromListWith (++)
        [(k, coms) | (k, FollowingCom, coms) <- childComAssignments]
      -- Step 5: Extract annotations from the preprocessed AST
      modAnnsRaw = extractModuleHeaderAnns lmod' mod'
      importAnns = extractImportAnns (1, 1) (hsmodImports mod')
      exportAnns = case hsmodExports mod' of
        Nothing -> Map.empty
        Just llies -> extractIEListAnns llies
      declAnns = extractDeclAnns (hsmodDecls mod')
      nestedAnns = extractNestedEpAnns (hsmodDecls mod')
      -- Step 6: Apply remaining comment patches to import/decl annotations
      patchAnns anns = Map.mapWithKey (\k ann ->
        let priorPatch = Map.findWithDefault [] k childPriorPatches
            followPatch = Map.findWithDefault [] k childFollowingPatches
        in ann { annPriorComments = priorPatch ++ annPriorComments ann
               , annFollowingComments = annFollowingComments ann ++ followPatch
               }
        ) anns
      importAnns' = patchAnns importAnns
      declAnns' = patchAnns declAnns
      -- Step 7: Module annotation gets only its own comments
      modKey = mkAnnKeyL lmod'
      modAnns = case Map.lookup modKey modAnnsRaw of
        Nothing -> modAnnsRaw
        Just modAnn ->
          let ownPriors = snd $ List.mapAccumL buildModComDP (1, 1) modOwnComs
          in Map.singleton modKey (modAnn { annPriorComments = ownPriors
                                          , annFollowingComments = [] })
  in modAnns <> importAnns' <> exportAnns <> declAnns' <> nestedAnns

-- | Redistribute intra-declaration comments from the module annotation to
-- individual AST nodes. Uses ghc-exactprint's bottom-up traversal to claim
-- comments within each node's span, then puts unclaimed comments back on
-- the module annotation (instead of distributing to imports/decls like
-- insertCppComments does).
redistributeIntraDeclComments :: ParsedSource -> ParsedSource
redistributeIntraDeclComments (L l p) = L l p'
  where
    modAnn = hsmodAnn (hsmodExt p)
    (allComments, p0) = case modAnn of
      EpAnn anct ant cst ->
        let cs = EPUtils.sortEpaComments $ priorComments cst ++ getFollowingComments cst
            p0' = p { hsmodExt = (hsmodExt p) { hsmodAnn = EpAnn anct ant emptyComments }}
        in (cs, p0')
      _ -> ([], p)
    -- Bottom-up traversal: each node claims comments within its span
    (p1, remaining) = runState (SYB.everywhereM (SYB.mkM addCommentsListItem
                                                  `SYB.extM` addCommentsGrhs
                                                  `SYB.extM` addCommentsList) p0) allComments
    -- Put unclaimed comments back on module annotation (NOT on imports/decls)
    p' = case hsmodAnn (hsmodExt p1) of
      EpAnn anc2 an2 cs2 ->
        let cs2' = EPUtils.workInComments cs2 remaining
        in p1 { hsmodExt = (hsmodExt p1) { hsmodAnn = EpAnn anc2 an2 cs2' }}
      _ -> p1

    addCommentsListItem :: EpAnn AnnListItem -> State [GHC.LEpaComment] (EpAnn AnnListItem)
    addCommentsListItem = addComments
    addCommentsGrhs :: EpAnn GrhsAnn -> State [GHC.LEpaComment] (EpAnn GrhsAnn)
    addCommentsGrhs = addComments
    addCommentsList :: EpAnn (AnnList ()) -> State [GHC.LEpaComment] (EpAnn (AnnList ()))
    addCommentsList = addComments
    addComments :: forall ann. EpAnn ann -> State [GHC.LEpaComment] (EpAnn ann)
    addComments (EpAnn anc an ocs) =
      case anc of
        EpaSpan (RealSrcSpan s _) -> do
          unAllocated <- get
          let (rest, these) = GHC.Parser.Lexer.allocateComments s unAllocated
              cs' = EPUtils.workInComments ocs these
          put rest
          return $ EpAnn anc an cs'
        _ -> return $ EpAnn anc an ocs
    addComments other = return other

data ComType = PriorCom | FollowingCom deriving (Eq)

-- | Distribute remaining module-level comments to the appropriate child nodes.
-- After insertCppComments, only trailing and inter-item comments remain.
distributeModuleComments
  :: [(AnnKey, (Int, Int), (Int, Int))]  -- (key, start, end) of child nodes
  -> [((Int, Int), (String, RealSrcSpan))]  -- sorted remaining module comments
  -> ( [((Int, Int), (String, RealSrcSpan))]  -- module's own comments (before any child)
     , [(AnnKey, ComType, [(Comment, DeltaPos)])]  -- child assignments
     )
distributeModuleComments targets coms =
  case targets of
    [] -> (coms, [])  -- no children, all comments stay with module
    ((_, firstStart, _) : _) ->
      let (beforeFirst, rest) = List.partition (\((l, _), _) -> l < fst firstStart) coms
          assignments = assignToTargets targets rest
      in (beforeFirst, assignments)
  where
    assignToTargets _ [] = []
    assignToTargets tgts comSpans =
      let go [] leftover = case reverse tgts of
            ((lastK, _, lastEnd) : _) | not (null leftover) ->
              let cds = snd $ List.mapAccumL buildModComDP lastEnd (List.sortOn fst leftover)
              in [(lastK, FollowingCom, cds)]
            _ -> []
          go ((k, _start, end) : rest) cs =
            let nextStart = case rest of
                  ((_, ns, _) : _) -> Just ns
                  [] -> Nothing
                -- Trailing: on same line as this target's end
                (trailing, nonTrailing) = List.partition
                  (\((l, _), _) -> l == fst end) cs
                -- Between this target and next
                (priorForNext, remaining) = case nextStart of
                  Just ns -> List.partition
                    (\((l, _), _) -> l > fst end && l < fst ns)
                    nonTrailing
                  Nothing -> ([], nonTrailing)
                trailingCDs = if null trailing then []
                  else let cds = snd $ List.mapAccumL buildModComDP end (List.sortOn fst trailing)
                       in [(k, FollowingCom, cds)]
                priorCDs = case rest of
                  ((nk, _, _) : _) | not (null priorForNext) ->
                    let sorted = List.sortOn fst priorForNext
                        initRef = case sorted of
                          ((pos, _) : _) -> pos
                          [] -> (1, 1)
                        cds = snd $ List.mapAccumL buildModComDP initRef sorted
                    in [(nk, PriorCom, cds)]
                  _ -> []
            in trailingCDs ++ priorCDs ++ go rest remaining
      in go tgts comSpans

buildModComDP :: (Int, Int) -> ((Int, Int), (String, RealSrcSpan))
              -> ((Int, Int), (Comment, DeltaPos))
buildModComDP prev ((line, col), (content, spanR)) =
  let dp = posToDP prev (line, col)
      nextPos = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
      bComment = Comment
        { commentOrigin = Nothing
        , commentIdentifier = realSpanToSrcSpan spanR
        , commentContents = content
        }
  in (nextPos, (bComment, dp))

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

extractImportAnns :: (Int, Int) -> [LImportDecl GhcPs] -> Anns
extractImportAnns startRef imports =
  let initial = (startRef, Nothing) :: ((Int, Int), Maybe AnnKey)
      (_, rawResults) = List.mapAccumL extractOne initial imports
      mainAnns = mconcat [main | (main, _, _) <- rawResults]
      ieAnns = mconcat [ie | (_, ie, _) <- rawResults]
      -- Collect trailing comment patches: (prevKey -> trailing comments)
      trailingPatches = Map.fromListWith (++)
        [(k, coms) | (_, _, Just (k, coms)) <- rawResults]
      -- Apply patches: add trailing comments as followingComments
      merged = Map.mapWithKey (\k ann -> case Map.lookup k trailingPatches of
        Nothing -> ann
        Just coms -> ann { annFollowingComments = annFollowingComments ann ++ coms }
        ) mainAnns
  in merged <> ieAnns
  where
    extractOne (prevEnd, prevKey) limport =
      let idecl = unLoc limport
      in case maybeImportEpAnn idecl of
        Nothing -> ((prevEnd, prevKey), (Map.empty, Map.empty, Nothing))
        Just (anc, cs) ->
          let ancSpan = epaLocationRealSrcSpan anc
              key = mkAnnKeyL limport
              importStart = ss2pos ancSpan
              importEnd = ss2posEnd ancSpan
              rawPriors = priorComments cs
              rawFollowing = getFollowingComments cs
              -- Split prior comments: trailing comments from the previous
              -- import (on prevEnd line) vs actual prior comments.
              allPriorSpans = List.sortOn fst (List.concatMap lepaToSpanAndContent rawPriors)
              (trailingPrev, rest) = case prevKey of
                Just _ -> List.partition
                  (\((line, _), _) -> line == fst prevEnd && fst prevEnd /= fst importStart)
                  allPriorSpans
                Nothing -> ([], allPriorSpans)
              -- Filter: only comments before import start are genuine priors
              actualPrior = List.filter
                (\((line, _), _) -> line < fst importStart) rest
              priorRef = case actualPrior of
                ((pos, _) : _) -> pos
                [] -> importStart
              priorComs = snd $ List.mapAccumL buildComDP priorRef actualPrior
              entryDelta = case actualPrior of
                [] -> case prevKey of
                  Just _ -> posToDP prevEnd importStart
                  Nothing -> DP (0, 0)  -- first import: spacing handled by module layout
                _ -> let (_, (_, spanR)) = List.last actualPrior
                         afterRef = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
                     in posToDP afterRef importStart
              followComs = lepaToCommentsWithDP importEnd rawFollowing
              -- Build trailing comments for previous import
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
              -- Also extract IE list annotations from import items
              ieAnns = case ideclImportList idecl of
                Nothing -> Map.empty
                Just (_, llies) -> extractIEListAnns llies
          in ((importEnd, Just key), (Map.singleton key ann, ieAnns, trailingPatch))

    buildComDP prev ((line, col), (content, spanR)) =
      let dp = posToDP prev (line, col)
          nextPos = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
          bComment = Comment
            { commentOrigin = Nothing
            , commentIdentifier = realSpanToSrcSpan spanR
            , commentContents = content
            }
      in (nextPos, (bComment, dp))

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
              -- Only classify as trailing when there IS a previous declaration
              (trailingPrev, rest) = case prevKey of
                Just _ -> List.partition
                  (\((line, _), _) -> line == fst prevEnd && fst prevEnd /= fst declStart)
                  allPriorSpans
                Nothing -> ([], allPriorSpans)
              -- Filter out inner comments (at/after declaration start).
              -- These belong to nested nodes and will be handled by
              -- extractNestedEpAnns. Including them here causes negative
              -- entryDelta, placing comments on the same line as the decl.
              actualPrior = List.filter
                (\((line, _), _) -> line < fst declStart) rest
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
  let -- Base extraction for all nodes (including HsIf/HsCase via location annotation)
      extractLHsExpr :: LHsExpr GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)]
      extractLHsExpr = extractFromLocatedWithLoc
      extractLHsDecl :: LHsDecl GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)]
      extractLHsDecl = extractFromLocated
      extractLHsBind :: LHsBind GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)]
      extractLHsBind = extractFromLocated
      extractLMatch :: LMatch GhcPs (LHsExpr GhcPs) -> [(AnnKey, RealSrcSpan, EpAnnComments)]
      extractLMatch = extractFromLocated
      extractLGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> [(AnnKey, RealSrcSpan, EpAnnComments)]
      extractLGRHS = extractFromLocated
      extractLStmt :: ExprLStmt GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)]
      extractLStmt = extractFromLocatedWithLoc
      extractLHsType :: LHsType GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)]
      extractLHsType = extractFromLocatedWithLoc
      extractLHsSigType :: LHsSigType GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)]
      extractLHsSigType = extractFromLocatedWithLoc
      extractLPat :: LPat GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)]
      extractLPat = extractFromLocatedWithLoc
      extract :: SYB.GenericQ [(AnnKey, RealSrcSpan, EpAnnComments)]
      extract =
        (const []
          `SYB.extQ` extractLHsExpr
          `SYB.extQ` extractLHsDecl
          `SYB.extQ` extractLHsBind
          `SYB.extQ` extractLMatch
          `SYB.extQ` extractLGRHS
          `SYB.extQ` extractLStmt
          `SYB.extQ` extractLHsType
          `SYB.extQ` extractLHsSigType
          `SYB.extQ` extractLPat
        )
      raw = SYB.everything (++) extract decls
      sorted = List.sortOn (\(_, sp, _) -> (SrcLoc.srcSpanStartLine sp, SrcLoc.srcSpanStartCol sp)) raw
      -- Build nested annotations with trailing comment redistribution:
      -- Comments on the same line as the previous node's end get moved from
      -- the current node's priorComments to the previous node's followingComments.
      initial = ((1, 1), Nothing) :: ((Int, Int), Maybe AnnKey)
      (_, rawNested) = List.mapAccumL buildNestedAccum initial sorted
      mainNested = Map.fromList [(k, ann) | (k, ann, _) <- rawNested]
      trailingPatches = Map.fromListWith (++)
        [(pk, coms) | (_, _, Just (pk, coms)) <- rawNested]
      baseAnns = Map.mapWithKey (\k ann -> case Map.lookup k trailingPatches of
        Nothing -> ann
        Just coms -> ann { annFollowingComments = annFollowingComments ann ++ coms }
        ) mainNested
      -- Redistribute inner comments (annsDP) from parent nodes to child nodes.
      -- For each annotation with inner comments, find the child annotation
      -- whose end position is closest to (and before) each comment's position,
      -- then move the comment to that child's annFollowingComments.
      -- Skip nodes handled by overrides (HsIf, HsDo) and nodes wrapped with
      -- docWrapNode (GRHS, Match) since their BDAnnotationRest handles annsDP.
      spanMap = Map.fromList [(k, (ss2pos sp, ss2posEnd sp)) | (k, sp, _) <- sorted]
      skipKeys = Map.map (const ()) overrideAnns
        <> Map.fromList [(k, ()) | (k, _, _) <- sorted, isWrappedNodeType k]
      isWrappedNodeType (AnnKey _ cn) = unConName cn `elem`
        ["GRHS", "Match", "ValD", "SigD", "TyClD", "InstD", "DerivD"]
      redistributedAnns = redistributeInnerComments spanMap skipKeys baseAnns
      -- Override pass: for compound expressions (HsIf, etc.), redistribute
      -- inner comments to child expression annotations (as prior comments)
      -- so BDAnnotationPrior emits them before the correct subexpression.
      overrideExpr :: LHsExpr GhcPs -> [(AnnKey, Annotation)]
      overrideExpr lexpr@(L loc expr) = case expr of
        HsIf xIf _ thenExpr elseExpr ->
          extractHsIfAnns lexpr loc xIf thenExpr elseExpr
        HsDo _ _ lstmts@(L stmtLoc stmts) ->
          extractHsDoAnns lexpr loc stmtLoc stmts
        _ -> []
      overrideExtract :: SYB.GenericQ [(AnnKey, Annotation)]
      overrideExtract = const [] `SYB.extQ` overrideExpr
      overrideAnns = Map.fromList $ SYB.everything (++) overrideExtract decls
  in overrideAnns <> redistributedAnns  -- overrideAnns wins for duplicate keys
  where
    buildNestedAccum (prevEnd, prevKey) (key, ancSpan, cs) =
      let nodeStart = ss2pos ancSpan
          nodeEnd = (SrcLoc.srcSpanEndLine ancSpan, SrcLoc.srcSpanEndCol ancSpan)
          rawPriors = priorComments cs
          -- Split prior comments: those BEFORE the node start are genuinely
          -- prior (emitted before the node by BDAnnotationPrior), while those
          -- AFTER or ON the node start belong inside the node and should be
          -- in annsDP (emitted at keyword positions by BDAnnotationKW).
          allPriorSpans = List.concatMap lepaToSpanAndContent rawPriors
          sortedPriors = List.sortOn fst allPriorSpans
          -- Trailing comments: on the same line as the previous node's end
          -- These should be followingComments on the previous node, not
          -- priorComments on this node.
          (trailingPrev, rest) = case prevKey of
            Just _ -> List.partition
              (\((line, _), _) -> line == fst prevEnd && fst prevEnd /= fst nodeStart)
              sortedPriors
            Nothing -> ([], sortedPriors)
          (genuinePriors, atOrAfterStart) = List.partition
            (\((line, _), _) -> line < fst nodeStart)
            rest
          -- Further split: comments on the nodeEnd line (same line as code end)
          -- are trailing comments that should go to annFollowingComments with
          -- DP relative to nodeEnd. True inner comments (between start and end
          -- lines) go to annsDP.
          (trailingSelf, innerComments) = List.partition
            (\((line, _), _) -> line == fst nodeEnd)
            atOrAfterStart
          -- Genuine prior comments (before node start)
          priorRef = case genuinePriors of
            ((pos, _) : _) -> pos
            [] -> nodeStart
          priorComs = snd $ List.mapAccumL buildComDP' priorRef genuinePriors
          entryDelta = case genuinePriors of
            [] -> DP (0, 0)
            _ -> let (_, (_, spanR)) = List.last genuinePriors
                     afterRef = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
                 in posToDP afterRef nodeStart
          -- Inner comments → annsDP as AnnComment entries
          innerDP = snd $ List.mapAccumL buildInnerComDP nodeStart innerComments
          -- Trailing-self comments (on same line as node end) → followingComments
          trailingSelfComs = snd $ List.mapAccumL buildComDP' nodeEnd trailingSelf
          followComs = trailingSelfComs ++ lepaToCommentsWithDP nodeEnd (getFollowingComments cs)
          -- Build trailing comments for previous node
          trailingComs = snd $ List.mapAccumL buildComDP' prevEnd trailingPrev
          trailingPatch = case prevKey of
            Just pk | not (null trailingComs) -> Just (pk, trailingComs)
            _ -> Nothing
          ann = Ann
            { annCapturedSpan = Nothing
            , annSortKey = Nothing
            , annsDP = innerDP
            , annFollowingComments = followComs
            , annPriorComments = priorComs
            , annEntryDelta = entryDelta
            }
      in ((nodeEnd, Just key), (key, ann, trailingPatch))

    buildComDP' prev ((line, col), (content, spanR)) =
      let dp = posToDP prev (line, col)
          nextPos = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
          bComment = Comment
            { commentOrigin = Nothing
            , commentIdentifier = realSpanToSrcSpan spanR
            , commentContents = content
            }
      in (nextPos, (bComment, dp))

    buildInnerComDP prev ((line, col), (content, spanR)) =
      let dp = posToDP prev (line, col)
          nextPos = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
          bComment = Comment
            { commentOrigin = Nothing
            , commentIdentifier = realSpanToSrcSpan spanR
            , commentContents = content
            }
      in (nextPos, (AnnComment bComment, dp))

    lastNestedCommentEnd :: [GHC.LEpaComment] -> Maybe (Int, Int)
    lastNestedCommentEnd lcs =
      let withSpans = List.concatMap lepaToSpanAndContent lcs
          sorted = List.sortOn fst withSpans
      in case sorted of
           [] -> Nothing
           _ -> let (_, (_, spanR)) = List.last sorted
                in Just (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)

-- | Redistribute inner comments (annsDP AnnComment entries) from parent
-- annotations to the most appropriate child annotations. For each inner
-- comment, finds the child annotation whose span ends closest before the
-- comment's position, and moves the comment to that child's
-- annFollowingComments with a recomputed DP relative to the child's end.
redistributeInnerComments
  :: Map.Map AnnKey ((Int, Int), (Int, Int))  -- span map: key -> (start, end)
  -> Map.Map AnnKey ()  -- override keys to skip
  -> Anns  -- input annotations
  -> Anns  -- output with inner comments redistributed
redistributeInnerComments spanMap skipKeys anns =
  let -- Collect all inner comments that need redistribution
      -- Skip nodes handled by overrides (HsIf, HsDo) to avoid double comments
      parentInnerComs = Map.toList $ Map.mapMaybeWithKey (\k ann ->
        if Map.member k skipKeys then Nothing
        else let innerComs = [(com, dp) | (AnnComment com, dp) <- annsDP ann]
             in if null innerComs then Nothing
                else case Map.lookup k spanMap of
                  Just (pStart, pEnd) -> Just (pStart, pEnd, innerComs)
                  Nothing -> Nothing
        ) anns
      -- For each parent's inner comments, find the best child target
      -- Children are annotations whose span is WITHIN the parent's span
      allChildren = [(k, start, end) | (k, (start, end)) <- Map.toList spanMap]
      assignments = List.concatMap (\(parentKey, (pStart, pEnd, coms)) ->
        let children = List.sortOn (\(_, s, _) -> s)
              [ (k, s, e) | (k, s, e) <- allChildren
              , k /= parentKey
              , s >= pStart && e <= pEnd
              ]
        in assignCommentsToChildren children coms
        ) parentInnerComs
      -- Group assignments by target key
      patches = Map.fromListWith (++) assignments
      -- Track which parents had comments successfully redistributed
      parentKeys = Map.fromList [(pk, ()) | (pk, _) <- parentInnerComs]
      -- Apply patches: add comments to children, remove from parents
      patched = Map.mapWithKey (\k ann ->
        let addComs = Map.findWithDefault [] k patches
            -- Strip inner AnnComment entries from parents that had redistribution
            strippedDP = if Map.member k parentKeys
              then List.filter (\x -> case x of { (AnnComment _, _) -> False; _ -> True }) (annsDP ann)
              else annsDP ann
        in ann { annFollowingComments = annFollowingComments ann ++ addComs
               , annsDP = strippedDP
               }
        ) anns
  in patched
  where
    -- | Assign each inner comment to the child whose end is closest before
    -- or at the comment's position. If no child qualifies, leave unassigned
    -- (comment stays on parent as annsDP).
    assignCommentsToChildren
      :: [(AnnKey, (Int, Int), (Int, Int))]  -- sorted children
      -> [(Comment, DeltaPos)]               -- inner comments
      -> [(AnnKey, [(Comment, DeltaPos)])]   -- assignments (key, comments)
    assignCommentsToChildren children coms =
      mapMaybe (assignOneComment children) coms

    assignOneComment
      :: [(AnnKey, (Int, Int), (Int, Int))]
      -> (Comment, DeltaPos)
      -> Maybe (AnnKey, [(Comment, DeltaPos)])
    assignOneComment children (com, _oldDP) =
      case srcSpanToRealSpan (commentIdentifier com) of
        Nothing -> Nothing
        Just comSpan ->
          let comPos = ss2pos comSpan
              -- Find the child whose end is closest to (but before/at) comPos.
              -- Sort by end position descending and take the first (closest end).
              candidates = List.sortOn (\(_, e) -> (fst comPos - fst e, snd comPos - snd e))
                [(k, e) | (k, _, e) <- children, e <= comPos]
          in case candidates of
            [] -> Nothing
            ((bestKey, bestEnd) : _) ->
              let dp = posToDP bestEnd comPos
              in Just (bestKey, [(com, dp)])

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
      initial = (containerRef, Nothing) :: ((Int, Int), Maybe AnnKey)
      (_, rawItems) = List.mapAccumL extractIEItem initial lies
      itemAnns = Map.fromList [(k, ann) | (k, ann, _) <- rawItems]
      trailingPatches = Map.fromListWith (++)
        [(pk, coms) | (_, _, Just (pk, coms)) <- rawItems]
      mergedItems = Map.mapWithKey (\k ann -> case Map.lookup k trailingPatches of
        Nothing -> ann
        Just coms -> ann { annFollowingComments = annFollowingComments ann ++ coms }
        ) itemAnns
  in containerAnns <> mergedItems
  where
    extractIEItem
      :: ((Int, Int), Maybe AnnKey)
      -> LIE GhcPs
      -> (((Int, Int), Maybe AnnKey), (AnnKey, Annotation, Maybe (AnnKey, [(Comment, DeltaPos)])))
    extractIEItem (prevEnd, prevKey) lie@(L lieEpann _) =
      case lieEpann of
        EpAnn anc _ cs ->
          let ancSpan = epaLocationRealSrcSpan anc
              key = mkAnnKeyL lie
              ieStart = ss2pos ancSpan
              ieEnd = ss2posEnd ancSpan
              rawPriors = priorComments cs
              rawFollowing = getFollowingComments cs
              allPriorSpans = List.sortOn fst (List.concatMap lepaToSpanAndContent rawPriors)
              -- Trailing comments from previous item
              (trailingPrev, rest) = case prevKey of
                Just _ -> List.partition
                  (\((line, _), _) -> line == fst prevEnd && fst prevEnd /= fst ieStart)
                  allPriorSpans
                Nothing -> ([], allPriorSpans)
              actualPrior = List.filter
                (\((line, _), _) -> line < fst ieStart) rest
              priorRef = case actualPrior of
                ((pos, _) : _) -> pos
                [] -> ieStart
              priorComs = snd $ List.mapAccumL buildIEComDP priorRef actualPrior
              entryDelta = case actualPrior of
                [] -> DP (0, 0)
                _ -> let (_, (_, spanR)) = List.last actualPrior
                         afterRef = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
                     in posToDP afterRef ieStart
              followComs = lepaToCommentsWithDP ieEnd rawFollowing
              trailingComs = snd $ List.mapAccumL buildIEComDP prevEnd trailingPrev
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
          in ((ieEnd, Just key), (key, ann, trailingPatch))
        _ ->
          let dummyAnn = Ann Nothing Nothing [] [] [] (DP (0, 0))
              key = mkAnnKeyL lie
          in ((prevEnd, prevKey), (key, dummyAnn, Nothing))

    buildIEComDP prev ((line, col), (content, spanR)) =
      let dp = posToDP prev (line, col)
          nextPos = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
          bComment = Comment
            { commentOrigin = Nothing
            , commentIdentifier = realSpanToSrcSpan spanR
            , commentContents = content
            }
      in (nextPos, (bComment, dp))

tryEpAnnFromLocation :: (Data l, Typeable l) => l -> Maybe (EpaLocation, EpAnnComments)
tryEpAnnFromLocation loc =
  tryExtractEpAnnFields loc
    <|> tryEpAnnFromDynamic (toDyn loc)
    <|> (tryEpAnnFromDynamic . gmapQi 0 toDyn $ loc)
    <|> asum (gmapQ tryExtractEpAnnFields loc)
    <|> asum (map tryEpAnnFromDynamic (gmapQ toDyn loc))
    <|> asum (map tryEpAnnFromDynamic (SYB.everything (++) (\x -> [toDyn x]) loc))

-- | Try to extract (EpaLocation, EpAnnComments) from a Dynamic that may
-- contain any EpAnn ann value. Uses specific fromDynamic attempts for common
-- types, then falls back to a generic 3-field detection approach.
tryEpAnnFromDynamic :: Dynamic -> Maybe (EpaLocation, EpAnnComments)
tryEpAnnFromDynamic dyn =
  (\e -> (entry e, comments e)) <$> fromDynamic @(EpAnn AnnContext) dyn
    <|> (\e -> (entry e, comments e)) <$> fromDynamic @(EpAnn AnnListItem) dyn
    <|> (\e -> (entry e, comments e)) <$> fromDynamic @(EpAnn NoEpAnns) dyn
    <|> (\e -> (entry e, comments e)) <$> fromDynamic @(EpAnn (AnnList ())) dyn
    <|> (\e -> (entry e, comments e)) <$> fromDynamic @(EpAnn ()) dyn
    <|> (\e -> (entry e, comments e)) <$> fromDynamic @(EpAnn [()]) dyn

-- | Generic extraction of (EpaLocation, EpAnnComments) from any Data value
-- that has exactly 3 fields where the 1st is EpaLocation and the 3rd is
-- EpAnnComments. This matches ANY EpAnn ann without needing to enumerate types.
tryExtractEpAnnFields :: Data d => d -> Maybe (EpaLocation, EpAnnComments)
tryExtractEpAnnFields d =
  let fields = gmapQ toDyn d
  in case fields of
    [f1, _, f3] -> (,) <$> fromDynamic @EpaLocation f1 <*> fromDynamic @EpAnnComments f3
    _ -> Nothing

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
      safeEpaLoc (EpaSpan (RealSrcSpan rss _)) = Just rss
      safeEpaLoc _ = Nothing
      -- Generic extraction: try each child of the payload to see if it's
      -- an EpAnn (any type) by checking for 3-field (EpaLocation, ?, EpAnnComments)
      fromPayload = catMaybes $ gmapQ
        (\child -> do (anc, cs) <- tryExtractEpAnnFields child; rss <- safeEpaLoc anc; pure (key, rss, cs))
        x
      fromLoc = do (anc, cs) <- tryEpAnnFromDynamic (toDyn loc); rss <- safeEpaLoc anc; pure (key, rss, cs)
      locOnly = case fromDynamic @(HsExpr GhcPs) (toDyn x) of
        Just e -> hasLocationOnlyEpAnn e
        Nothing -> False
  in if null fromPayload && locOnly then maybe [] pure fromLoc else fromPayload

-- | Like extractFromLocated, but tries the location annotation first, then
-- also checks payload extension fields. GHC 9.14 stores comments on both
-- the location annotation (e.g., EpAnn AnnListItem for expressions) and on
-- constructor extension fields (e.g., XHsDo). We merge comments from both.
extractFromLocatedWithLoc
  :: (Data a, HasLoc l, HasLoc (GenLocated l a), Data l, Typeable l)
  => GenLocated l a
  -> [(AnnKey, RealSrcSpan, EpAnnComments)]
extractFromLocatedWithLoc ln@(L loc x) =
  let key = mkAnnKeyL ln
      safeEpaLoc (EpaSpan (RealSrcSpan rss _)) = Just rss
      safeEpaLoc _ = Nothing
      -- Try location annotation
      fromLoc = case tryExtractEpAnnFields loc of
        Just (anc, cs) -> case safeEpaLoc anc of
          Just rss -> [(key, rss, cs)]
          Nothing -> []
        Nothing -> case tryEpAnnFromDynamic (toDyn loc) of
          Just (anc, cs) -> case safeEpaLoc anc of
            Just rss -> [(key, rss, cs)]
            Nothing -> []
          Nothing -> []
      -- Also try payload extension fields (e.g., XHsDo, XHsIf)
      fromPayload = catMaybes $ gmapQ
        (\child -> do (anc, cs) <- tryExtractEpAnnFields child; rss <- safeEpaLoc anc; pure (key, rss, cs))
        x
      -- Merge: use location result as base, add any payload comments
      merged = case (fromLoc, fromPayload) of
        ([], []) -> []
        ([], ps) -> ps
        (ls, []) -> ls
        ([(k, rss, locCs)], payloads) ->
          let allPriors = priorComments locCs ++ List.concatMap (\(_, _, c) -> priorComments c) payloads
              allFollows = getFollowingComments locCs ++ List.concatMap (\(_, _, c) -> getFollowingComments c) payloads
          in [(k, rss, EpaCommentsBalanced allPriors allFollows)]
        (ls, _) -> ls  -- multiple location results, just use those
  in merged

-- | Redistribute inner comments from HsIf to child expression annotations.
-- GHC 9.14 stores all comments on the location annotation (EpAnn AnnListItem)
-- of the HsIf node. Comments between keywords belong to child expressions:
-- - Between "then" and "else" → prior comment on then-expression
-- - After "else" → prior comment on else-expression
-- We create override annotations for the children with the redistributed
-- comments as annPriorComments, so BDAnnotationPrior emits them correctly.
extractHsIfAnns
  :: LHsExpr GhcPs
  -> EpAnn AnnListItem  -- location annotation (has comments)
  -> AnnsIf             -- extension annotation (has keyword positions)
  -> LHsExpr GhcPs      -- then-expression
  -> LHsExpr GhcPs      -- else-expression
  -> [(AnnKey, Annotation)]
extractHsIfAnns lexpr locAnn annsIf thenExpr elseExpr =
  let ifKey = mkAnnKeyL lexpr
      thenKey = mkAnnKeyL thenExpr
      elseKey = mkAnnKeyL elseExpr
  in case locAnn of
    EpAnn locAnc _ locCs ->
      let ancSpan = epaLocationRealSrcSpan locAnc
          nodeStart = ss2pos ancSpan
          nodeEnd = ss2posEnd ancSpan
          -- Keyword positions from AnnsIf
          thenPos = epTokenPos (aiThen annsIf)
          elsePos = epTokenPos (aiElse annsIf)
          -- Comments from location annotation
          rawPriors = priorComments locCs
          allComSpans = List.sortOn fst $ List.concatMap lepaToSpanAndContent rawPriors
          -- Split: before node start (genuine prior) vs at/after node start (inner)
          (genuinePriorSpans, innerComSpans) = List.partition
            (\((line, _), _) -> line < fst nodeStart) allComSpans
          -- Classify inner comments by keyword position
          -- Between "then" and "else" → then-expression prior
          -- After "else" → else-expression prior
          (thenComSpans, elseComSpans) = classifyByKeywords thenPos elsePos innerComSpans
          -- Build HsIf annotation: genuine priors only, no inner comments
          genuinePriorComs = lepaToCommentsWithDP nodeStart
            (List.filter (isBeforeNode nodeStart) rawPriors)
          entryDelta = case genuinePriorSpans of
            [] -> DP (0, 0)
            _ -> let (_, (_, spanR)) = List.last genuinePriorSpans
                     afterRef = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
                 in posToDP afterRef nodeStart
          followComs = lepaToCommentsWithDP nodeEnd (getFollowingComments locCs)
          ifAnn = Ann
            { annCapturedSpan = Nothing
            , annSortKey = Nothing
            , annsDP = []
            , annFollowingComments = followComs
            , annPriorComments = genuinePriorComs
            , annEntryDelta = entryDelta
            }
          -- Build child annotations with redistributed comments
          thenChildAnn = buildChildAnn thenPos thenComSpans thenExpr
          elseChildAnn = buildChildAnn elsePos elseComSpans elseExpr
      in [(ifKey, ifAnn)]
         ++ maybe [] (\a -> [(thenKey, a)]) thenChildAnn
         ++ maybe [] (\a -> [(elseKey, a)]) elseChildAnn
    _ -> []  -- EpAnnNotUsed; base extraction handles this
  where
    epTokenPos :: EpToken tok -> Maybe (Int, Int)
    epTokenPos (EpTok loc) = Just $ ss2pos (epaLocationRealSrcSpan loc)
    epTokenPos NoEpTok = Nothing

    isBeforeNode :: (Int, Int) -> GHC.LEpaComment -> Bool
    isBeforeNode ns lc =
      all (\((line, _), _) -> line < fst ns) (lepaToSpanAndContent lc)

    -- | Classify inner comments: before elsePos → then-expression,
    -- at/after elsePos → else-expression
    classifyByKeywords
      :: Maybe (Int, Int) -> Maybe (Int, Int)
      -> [((Int, Int), (String, RealSrcSpan))]
      -> ([((Int, Int), (String, RealSrcSpan))], [((Int, Int), (String, RealSrcSpan))])
    classifyByKeywords _thenPos elsePos coms = case elsePos of
      Just ep -> List.partition (\((line, _), _) -> line < fst ep) coms
      Nothing -> (coms, [])  -- no else → all go to then

    -- | Build a child annotation with redistributed comments as prior comments.
    -- The DP for each comment is computed relative to the preceding keyword
    -- position (e.g., "then" keyword for then-expression comments), so that
    -- the comment gets placed on a new line at the correct column.
    buildChildAnn
      :: Maybe (Int, Int)  -- keyword position (e.g., "then" position)
      -> [((Int, Int), (String, RealSrcSpan))]
      -> LHsExpr GhcPs
      -> Maybe Annotation
    buildChildAnn _ [] _ = Nothing
    buildChildAnn kwPos comSpans childExpr =
      let childStart = getExprStart childExpr
          -- Use keyword position as initial reference (it's before the comments).
          -- This gives positive line deltas for DP computation.
          initRef = case kwPos of
            Just kp -> kp
            Nothing -> case comSpans of
              ((pos, _) : _) -> pos
              [] -> childStart
          priorComs = snd $ List.mapAccumL (buildRelativeDP childStart) initRef comSpans
          entryDelta = case comSpans of
            [] -> DP (0, 0)
            _ -> let (_, (_, spanR)) = List.last comSpans
                     afterRef = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
                 in posToDP afterRef childStart
      in Just Ann
            { annCapturedSpan = Nothing
            , annSortKey = Nothing
            , annsDP = []
            , annFollowingComments = []
            , annPriorComments = priorComs
            , annEntryDelta = entryDelta
            }

    getExprStart :: LHsExpr GhcPs -> (Int, Int)
    getExprStart lexpr =
      let srcSpan = getLocA lexpr
      in case srcSpanToRealSpan srcSpan of
        Just rsp -> ss2pos rsp
        Nothing -> (1, 1)

    -- | Build a (Comment, DP) for a prior comment on a child expression.
    -- The DP's x-component is the column offset from the child's start column,
    -- since layoutMoveToCommentPos adds indLevelLinger (≈ child indent) to x.
    buildRelativeDP
      :: (Int, Int)  -- child expression start
      -> (Int, Int)  -- previous position (for chaining)
      -> ((Int, Int), (String, RealSrcSpan))
      -> ((Int, Int), (Comment, DeltaPos))
    buildRelativeDP (_childLine, _childCol) prev ((comLine, _comCol), (content, spanR)) =
      let -- layoutMoveToCommentPos uses indLevelLinger + x for column positioning.
          -- indLevelLinger already equals the child's indent level, so x=0
          -- places the comment at the correct indent. y must be >= 1 to preserve
          -- the pending newline from docPar.
          dp = if fst prev == comLine
               then DP (0, 0)
               else DP (max 1 (comLine - fst prev), 0)
          nextPos = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
          bComment = Comment
            { commentOrigin = Nothing
            , commentIdentifier = realSpanToSrcSpan spanR
            , commentContents = content
            }
      in (nextPos, (bComment, dp))

-- | Redistribute inner comments from HsDo to child statement annotations.
-- GHC 9.14 stores all comments on the location annotation (EpAnn AnnListItem)
-- of the HsDo node. Comments between statements should appear before the
-- nearest following statement.
extractHsDoAnns
  :: LHsExpr GhcPs
  -> EpAnn AnnListItem  -- location annotation (has comments)
  -> SrcSpanAnnLW  -- statement list location annotation
  -> [ExprLStmt GhcPs]  -- statements in the do-block
  -> [(AnnKey, Annotation)]
extractHsDoAnns lexpr locAnn stmtListAnn stmts =
  let doKey = mkAnnKeyL lexpr
  in case locAnn of
    EpAnn locAnc _ locCs ->
      let ancSpan = epaLocationRealSrcSpan locAnc
          nodeStart = ss2pos ancSpan
          nodeEnd = ss2posEnd ancSpan
          -- Comments from location annotation AND statement list annotation
          stmtListCs = case stmtListAnn of
            EpAnn _ _ cs -> cs
            _ -> emptyComments
          rawPriors = priorComments locCs ++ priorComments stmtListCs
          rawFollowing = getFollowingComments locCs ++ getFollowingComments stmtListCs
          allPriorSpans = List.sortOn fst $ List.concatMap lepaToSpanAndContent rawPriors
          allFollowSpans = List.sortOn fst $ List.concatMap lepaToSpanAndContent rawFollowing
          allComSpans = List.sortOn fst (allPriorSpans ++ allFollowSpans)
          -- Split: before node start (genuine prior) vs at/after node start (inner)
          (genuinePriorSpans, innerComSpans) = List.partition
            (\((line, _), _) -> line < fst nodeStart) allComSpans
          -- Get statement positions (key, startPos, endPos)
          stmtPosns = mapMaybe getStmtKeyAndPos stmts
          -- Distribute inner comments to nearest following statement
          assignments = distributeToStmts stmtPosns innerComSpans
          -- Build HsDo override annotation: genuine priors only
          genuinePriorComs = lepaToCommentsWithDP nodeStart
            (List.filter (isBeforeNode nodeStart) rawPriors)
          entryDelta = case genuinePriorSpans of
            [] -> DP (0, 0)
            _ -> let (_, (_, spanR)) = List.last genuinePriorSpans
                     afterRef = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
                 in posToDP afterRef nodeStart
          doAnn = Ann
            { annCapturedSpan = Nothing
            , annSortKey = Nothing
            , annsDP = []
            , annFollowingComments = []
            , annPriorComments = genuinePriorComs
            , annEntryDelta = entryDelta
            }
          -- Build child annotations with redistributed comments
          childAnns = List.concatMap (buildStmtAnn nodeStart) assignments
      in [(doKey, doAnn)] ++ childAnns
    _ -> []
  where
    isBeforeNode :: (Int, Int) -> GHC.LEpaComment -> Bool
    isBeforeNode ns lc =
      all (\((line, _), _) -> line < fst ns) (lepaToSpanAndContent lc)

    getStmtKeyAndPos :: ExprLStmt GhcPs -> Maybe (AnnKey, (Int, Int), (Int, Int))
    getStmtKeyAndPos lstmt =
      let key = mkAnnKeyL lstmt
          srcSpan = getLocA lstmt
      in case srcSpanToRealSpan srcSpan of
        Just rsp -> Just (key, ss2pos rsp, ss2posEnd rsp)
        Nothing  -> Nothing

    -- | For each inner comment, find the target statement. Comments on the
    -- same line as a statement's end and after it are trailing (following)
    -- comments on that statement. Other inner comments are prior comments
    -- on the first statement that starts on or after the comment's line.
    distributeToStmts
      :: [(AnnKey, (Int, Int), (Int, Int))]  -- (key, start, end)
      -> [((Int, Int), (String, RealSrcSpan))]
      -> [(AnnKey, (Int, Int), (Int, Int), [((Int, Int), (String, RealSrcSpan))], [((Int, Int), (String, RealSrcSpan))])]
      -- ^ (key, start, end, priorComs, followingComs)
    distributeToStmts stmtPosns comSpans =
      let assignComment comSpan@((comLine, comCol), _) =
            -- First check: is this a trailing comment on some statement?
            case List.find (\(_, _, (endLine, endCol)) ->
                    comLine == endLine && comCol >= endCol) stmtPosns of
              Just (key, _, _) -> Just (key, comSpan, True)  -- True = trailing
              Nothing ->
                -- Prior comment on next statement
                case List.find (\(_, (stmtLine, _), _) -> stmtLine >= comLine) stmtPosns of
                  Just (key, _, _) -> Just (key, comSpan, False)  -- False = prior
                  Nothing -> Nothing
          priorGrouped = Map.fromListWith (++)
            [(k, [c]) | (k, c, False) <- mapMaybe assignComment comSpans]
          followGrouped = Map.fromListWith (++)
            [(k, [c]) | (k, c, True) <- mapMaybe assignComment comSpans]
      in [ (key, pos, endPos, List.sortOn fst $ Map.findWithDefault [] key priorGrouped
                             , List.sortOn fst $ Map.findWithDefault [] key followGrouped)
         | (key, pos, endPos) <- stmtPosns
         , Map.member key priorGrouped || Map.member key followGrouped
         ]

    buildStmtAnn
      :: (Int, Int)  -- nodeStart (do-expression position)
      -> (AnnKey, (Int, Int), (Int, Int), [((Int, Int), (String, RealSrcSpan))], [((Int, Int), (String, RealSrcSpan))])
      -> [(AnnKey, Annotation)]
    buildStmtAnn doStart (key, stmtStart, stmtEnd, priorComSpans, followComSpans) =
      let -- Use do-expression's position as initial reference so first
          -- comment gets a proper line delta (not DP(0,0)).
          initRef = doStart
          priorComs = snd $ List.mapAccumL (buildRelativeDP stmtStart) initRef priorComSpans
          followComs = snd $ List.mapAccumL (buildRelativeDP stmtStart) stmtEnd followComSpans
          entryDelta = case priorComSpans of
            [] -> DP (0, 0)
            _ -> let (_, (_, spanR)) = List.last priorComSpans
                     afterRef = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
                 in posToDP afterRef stmtStart
      in [(key, Ann
            { annCapturedSpan = Nothing
            , annSortKey = Nothing
            , annsDP = []
            , annFollowingComments = followComs
            , annPriorComments = priorComs
            , annEntryDelta = entryDelta
            })]

    -- | Build a (Comment, DP) for a prior comment on a do-block statement.
    -- For y > 0, x = col - 1 (absolute 0-indexed column). This works because
    -- layoutMoveToCommentPos computes: column = indLevelLinger + x,
    -- and indLevelLinger = 0 for top-level do-blocks (the outer indent level
    -- before docSetBaseAndIndent pushes the new level).
    buildRelativeDP
      :: (Int, Int) -> (Int, Int)
      -> ((Int, Int), (String, RealSrcSpan))
      -> ((Int, Int), (Comment, DeltaPos))
    buildRelativeDP _stmtStart prev ((comLine, comCol), (content, spanR)) =
      let dp = posToDP prev (comLine, comCol)
          nextPos = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
          bComment = Comment
            { commentOrigin = Nothing
            , commentIdentifier = realSpanToSrcSpan spanR
            , commentContents = content
            }
      in (nextPos, (bComment, dp))

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
