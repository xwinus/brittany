{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | Extract brittany's Anns from GHC 9.14 parsed AST (EpAnn).
-- Traverses the AST to collect EpAnn from module, imports, decls, and
-- nested expr/bind/stmt nodes, building the Map AnnKey Annotation format
-- expected by layouters.
module Language.Haskell.Brittany.Internal.ExtractAnns where

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
      -- Compute starting reference for imports: end of module header or
      -- last prior comment (pragma), so import entry deltas are correct.
      importPrevEnd = case hsmodAnn (hsmodExt mod') of
        EpAnn anc _ cs ->
          let ancSpan = epaLocationRealSrcSpan anc
              headerEnd = ss2posEnd ancSpan
              -- Also check prior comments (pragmas): the last one may be
              -- later than the module header anchor.
              priorEnds = [ ss2posEnd (epaLocationRealSrcSpan (EPTypes.commentLoc x))
                          | lc <- priorComments cs
                          , x <- EPUtils.tokComment lc
                          ]
          in if null priorEnds then headerEnd
             else maximum (headerEnd : priorEnds)
        _ -> (1, 1)
      importAnns = extractImportAnns importPrevEnd (hsmodImports mod')
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

extractImportAnns :: (Int, Int) -> [LImportDecl GhcPs] -> Anns
extractImportAnns startRef imports = mconcat $ snd $ List.mapAccumL extractOne startRef imports
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
      extract :: SYB.GenericQ [(AnnKey, RealSrcSpan, EpAnnComments)]
      extract =
        (const []
          `SYB.extQ` extractLHsExpr
          `SYB.extQ` extractLHsDecl
          `SYB.extQ` extractLHsBind
          `SYB.extQ` extractLMatch
          `SYB.extQ` extractLGRHS
          `SYB.extQ` extractLStmt
        )
      raw = SYB.everything (++) extract decls
      sorted = List.sortOn (\(_, sp, _) -> (SrcLoc.srcSpanStartLine sp, SrcLoc.srcSpanStartCol sp)) raw
      baseAnns = Map.fromList $ map buildNested sorted
      -- Override pass: for compound expressions (HsIf, etc.), redistribute
      -- inner comments to child expression annotations (as prior comments)
      -- so BDAnnotationPrior emits them before the correct subexpression.
      overrideExpr :: LHsExpr GhcPs -> [(AnnKey, Annotation)]
      overrideExpr lexpr@(L loc expr) = case expr of
        HsIf xIf _ thenExpr elseExpr ->
          extractHsIfAnns lexpr loc xIf thenExpr elseExpr
        HsDo _ _ (L _ stmts) ->
          extractHsDoAnns lexpr loc stmts
        _ -> []
      overrideExtract :: SYB.GenericQ [(AnnKey, Annotation)]
      overrideExtract = const [] `SYB.extQ` overrideExpr
      overrideAnns = Map.fromList $ SYB.everything (++) overrideExtract decls
  in overrideAnns <> baseAnns  -- overrideAnns wins for duplicate keys
  where
    buildNested (key, ancSpan, cs) =
      let nodeStart = ss2pos ancSpan
          nodeEnd = (SrcLoc.srcSpanEndLine ancSpan, SrcLoc.srcSpanEndCol ancSpan)
          rawPriors = priorComments cs
          -- Split prior comments: those BEFORE the node start are genuinely
          -- prior (emitted before the node by BDAnnotationPrior), while those
          -- AFTER or ON the node start belong inside the node and should be
          -- in annsDP (emitted at keyword positions by BDAnnotationKW).
          allPriorSpans = List.concatMap lepaToSpanAndContent rawPriors
          sortedPriors = List.sortOn fst allPriorSpans
          (genuinePriors, innerComments) = List.partition
            (\((line, _), _) -> line < fst nodeStart)
            sortedPriors
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
          followComs = lepaToCommentsWithDP nodeEnd (getFollowingComments cs)
          ann = Ann
            { annCapturedSpan = Nothing
            , annSortKey = Nothing
            , annsDP = innerDP
            , annFollowingComments = followComs
            , annPriorComments = priorComs
            , annEntryDelta = entryDelta
            }
      in (key, ann)

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
      -- Generic extraction: try each child of the payload to see if it's
      -- an EpAnn (any type) by checking for 3-field (EpaLocation, ?, EpAnnComments)
      fromPayload = catMaybes $ gmapQ
        (\child -> fmap (\(anc, cs) -> (key, epaLocationRealSrcSpan anc, cs)) (tryExtractEpAnnFields child))
        x
      fromLoc = fmap (\(anc, cs) -> (key, epaLocationRealSrcSpan anc, cs)) (tryEpAnnFromDynamic (toDyn loc))
      locOnly = case fromDynamic @(HsExpr GhcPs) (toDyn x) of
        Just e -> hasLocationOnlyEpAnn e
        Nothing -> False
  in if null fromPayload && locOnly then maybe [] pure fromLoc else fromPayload

-- | Like extractFromLocated, but always tries the location annotation first.
-- GHC 9.14 stores comments on the location annotation (e.g., EpAnn AnnListItem
-- for expressions), not (only) in the payload's extension field.
extractFromLocatedWithLoc
  :: (Data a, HasLoc l, HasLoc (GenLocated l a), Data l, Typeable l)
  => GenLocated l a
  -> [(AnnKey, RealSrcSpan, EpAnnComments)]
extractFromLocatedWithLoc ln@(L loc _) =
  let key = mkAnnKeyL ln
      -- Try the location annotation first (this is where comments live)
      fromLoc = case tryExtractEpAnnFields loc of
        Just (anc, cs) -> [(key, epaLocationRealSrcSpan anc, cs)]
        Nothing -> case tryEpAnnFromDynamic (toDyn loc) of
          Just (anc, cs) -> [(key, epaLocationRealSrcSpan anc, cs)]
          Nothing -> []
  in if null fromLoc then extractFromLocated ln else fromLoc

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
  -> [ExprLStmt GhcPs]  -- statements in the do-block
  -> [(AnnKey, Annotation)]
extractHsDoAnns lexpr locAnn stmts =
  let doKey = mkAnnKeyL lexpr
  in case locAnn of
    EpAnn locAnc _ locCs ->
      let ancSpan = epaLocationRealSrcSpan locAnc
          nodeStart = ss2pos ancSpan
          nodeEnd = ss2posEnd ancSpan
          -- Comments from location annotation
          rawPriors = priorComments locCs
          allComSpans = List.sortOn fst $ List.concatMap lepaToSpanAndContent rawPriors
          -- Split: before node start (genuine prior) vs at/after node start (inner)
          (genuinePriorSpans, innerComSpans) = List.partition
            (\((line, _), _) -> line < fst nodeStart) allComSpans
          -- Get statement positions (key, startPos)
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
          followComs = lepaToCommentsWithDP nodeEnd (getFollowingComments locCs)
          doAnn = Ann
            { annCapturedSpan = Nothing
            , annSortKey = Nothing
            , annsDP = []
            , annFollowingComments = followComs
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

    getStmtKeyAndPos :: ExprLStmt GhcPs -> Maybe (AnnKey, (Int, Int))
    getStmtKeyAndPos lstmt =
      let key = mkAnnKeyL lstmt
          srcSpan = getLocA lstmt
      in case srcSpanToRealSpan srcSpan of
        Just rsp -> Just (key, ss2pos rsp)
        Nothing  -> Nothing

    -- | For each inner comment, find the first statement that starts on or
    -- after the comment's line. Group comments by target statement.
    distributeToStmts
      :: [(AnnKey, (Int, Int))]
      -> [((Int, Int), (String, RealSrcSpan))]
      -> [(AnnKey, (Int, Int), [((Int, Int), (String, RealSrcSpan))])]
    distributeToStmts stmtPosns comSpans =
      let assignComment comSpan@((comLine, _), _) =
            case List.find (\(_, (stmtLine, _)) -> stmtLine >= comLine) stmtPosns of
              Just (key, _) -> Just (key, comSpan)
              Nothing -> Nothing
          grouped = Map.fromListWith (++) [(k, [c]) | (k, c) <- mapMaybe assignComment comSpans]
      in [ (key, pos, List.sortOn fst $ Map.findWithDefault [] key grouped)
         | (key, pos) <- stmtPosns
         , Map.member key grouped
         ]

    buildStmtAnn
      :: (Int, Int)  -- nodeStart (do-expression position)
      -> (AnnKey, (Int, Int), [((Int, Int), (String, RealSrcSpan))])
      -> [(AnnKey, Annotation)]
    buildStmtAnn doStart (key, stmtStart, comSpans) =
      let -- Use do-expression's position as initial reference so first
          -- comment gets a proper line delta (not DP(0,0)).
          initRef = doStart
          priorComs = snd $ List.mapAccumL (buildRelativeDP stmtStart) initRef comSpans
          entryDelta = case comSpans of
            [] -> DP (0, 0)
            _ -> let (_, (_, spanR)) = List.last comSpans
                     afterRef = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
                 in posToDP afterRef stmtStart
      in [(key, Ann
            { annCapturedSpan = Nothing
            , annSortKey = Nothing
            , annsDP = []
            , annFollowingComments = []
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
