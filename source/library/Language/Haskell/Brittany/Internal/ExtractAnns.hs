{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | Extract brittany's Anns from GHC 9.14 parsed AST (EpAnn).
-- Traverses the AST to collect EpAnn from module, imports, decls, and
-- nested expr/bind/stmt nodes, building the Map AnnKey Annotation format
-- expected by layouters.
module Language.Haskell.Brittany.Internal.ExtractAnns
  ( extractAnnsFromModule
  , recoverMissingComments
  ) where

import Control.Monad.Trans.State.Strict (State, get, put, runState)
import qualified Data.Char as Char
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
  , GRHS(..)
  , GrhsAnn
  , HsExpr(..)
  , HsLocalBindsLR(..)
  , HsModule(..)
  , HsSigType
  , HsType
  , HsUntypedSplice(..)
  , IE(..)
  , ImportDecl(..)
  , LIE
  , LImportDecl
  , LConDecl
  , LHsDecl
  , LHsExpr
  , LHsBind
  , LHsLocalBinds
  , LHsSigType
  , LHsType
  , LMatch
  , LPat
  , LGRHS
  , LTyFamInstDecl
  , LDataFamInstDecl
  , MatchGroup(..)
  , AnnsIf(..)
  , AnnsModule(..)
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
  , EpaComment(..)
  , EpaCommentTok(..)
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
import qualified GHC.Data.FastString as FastString
import qualified GHC.Data.Strict
import qualified GHC.Parser.Lexer
import GHC.Types.SrcLoc (EpaLocation'(..), RealSrcSpan, SrcSpan(..))
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.ExactPrintCompat
import Language.Haskell.Brittany.Internal.Prelude
import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Types as EPTypes
import qualified Language.Haskell.GHC.ExactPrint.Utils as EPUtils

-- | Recover comments from the source text that GHC 9.14's parser dropped.
-- GHC 9.14's parser sometimes fails to capture trailing line comments
-- (e.g., in multi-clause function definitions, pattern synonym where-clauses).
-- This scans the source for line comments, compares with comments already
-- in the parsed AST, and adds missing ones to the module annotation.
recoverMissingComments :: String -> String -> ParsedSource -> ParsedSource
recoverMissingComments src fp lmod@(L l p) =
  let quasiQuoteSpans = collectQuasiQuoteContentSpans lmod
      sourceComments = filter (not . isInsideQuasiQuote quasiQuoteSpans . fst)
        $ scanLineComments fp src
      astComments = collectAstComments lmod
      astPositions = Map.fromList [((SrcLoc.srcSpanStartLine sp, SrcLoc.srcSpanStartCol sp), ())
                                  | sp <- astComments]
      missing = [c | c <- sourceComments, not (Map.member (fst c) astPositions)]
  in if null missing then lmod
     else
       let modAnn = hsmodAnn (hsmodExt p)
       in case modAnn of
         EpAnn anc an cs ->
           let newComments = map mkLEpaComment' missing
               cs' = EPUtils.workInComments cs newComments
           in L l (p { hsmodExt = (hsmodExt p) { hsmodAnn = EpAnn anc an cs' }})
         _ -> lmod
  where
    isInsideQuasiQuote :: [RealSrcSpan] -> (Int, Int) -> Bool
    isInsideQuasiQuote spans position = any
      (\spanR -> position >= ss2pos spanR && position < ss2posEnd spanR)
      spans

    collectQuasiQuoteContentSpans :: ParsedSource -> [RealSrcSpan]
    collectQuasiQuoteContentSpans = SYB.everything (++) query
     where
      query :: SYB.GenericQ [RealSrcSpan]
      query = const [] `SYB.extQ` fromUntypedSplice

      fromUntypedSplice :: HsUntypedSplice GhcPs -> [RealSrcSpan]
      fromUntypedSplice splice = case splice of
        HsQuasiQuote _ _ content ->
          maybeToList $ SrcLoc.srcSpanToRealSrcSpan $ getLocA content
        _ -> []

    -- Scan source text for line comments (-- ...)
    -- Handles string literals and block comments to avoid false positives.
    scanLineComments :: String -> String -> [((Int, Int), String)]
    scanLineComments _fp s = List.concatMap scanLine (zip [1..] (List.lines s))
      where
        scanLine (lineNum, lineStr) =
          case findDashDash 1 False 0 lineStr of
            Nothing -> []
            Just (col, content) -> [((lineNum, col), content)]
        -- Find -- outside of strings and block comments
        findDashDash :: Int -> Bool -> Int -> String -> Maybe (Int, String)
        findDashDash _ _ _ [] = Nothing
        -- Inside string literal
        findDashDash col True bc ('"':rest) = findDashDash (col+1) False bc rest
        findDashDash col True bc ('\\':_:rest) = findDashDash (col+2) True bc rest
        findDashDash col True bc (_:rest) = findDashDash (col+1) True bc rest
        -- Block comment tracking
        findDashDash col False bc ('{':'-':rest) | bc >= 0 = findDashDash (col+2) False (bc+1) rest
        findDashDash col False bc ('-':'}':rest) | bc > 0 = findDashDash (col+2) False (bc-1) rest
        findDashDash col False bc (_:rest) | bc > 0 = findDashDash (col+1) False bc rest
        -- Normal code
        findDashDash col False 0 ('"':rest) = findDashDash (col+1) True 0 rest
        findDashDash col False 0 ('-':'-':rest)
          -- Must not be a symbolic operator (e.g., -->, --+)
          | null rest || not (isSymChar (head rest)) =
            Just (col, "--" ++ rest)
        findDashDash col False 0 (_:rest) = findDashDash (col+1) False 0 rest
        findDashDash col False bc (_:rest) = findDashDash (col+1) False bc rest

    isSymChar :: Char -> Bool
    isSymChar c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

    -- Collect all comment positions already in the AST by finding EpAnnComments
    collectAstComments :: ParsedSource -> [RealSrcSpan]
    collectAstComments ps =
      let extract :: SYB.GenericQ [RealSrcSpan]
          extract = const [] `SYB.extQ` extractFromComments
      in SYB.everything (++) extract ps

    extractFromComments :: EpAnnComments -> [RealSrcSpan]
    extractFromComments cs =
      [sp | L (EpaSpan (RealSrcSpan sp _)) _ <- priorComments cs ++ getFollowingComments cs]

    mkLEpaComment' :: ((Int, Int), String) -> LEpaComment
    mkLEpaComment' ((line, col), content) =
      let endCol = col + length content
          fs = FastString.mkFastString fp
          startLoc = SrcLoc.mkRealSrcLoc fs line col
          endLoc = SrcLoc.mkRealSrcLoc fs line endCol
          rss = SrcLoc.mkRealSrcSpan startLoc endLoc
          loc = EpaSpan (RealSrcSpan rss GHC.Data.Strict.Nothing)
          comment = EpaComment (EpaLineComment content) rss
      in L loc comment

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
      -- Get module header positions for comment splitting
      modHeaderEndLine = case hsmodAnn (hsmodExt mod') of
        EpAnn _ an _ -> case am_where an of
          EpTok loc -> fst (ss2posEnd (epaLocationRealSrcSpan loc))
          NoEpTok -> 0
        _ -> 0
      modKeywordLine = case hsmodAnn (hsmodExt mod') of
        EpAnn _ an _ -> case am_mod an of
          EpTok loc -> fst (ss2pos (epaLocationRealSrcSpan loc))
          NoEpTok -> 0
        _ -> 0
      -- Step 4: Distribute remaining module comments to child nodes
      sortedModComs = List.sortOn fst (modPriorComments ++ modFollowingComments)
      (modOwnComs0, childComAssignments) = distributeModuleComments allTargets sortedModComs
      -- Split modOwnComs: comments after 'where' should be prior on first child
      (trulyModComs, afterWhereComs) = case allTargets of
        ((firstK, _, _) : _) | modHeaderEndLine > 0 ->
          let (before, after) = List.partition (\((l, _), _) -> l <= modHeaderEndLine) modOwnComs0
          in case after of
            [] -> (before, [])
            _ -> (before, after)
        _ -> (modOwnComs0, [])
      -- Further split: comments BEFORE module keyword are true priors,
      -- comments between exports/module and where are following comments
      (modPriorOwnComs, modFollowOwnComs) = case modKeywordLine of
        0 -> (trulyModComs, [])
        ml -> List.partition (\((l, _), _) -> l < ml) trulyModComs
      modOwnComs = modPriorOwnComs
      afterWherePatches = case (afterWhereComs, allTargets) of
        (_:_, (firstK, _, _) : _) ->
          let cds = snd $ List.mapAccumL buildModComDP (modHeaderEndLine, 1) afterWhereComs
          in Map.singleton firstK cds
        _ -> Map.empty
      childPriorPatches = Map.unionWith (++) afterWherePatches $ Map.fromListWith (++)
        [(k, coms) | (k, PriorCom, coms) <- childComAssignments]
      childFollowingPatches = Map.fromListWith (++)
        [(k, coms) | (k, FollowingCom, coms) <- childComAssignments]
      -- Step 5: Extract annotations from the preprocessed AST
      modAnnsRaw = extractModuleHeaderAnns lmod' mod'
      importAnns = extractImportAnns (1, 1) (hsmodImports mod')
      exportAnns = case hsmodExports mod' of
        Nothing -> Map.empty
        Just llies -> extractIEListAnns ExportIEList llies
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
              -- Comments between exports and 'where' become following comments
              -- Use a reference point that gives correct DP relative to node end
              ownFollows = case modFollowOwnComs of
                [] -> []
                coms ->
                  -- For the first comment, compute DP from the export list end
                  -- Use (line, col-1) as ref so same-line comments get DP (0, 1+)
                  let ((firstLine, firstCol), _) = head coms
                      ref = (firstLine, firstCol - 1)
                  in snd $ List.mapAccumL buildModComDP ref coms
          in Map.singleton modKey (modAnn { annPriorComments = ownPriors
                                          , annFollowingComments = ownFollows })
      -- Step 8: Merge all annotations. declAnns' takes priority over
      -- nestedAnns for shared keys. Then do a second pass of inner comment
      -- redistribution for inner comments from declAnns' (which weren't
      -- processed by extractNestedEpAnns's redistribution pass).
      merged = modAnns <> importAnns' <> exportAnns <> declAnns' <> nestedAnns
      -- Build span map: nested spans + top-level decl spans
      nestedSpans = extractNestedSpanMap (hsmodDecls mod')
      declSpans = Map.fromList [(k, (s, e)) | (k, s, e) <- allTargets]
      fullSpanMap = nestedSpans <> declSpans
      -- Only process declAnns' keys with inner comments, and only those
      -- where inner-to-child redistribution is safe.
      -- Skip all other keys to avoid disrupting existing comment placement.
      declWithInner = Map.filterWithKey (\(AnnKey _ cn) ann ->
        not (null [() | (AnnComment _, _) <- annsDP ann])
        && unConName cn `elem` ["InstD", "TyClD", "ValD", "SigD"]
        ) declAnns'
      -- Also redistribute inner comments from MatchGroup annotations
      nestedWithInner = Map.filterWithKey (\(AnnKey _ cn) ann ->
        not (null [() | (AnnComment _, _) <- annsDP ann])
        && unConName cn `elem` ["MatchGroup"]
        ) nestedAnns
      allWithInner = declWithInner <> nestedWithInner
      nonDeclKeys = Map.fromList
        [(k, ()) | k <- Map.keys merged, not (Map.member k allWithInner)]
  in redistributeInnerCommentsWithChildSkips
    fullSpanMap nonDeclKeys Map.empty merged

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
                                                  `SYB.extM` addCommentsList
                                                  `SYB.extM` addCommentsMatchGroup) p0) allComments
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
    addCommentsMatchGroup :: SrcSpanAnnLW -> State [GHC.LEpaComment] SrcSpanAnnLW
    addCommentsMatchGroup = addComments
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
                (postDocs, nextPriors) = List.partition
                  isHaddockPostDoc priorForNext
                following = List.sortOn fst $ trailing ++ postDocs
                trailingCDs = if null following then []
                  else let cds = snd $ List.mapAccumL buildModComDP end following
                       in [(k, FollowingCom, cds)]
                priorCDs = case rest of
                  ((nk, _, _) : _) | not (null nextPriors) ->
                    let sorted = List.sortOn fst nextPriors
                        priorRef = case reverse following of
                          (_, (_, spanR)) : _ -> ss2posEnd spanR
                          [] -> end
                        cds = snd $ List.mapAccumL buildModComDP priorRef sorted
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

isHaddockPostDoc :: ((Int, Int), (String, RealSrcSpan)) -> Bool
isHaddockPostDoc (_, (content, _)) = case dropWhile Char.isSpace content of
  '-' : '-' : rest -> startsWithCaret rest
  '{' : '-' : rest -> startsWithCaret rest
  _ -> False
 where
  startsWithCaret = List.isPrefixOf "^" . dropWhile Char.isSpace

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
                Just (_, llies) -> extractIEListAnns ImportIEList llies
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
      -- Collect comments reassigned to the preceding declaration.
      followingPatches = Map.fromListWith (++)
        [(k, coms) | (_, Just (k, coms)) <- rawResults]
      merged = Map.mapWithKey (\k ann -> case Map.lookup k followingPatches of
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
              -- Split prior comments: those before declaration start are true
              -- priors, those at/after are inner (within the declaration body).
              actualPrior = List.filter
                (\((line, _), _) -> line < fst declStart) rest
              (postDocsForPrev, actualPrior') = case prevKey of
                Just _ -> List.partition isHaddockPostDoc actualPrior
                Nothing -> ([], actualPrior)
              innerComs = List.filter
                (\((line, _), _) -> line >= fst declStart) rest
              -- Top-level prior comments are emitted from column one. Keep the
              -- first comment's source column while avoiding an extra row move.
              priorRef = case actualPrior' of
                (((line, _), _) : _) -> (line, 1)
                [] -> declStart
              priorComs = snd $ List.mapAccumL buildComDP priorRef actualPrior'
              -- Entry delta: if there are prior comments, delta from
              -- last prior comment end to declaration start.
              entryDelta = case actualPrior' of
                [] -> DP (0, 0)
                _ -> let (_, (_, spanR)) = List.last actualPrior'
                         afterRef = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
                     in posToDP afterRef declStart
              -- Inner comments → annsDP as AnnComment entries so
              -- redistributeInnerComments can push them to children
              innerDP = snd $ List.mapAccumL buildInnerComDP declStart innerComs
              -- Following comments: relative to declaration end
              followComs = lepaToCommentsWithDP declEnd rawFollowing
              previousFollowing = List.sortOn fst
                $ trailingPrev ++ postDocsForPrev
              followingComs = snd
                $ List.mapAccumL buildComDP prevEnd previousFollowing
              followingPatch = case prevKey of
                Just pk
                  | not (null followingComs) -> Just (pk, followingComs)
                _ -> Nothing
              ann = Ann
                { annCapturedSpan = Nothing
                , annSortKey = Nothing
                , annsDP = innerDP
                , annFollowingComments = followComs
                , annPriorComments = priorComs
                , annEntryDelta = entryDelta
                }
          in ((declEnd, Just key), (Map.singleton key ann, followingPatch))

    buildComDP prev ((line, col), (content, spanR)) =
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

-- | Extract span map for all nested nodes. Used for inner comment
-- redistribution across the full annotation map.
extractNestedSpanMap :: [LHsDecl GhcPs] -> Map.Map AnnKey ((Int, Int), (Int, Int))
extractNestedSpanMap decls =
  let raw = SYB.everything (++) extractAll decls
  in Map.fromList [(k, (ss2pos sp, ss2posEnd sp)) | (k, sp, _) <- raw]
  where
    extractAll :: SYB.GenericQ [(AnnKey, RealSrcSpan, EpAnnComments)]
    extractAll =
      (const []
        `SYB.extQ` (extractFromLocatedWithLoc :: LHsExpr GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)])
        `SYB.extQ` (extractFromLocated :: LHsDecl GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)])
        `SYB.extQ` (extractFromLocatedWithLoc :: LHsBind GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)])
        `SYB.extQ` (extractFromLocatedWithLoc :: LMatch GhcPs (LHsExpr GhcPs) -> [(AnnKey, RealSrcSpan, EpAnnComments)])
        `SYB.extQ` (extractFromLocated :: LGRHS GhcPs (LHsExpr GhcPs) -> [(AnnKey, RealSrcSpan, EpAnnComments)])
        `SYB.extQ` (extractFromLocatedWithLoc :: ExprLStmt GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)])
        `SYB.extQ` (extractFromLocatedWithLoc :: LHsType GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)])
        `SYB.extQ` (extractFromLocatedWithLoc :: LHsSigType GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)])
        `SYB.extQ` (extractFromLocatedWithLoc :: LPat GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)])
        `SYB.extQ` (extractFromLocatedWithLoc :: LConDecl GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)])
        `SYB.extQ` (extractFromLocatedWithLoc :: LTyFamInstDecl GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)])
        `SYB.extQ` (extractFromLocatedWithLoc :: LDataFamInstDecl GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)])
      )

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
      extractLHsBind = extractFromLocatedWithLoc
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
      extractLConDecl :: LConDecl GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)]
      extractLConDecl = extractFromLocatedWithLoc
      extractLTyFamInst :: LTyFamInstDecl GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)]
      extractLTyFamInst = extractFromLocatedWithLoc
      extractLDataFamInst :: LDataFamInstDecl GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)]
      extractLDataFamInst = extractFromLocatedWithLoc
      -- Extract from HsLocalBindsLR extension field (EpAnn (AnnList ()))
      -- which receives comments from redistributeIntraDeclComments
      extractHsLocalBinds :: HsLocalBindsLR GhcPs GhcPs -> [(AnnKey, RealSrcSpan, EpAnnComments)]
      extractHsLocalBinds (HsValBinds (EpAnn anc _ cs) _) =
        case anc of
          EpaSpan (RealSrcSpan rss _) ->
            let key = AnnKey [realSpanToSrcSpan rss] (CN "HsValBinds")
            in [(key, rss, cs)]
          _ -> []
      extractHsLocalBinds _ = []
      -- Extract comments from MatchGroup's mg_alts wrapper (SrcSpanAnnLW).
      -- ghc-exactprint may place comments on this AnnList (EpToken "where")
      -- annotation which none of the standard extractors handle.
      extractMatchGroup :: MatchGroup GhcPs (LHsExpr GhcPs) -> [(AnnKey, RealSrcSpan, EpAnnComments)]
      extractMatchGroup (MG _ (L (EpAnn anc _ cs) _)) =
        let coms = priorComments cs ++ getFollowingComments cs
        in if null coms then []
           else case anc of
             EpaSpan (RealSrcSpan rss _) ->
               let key = AnnKey [realSpanToSrcSpan rss] (CN "MatchGroup")
               in [(key, rss, cs)]
             _ -> []
      extractMatchGroup _ = []
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
          `SYB.extQ` extractLConDecl
          `SYB.extQ` extractLTyFamInst
          `SYB.extQ` extractLDataFamInst
          `SYB.extQ` extractHsLocalBinds
          `SYB.extQ` extractMatchGroup
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
        HsIf xIf condExpr thenExpr elseExpr ->
          extractHsIfAnns lexpr loc xIf condExpr thenExpr elseExpr
        HsDo _ _ lstmts@(L stmtLoc stmts) ->
          extractHsDoAnns lexpr loc stmtLoc stmts
        _ -> []
      overrideGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> [(AnnKey, Annotation)]
      overrideGRHS lgrhs@(L _loc (GRHS xgrhs _guards body)) =
        extractGRHSAnns lgrhs xgrhs body
      overrideGRHS _ = []
      overrideExtract :: SYB.GenericQ [(AnnKey, Annotation)]
      overrideExtract = const [] `SYB.extQ` overrideExpr `SYB.extQ` overrideGRHS
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
redistributeInnerComments spanMap skipKeys =
  redistributeInnerCommentsWithChildSkips spanMap skipKeys skipKeys

redistributeInnerCommentsWithChildSkips
  :: Map.Map AnnKey ((Int, Int), (Int, Int))
  -> Map.Map AnnKey ()
  -> Map.Map AnnKey ()
  -> Anns
  -> Anns
redistributeInnerCommentsWithChildSkips spanMap skipKeys childSkipKeys anns =
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
      assignments = List.concatMap (\(parentKey@(AnnKey _ cn), (pStart, pEnd, coms)) ->
        let children = List.sortOn (\(_, s, _) -> s)
              [ (k, s, e) | (k, s, e) <- allChildren
              , k /= parentKey
              , not (Map.member k childSkipKeys)
              , s >= pStart && e <= pEnd
              ]
            -- For declaration-level parents (InstD, TyClD), inter-child
            -- comments are typically haddock for the next child (prior).
            -- For other parents, comments follow the previous child.
            isDeclParent = unConName cn `elem` ["InstD", "TyClD", "HsValBinds"]
        in assignCommentsToChildren isDeclParent children coms
        ) parentInnerComs
      -- Group assignments by target key, separating prior vs following
      followingPatches = Map.fromListWith (flip (++))
        [(k, coms) | (k, False, coms) <- assignments]
      priorPatches = Map.fromListWith (flip (++))
        [(k, coms) | (k, True, coms) <- assignments]
      -- Track which parents had comments successfully redistributed
      parentKeys = Map.fromList [(pk, ()) | (pk, _) <- parentInnerComs]
      -- Create default annotations for target keys not already in anns
      defaultAnn = Ann Nothing Nothing [] [] [] (DP (0,0))
      newTargetKeys = Map.fromList
        [ (k, defaultAnn)
        | k <- Map.keys followingPatches ++ Map.keys priorPatches
        , not (Map.member k anns)
        ]
      annsWithTargets = anns `Map.union` newTargetKeys
      -- Apply patches: add comments to children, remove from parents
      patched = Map.mapWithKey (\k ann ->
        let addFollow = Map.findWithDefault [] k followingPatches
            addPrior = Map.findWithDefault [] k priorPatches
            -- Strip inner AnnComment entries from parents that had redistribution
            strippedDP = if Map.member k parentKeys
              then List.filter (\x -> case x of { (AnnComment _, _) -> False; _ -> True }) (annsDP ann)
              else annsDP ann
            -- When adding prior comments, update entry delta to ensure a newline
            -- between the last prior comment and the node content
            updatedDelta = if null addPrior then annEntryDelta ann
              else DP (1, 0)
        in ann { annFollowingComments = annFollowingComments ann ++ addFollow
               , annPriorComments = addPrior ++ annPriorComments ann
               , annsDP = strippedDP
               , annEntryDelta = updatedDelta
               }
        ) annsWithTargets
  in patched
  where
    -- | Assign each inner comment to the child whose end is closest before
    -- or at the comment's position. If no child qualifies, leave unassigned
    -- (comment stays on parent as annsDP).
    assignCommentsToChildren
      :: Bool  -- ^ True for decl-level parents (prefer afterCandidates for non-same-line)
      -> [(AnnKey, (Int, Int), (Int, Int))]  -- sorted children
      -> [(Comment, DeltaPos)]               -- inner comments
      -> [(AnnKey, Bool, [(Comment, DeltaPos)])]   -- (key, isPrior, comments)
    assignCommentsToChildren isDeclParent children coms =
      mapMaybe (assignOneComment isDeclParent children) coms

    assignOneComment
      :: Bool
      -> [(AnnKey, (Int, Int), (Int, Int))]
      -> (Comment, DeltaPos)
      -> Maybe (AnnKey, Bool, [(Comment, DeltaPos)])
    assignOneComment isDeclParent children (com, _oldDP) =
      case srcSpanToRealSpan (commentIdentifier com) of
        Nothing -> Nothing
        Just comSpan ->
          let comPos = ss2pos comSpan
              -- Find the child whose end is closest to (but before/at) comPos.
              candidates = List.sortOn (\(_, e) -> (fst comPos - fst e, snd comPos - snd e))
                [(k, e) | (k, _, e) <- children, e <= comPos]
              -- Fallback: if no child ends before comment, assign as prior to
              -- the first child that starts after the comment.
              afterCandidates = List.sortOn (\(_, s, _) -> s)
                [(k, s, e) | (k, s, e) <- children, s > comPos]
              -- Same-line candidates: comment on same line as a child's end
              -- Among same-end candidates, prefer the one with latest start (smallest span)
              sameLineCandidates0 = [(k, e) | (k, e) <- candidates, fst e == fst comPos]
              sameLineCandidates = case sameLineCandidates0 of
                [] -> []
                cs -> let bestEnd = snd (head cs)
                          sameEnd = [(k, e) | (k, e) <- cs, e == bestEnd]
                      in case sameEnd of
                        [_] -> sameEnd
                        _ ->
                          -- Multiple candidates with same end: pick the one with latest start (most specific)
                          let withStart = [(k, e, s) | (k, s, _) <- children, (k2, e) <- sameEnd, k == k2]
                          in case List.sortOn (\(_, _, s) -> (negate (fst s), negate (snd s))) withStart of
                            ((k, e, _) : _) -> [(k, e)]
                            [] -> sameEnd
          in case sameLineCandidates of
            ((bestKey, bestEnd) : _) ->
              -- Trailing comment on same line as child end
              let dp = posToDP bestEnd comPos
              in Just (bestKey, False, [(com, dp)])
            [] | isDeclParent ->
              -- For decl-level parents, prefer assigning to next child as prior
              case afterCandidates of
                ((bestKey, _bestStart, _) : _) ->
                  let dp = DP (1, snd comPos - snd _bestStart)
                  in Just (bestKey, True, [(com, dp)])
                [] -> case candidates of
                  ((bestKey, bestEnd) : _) ->
                    let dp = posToDP bestEnd comPos
                    in Just (bestKey, False, [(com, dp)])
                  [] -> Nothing
            [] ->
              -- For other parents, prefer assigning to previous child as following
              case candidates of
                ((bestKey, bestEnd) : _) ->
                  let dp = posToDP bestEnd comPos
                  in Just (bestKey, False, [(com, dp)])
                [] -> case afterCandidates of
                  ((bestKey, _bestStart, _) : _) ->
                    let dp = DP (1, snd comPos - snd _bestStart)
                    in Just (bestKey, True, [(com, dp)])
                  [] -> Nothing

-- | Extract annotations for IE (import/export) list container and items.
-- Handles both import lists (ideclImportList) and export lists (hsmodExports).
data IEListContext = ImportIEList | ExportIEList deriving Eq

extractIEListAnns :: IEListContext -> GenLocated (EpAnn ann) [LIE GhcPs] -> Anns
extractIEListAnns listContext llies@(L epann lies) =
  let -- Get IE item positions for comment redistribution
      iePositions = mapMaybe (\lie -> case lie of
        L (EpAnn anc _ _) _ ->
          let sp = epaLocationRealSrcSpan anc
          in Just (mkAnnKeyL lie, ss2pos sp, ss2posEnd sp)
        _ -> Nothing
        ) lies
      -- Extract container comments and redistribute to IE items
      (containerAnns, containerPriorPatches, containerFollowPatches) = case epann of
        EpAnn anc _ cs ->
          let ancSpan = epaLocationRealSrcSpan anc
              key = mkAnnKeyL llies
              ref = ss2pos ancSpan
              rawPriors = List.sortOn fst $ List.concatMap lepaToSpanAndContent (priorComments cs)
              rawFollows = List.sortOn fst $ List.concatMap lepaToSpanAndContent (getFollowingComments cs)
              -- Distribute container's prior comments to IE items
              (ownPriors, priorFromPriors, followFromPriors) = distributeContainerCommentsToIE iePositions rawPriors
              (ownFollows, priorFromFollows, followFromFollows) = distributeContainerCommentsToIE iePositions rawFollows
              -- Shift ref by 1 to account for "(" written before comments
              ownPriorRef = (fst ref, snd ref + 1)
              ownPriorComs = snd $ List.mapAccumL buildModComDP ownPriorRef ownPriors
              ownFollowComs = snd $ List.mapAccumL buildModComDP ref ownFollows
              ann = Ann Nothing Nothing [] ownFollowComs ownPriorComs (posToDP ref (ss2pos ancSpan))
          in (Map.singleton key ann, priorFromPriors ++ priorFromFollows, followFromPriors ++ followFromFollows)
        _ -> (Map.empty, [], [])
      containerRef = case epann of
        EpAnn anc _ _ -> ss2pos (epaLocationRealSrcSpan anc)
        _ -> (1, 1)
      initial = (containerRef, Nothing) :: ((Int, Int), Maybe AnnKey)
      (_, rawItems) = List.mapAccumL extractIEItem initial lies
      itemAnns = Map.fromList [(k, ann) | (k, ann, _) <- rawItems]
      trailingPatches = Map.fromListWith (++)
        [(pk, coms) | (_, _, Just (pk, coms)) <- rawItems]
      -- Merge all patches
      allFollowPatches = Map.fromListWith (++)
        (trailingPatches' ++ containerFollowPatches)
      allPriorPatches = Map.fromListWith (++) containerPriorPatches
      trailingPatches' = Map.toList trailingPatches
      mergedItems = Map.mapWithKey (\k ann ->
        let fp = Map.findWithDefault [] k allFollowPatches
            pp = Map.findWithDefault [] k allPriorPatches
        in ann { annFollowingComments = annFollowingComments ann ++ fp
               , annPriorComments = pp ++ annPriorComments ann
               }
        ) itemAnns
      anns = containerAnns <> mergedItems
  in if listContext == ExportIEList
    then Map.map markHaddockSections anns
    else anns
  where
    markHaddockSections ann = ann
      { annFollowingComments = map markSection (annFollowingComments ann)
      , annPriorComments = map markSection (annPriorComments ann)
      }

    markSection commentAndDP@(comment, dp)
      | isHaddockSectionComment comment =
          (comment { commentOrigin = Just AnnHaddockSection }, dp)
      | otherwise = commentAndDP

    isHaddockSectionComment comment =
      case dropWhile Char.isSpace (commentContents comment) of
        '-' : '-' : rest -> case dropWhile Char.isSpace rest of
          '*' : _ -> True
          _ -> False
        _ -> False

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

-- | Distribute container comments to the nearest IE item.
-- A comment between two items goes as a following comment on the previous item.
-- Comments before the first item stay with the container.
distributeContainerCommentsToIE
  :: [(AnnKey, (Int, Int), (Int, Int))]  -- IE item (key, start, end)
  -> [((Int, Int), (String, RealSrcSpan))]  -- sorted comments
  -> ( [((Int, Int), (String, RealSrcSpan))]  -- own (container) comments
     , [(AnnKey, [(Comment, DeltaPos)])]  -- prior patches
     , [(AnnKey, [(Comment, DeltaPos)])]  -- following patches
     )
distributeContainerCommentsToIE iePositions comSpans =
  case iePositions of
    [] -> (comSpans, [], [])  -- no items, all comments stay with container
    _ ->
      let assignComment comSpan@((comLine, _comCol), _) =
            -- Trailing: on same line as some item's end
            case List.find (\(_, _, (endLine, _)) -> comLine == endLine) iePositions of
              Just (key, _, endPos) -> Right (key, endPos, comSpan, True)
              Nothing ->
                -- Between items: assign as following on the previous item
                case reverse $ takeWhile (\(_, _, (endLine, _)) -> endLine < comLine) iePositions of
                  ((key, _, endPos) : _) -> Right (key, endPos, comSpan, True)
                  _ ->
                    -- Before first item: stays with container
                    Left comSpan
          results = map assignComment comSpans
          ownComs = [c | Left c <- results]
          followGroups = Map.fromListWith (++)
            [(k, [(pos, c)]) | Right (k, pos, c, True) <- results]
          buildPatches groups = do
            (k, items) <- Map.toList groups
            let sorted = List.sortOn (fst . snd) items
                ref = fst (head sorted)
                coms = snd $ List.mapAccumL buildModComDP ref (map snd sorted)
            [(k, coms)]
      in (ownComs, [], buildPatches followGroups)

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
  -> LHsExpr GhcPs      -- condition-expression
  -> LHsExpr GhcPs      -- then-expression
  -> LHsExpr GhcPs      -- else-expression
  -> [(AnnKey, Annotation)]
extractHsIfAnns lexpr locAnn annsIf condExpr thenExpr elseExpr =
  let ifKey = mkAnnKeyL lexpr
      condKey = mkAnnKeyL condExpr
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
          (genuinePriorSpans, innerComSpans0) = List.partition
            (\((line, _), _) -> line < fst nodeStart) allComSpans
          -- Comments on nodeStart line but before "then" keyword are condition
          -- trailing comments → followingComments on HsIf, not inner
          (condTrailingSpans, innerComSpans) = case thenPos of
            Just tp -> List.partition
              (\((line, _), _) -> line == fst nodeStart && line < fst tp)
              innerComSpans0
            Nothing -> ([], innerComSpans0)
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
          condEnd = case srcSpanToRealSpan (getLocA condExpr) of
            Just rsp -> ss2posEnd rsp
            Nothing -> nodeStart
          condTrailingComs = snd $ List.mapAccumL buildModComDP condEnd condTrailingSpans
          followComs = lepaToCommentsWithDP nodeEnd (getFollowingComments locCs)
          ifAnn = Ann
            { annCapturedSpan = Nothing
            , annSortKey = Nothing
            , annsDP = []
            , annFollowingComments = followComs
            , annPriorComments = genuinePriorComs
            , annEntryDelta = entryDelta
            }
          -- Build condition annotation with trailing comments
          condAnn = if null condTrailingComs then Nothing
            else Just Ann
              { annCapturedSpan = Nothing
              , annSortKey = Nothing
              , annsDP = []
              , annFollowingComments = condTrailingComs
              , annPriorComments = []
              , annEntryDelta = DP (0, 0)
              }
          -- Build child annotations with redistributed comments
          thenChildAnn = buildChildAnn thenPos thenComSpans thenExpr
          elseChildAnn = buildChildAnn elsePos elseComSpans elseExpr
      in [(ifKey, ifAnn)]
         ++ maybe [] (\a -> [(condKey, a)]) condAnn
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
          -- Use previous statement's end as reference for prior comment DPs
          stmtEnds = Map.fromList
            [(k2, e1)
            | ((_, _, e1), (k2, _, _)) <- zip stmtPosns (drop 1 stmtPosns)]
          childAnns = List.concatMap (\a@(key, _, _, _, _) ->
            let prevStmtEnd = Map.findWithDefault nodeStart key stmtEnds
            in buildStmtAnn prevStmtEnd a
            ) assignments
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

    -- | Build a (Comment, DP) for a comment on a do-block statement.
    -- For y > 0 (different line), x = comCol - stmtCol (relative to statement
    -- indent). layoutMoveToCommentPos computes: column = indLevelLinger + x.
    -- After docSetBaseAndIndent, indLevelLinger matches the statement indent,
    -- so the comment ends up at the correct absolute column.
    -- For y = 0 (same line), x = col offset from prev position.
    buildRelativeDP
      :: (Int, Int) -> (Int, Int)
      -> ((Int, Int), (String, RealSrcSpan))
      -> ((Int, Int), (Comment, DeltaPos))
    buildRelativeDP stmtStart prev ((comLine, comCol), (content, spanR)) =
      let rawDp = posToDP prev (comLine, comCol)
          dp = case rawDp of
            DP (y, _x) | y > 0 ->
              DP (y, comCol - snd stmtStart)
            _ -> rawDp
          nextPos = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
          bComment = Comment
            { commentOrigin = Nothing
            , commentIdentifier = realSpanToSrcSpan spanR
            , commentContents = content
            }
      in (nextPos, (bComment, dp))

-- | Extract override annotations for GRHS nodes.
-- Inner comments before the body expression should be prior comments on the
-- body, not trailing comments emitted after it by BDAnnotationRest.
extractGRHSAnns
  :: LGRHS GhcPs (LHsExpr GhcPs)
  -> EpAnn GrhsAnn       -- extension field annotation (has comments)
  -> LHsExpr GhcPs       -- body expression
  -> [(AnnKey, Annotation)]
extractGRHSAnns lgrhs xAnn body =
  let grhsKey = mkAnnKeyL lgrhs
      bodyKey = mkAnnKeyL body
  in case xAnn of
    EpAnn locAnc _ locCs ->
      let ancSpan = epaLocationRealSrcSpan locAnc
          nodeStart = ss2pos ancSpan
          nodeEnd = ss2posEnd ancSpan
          -- Get body expression position
          bodyStart = case srcSpanToRealSpan (getLocA body) of
            Just rsp -> ss2pos rsp
            Nothing -> nodeEnd
          -- Collect all comments from location annotation
          rawPriors = priorComments locCs
          rawFollowing = getFollowingComments locCs
          allPriorSpans = List.sortOn fst $ List.concatMap lepaToSpanAndContent rawPriors
          allFollowSpans = List.sortOn fst $ List.concatMap lepaToSpanAndContent rawFollowing
          allComSpans = List.sortOn fst (allPriorSpans ++ allFollowSpans)
          -- Split: genuine prior (before GRHS start) vs inner (at/after start)
          (genuinePriorSpans, innerComSpans) = List.partition
            (\((line, _), _) -> line < fst nodeStart) allComSpans
          -- Split inner: before body (to be prior on body) vs at/after body (trailing)
          (beforeBody, atOrAfterBody) = List.partition
            (\((line, _), _) -> line < fst bodyStart) innerComSpans
          -- Further split at/after: same-line trailing vs true inner
          bodyEnd = case srcSpanToRealSpan (getLocA body) of
            Just rsp -> ss2posEnd rsp
            Nothing -> nodeEnd
          (sameLineTrailing, trueInner) = List.partition
            (\((line, _), _) -> line == fst bodyEnd) atOrAfterBody
          -- Build GRHS annotation
          genuinePriorComs = lepaToCommentsWithDP nodeStart
            (List.filter (\lc -> all (\((line, _), _) -> line < fst nodeStart) (lepaToSpanAndContent lc)) rawPriors)
          entryDelta = case genuinePriorSpans of
            [] -> DP (0, 0)
            _ -> let (_, (_, spanR)) = List.last genuinePriorSpans
                     afterRef = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
                 in posToDP afterRef nodeStart
          -- True inner comments → annsDP for BDAnnotationRest
          trailingDP = snd $ List.mapAccumL buildInnerComDP' nodeStart trueInner
          -- Same-line trailing → followingComments with DP relative to bodyEnd
          sameLineFollowComs = snd $ List.mapAccumL buildModComDP bodyEnd sameLineTrailing
          grhsAnn = Ann
            { annCapturedSpan = Nothing
            , annSortKey = Nothing
            , annsDP = trailingDP
            , annFollowingComments = sameLineFollowComs
            , annPriorComments = genuinePriorComs
            , annEntryDelta = entryDelta
            }
          -- Before-body comments become prior comments on the body expression
          -- Use absolute column (0-indexed) for comment x-position since we
          -- don't know the backend's indent level at this point
          bodyPriorComs = snd $ List.mapAccumL (buildBodyRelDP bodyStart) nodeStart beforeBody
          bodyAnn = if null bodyPriorComs then Nothing
            else Just $ Ann
              { annCapturedSpan = Nothing
              , annSortKey = Nothing
              , annsDP = []
              , annFollowingComments = []
              , annPriorComments = bodyPriorComs
              , annEntryDelta = DP (1, 0)
              }
      in [(grhsKey, grhsAnn)] ++ maybe [] (\a -> [(bodyKey, a)]) bodyAnn
    _ -> []
  where
    buildInnerComDP' prev ((line, col), (content, spanR)) =
      let dp = posToDP prev (line, col)
          nextPos = (SrcLoc.srcSpanEndLine spanR, SrcLoc.srcSpanEndCol spanR)
          bComment = Comment
            { commentOrigin = Nothing
            , commentIdentifier = realSpanToSrcSpan spanR
            , commentContents = content
            }
      in (nextPos, (AnnComment bComment, dp))

    -- Build DP for prior comments on the body expression, relative to
    -- the body start column. layoutMoveToCommentPos computes
    -- addSepSpace = indLevelLinger + x, so x should be the offset from
    -- the body column (which will be at indLevelLinger).
    buildBodyRelDP
      :: (Int, Int)  -- body start position
      -> (Int, Int)  -- prev position (accumulator)
      -> ((Int, Int), (String, RealSrcSpan))
      -> ((Int, Int), (Comment, DeltaPos))
    buildBodyRelDP bStart prev ((comLine, comCol), (content, spanR)) =
      let rawDp = posToDP prev (comLine, comCol)
          dp = case rawDp of
            DP (y, _x) | y > 0 ->
              DP (y, comCol - snd bStart)  -- relative to body column
            _ -> rawDp
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
