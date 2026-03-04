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
  ( HsExpr(..)
  , HsModule(..)
  , ImportDecl(..)
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
      declAnns = extractDeclAnns (hsmodDecls mod')
      nestedAnns = extractNestedEpAnns (hsmodDecls mod')
      result = modAnns <> importAnns <> declAnns <> nestedAnns
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
      case maybeImportEpAnn (unLoc limport) of
        Nothing -> (prevEnd, Map.empty)
        Just (anc, cs) ->
          let ancSpan = epaLocationRealSrcSpan anc
              key = mkAnnKeyL limport
              ann = buildAnnotation prevEnd ancSpan cs
              prevEnd' = ss2posEnd ancSpan
          in (prevEnd', Map.singleton key ann)

extractDeclAnns :: [LHsDecl GhcPs] -> Anns
extractDeclAnns decls = mconcat $ map extractOne decls
  where
    extractOne ldecl =
      case maybeDeclEpAnn ldecl of
        Nothing -> Map.empty
        Just (anc, cs) ->
          let ancSpan = epaLocationRealSrcSpan anc
              key = mkAnnKeyL ldecl
              ref = ss2pos ancSpan
              -- Use DP (0, 0) for entry delta: ppModule handles inter-declaration
              -- spacing via explicit newlines; a non-zero annEntryDelta would
              -- double-count when docWrapNodePrior calls moveToExactAnn.
              ann = Ann
                { annCapturedSpan = Nothing
                , annSortKey = Nothing
                , annsDP = []
                , annFollowingComments = lepaToCommentsWithDP ref (getFollowingComments cs)
                , annPriorComments = lepaToCommentsWithDP ref (priorComments cs)
                , annEntryDelta = DP (0, 0)
                }
          in Map.singleton key ann

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
      let ref = ss2pos ancSpan
          ann = Ann
            { annCapturedSpan = Nothing
            , annSortKey = Nothing
            , annsDP = []
            , annFollowingComments = lepaToCommentsWithDP ref (getFollowingComments cs)
            , annPriorComments = lepaToCommentsWithDP ref (priorComments cs)
            , annEntryDelta = DP (0, 0)
            }
      in (key, ann)

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

posToDP :: (Int, Int) -> (Int, Int) -> DeltaPos
posToDP (prevL, prevC) (curL, curC)
  | curL == prevL = DP (0, curC - prevC)
  | otherwise = DP (curL - prevL, curC - 1)

ss2pos :: RealSrcSpan -> (Int, Int)
ss2pos s = (SrcLoc.srcSpanStartLine s, SrcLoc.srcSpanStartCol s)

ss2posEnd :: RealSrcSpan -> (Int, Int)
ss2posEnd s = (SrcLoc.srcSpanEndLine s, SrcLoc.srcSpanEndCol s)
