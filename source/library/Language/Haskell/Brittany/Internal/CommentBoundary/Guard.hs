{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.CommentBoundary.Guard
  ( GuardRegion(..)
  , GuardIndex
  , buildGuardIndex
  , guardBoundary
  , materializeGuardComments
  ) where

import qualified Data.Char as Char
import qualified Data.Generics as Generics
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC (GenLocated(L), HsModule, unLoc)
import GHC.Hs
  ( GRHS(..), GRHSs(..), GrhsAnn(..), HsBindLR(..), HsExpr(..)
  , HsMatchContext(..), LGRHS, LHsExpr, LMatch, Match(..)
  )
import GHC.Parser.Annotation (EpAnn(..), getHasLoc, getLocA)
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.ExactPrintCompat
import Language.Haskell.Brittany.Internal.ExtractAnns.GuardComments
  ( guardGroupComments )
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types

data GuardRegion = GuardRegion
  { guardPrecedingSpan :: SrcLoc.RealSrcSpan
  , guardBarSpan :: SrcLoc.RealSrcSpan
  , guardOwner :: AnnKey
  }

newtype GuardIndex = GuardIndex [(Int, GuardRegion)]

buildGuardIndex :: HsModule GhcPs -> GuardIndex
buildGuardIndex = GuardIndex . zip [0 ..]
  . List.sortOn (spanStart . guardBarSpan)
  . Generics.everything (++)
      (Generics.mkQ [] matchRegions
        `Generics.extQ` bindingRegions
        `Generics.extQ` expressionRegions)

matchRegions :: LMatch GhcPs (LHsExpr GhcPs) -> [GuardRegion]
matchRegions match@(L _ Match { m_pats = patterns, m_ctxt = context, m_grhss = rhs }) =
  maybe [] (`rhsRegions` NonEmpty.toList (grhssGRHSs rhs)) preceding
 where
  preceding = case reverse $ unLoc patterns of
    pattern' : _ -> srcSpanToRealSpan $ getLocA pattern'
    [] -> case context of
      FunRhs functionId _ _ _ -> srcSpanToRealSpan $ getLocA functionId
      _ -> do
        matchSpan <- srcSpanToRealSpan $ getLocA match
        pure $ SrcLoc.mkRealSrcSpan
          (SrcLoc.realSrcSpanStart matchSpan) (SrcLoc.realSrcSpanStart matchSpan)

bindingRegions :: HsBindLR GhcPs GhcPs -> [GuardRegion]
bindingRegions PatBind { pat_lhs = pattern', pat_rhs = rhs } =
  maybe [] (`rhsRegions` NonEmpty.toList (grhssGRHSs rhs))
    $ srcSpanToRealSpan $ getLocA pattern'
bindingRegions _ = []

expressionRegions :: HsExpr GhcPs -> [GuardRegion]
expressionRegions (HsMultiIf (keyword, _, _) guarded) =
  maybe [] (`rhsRegions` NonEmpty.toList guarded)
    $ srcSpanToRealSpan $ getHasLoc keyword
expressionRegions _ = []

rhsRegions
  :: SrcLoc.RealSrcSpan -> [LGRHS GhcPs (LHsExpr GhcPs)] -> [GuardRegion]
rhsRegions _ [] = []
rhsRegions preceding (guarded@(L _ (GRHS annotations statements body)) : remaining) =
  maybeToList current ++ maybe [] (`rhsRegions` remaining) bodySpan
 where
  bodySpan = srcSpanToRealSpan $ getLocA body
  current = do
    guard $ not $ null statements
    bar <- ga_vbar $ anns annotations
    barSpan <- srcSpanToRealSpan $ getHasLoc bar
    pure $ GuardRegion preceding barSpan
      (mkAnnKey $ L (getLocA guarded) $ unLoc guarded)

-- Placement identifies the ordinary suffix selected during materialization.
-- Other comments can occupy the same source gap while retaining their owner.
guardBoundary
  :: GuardIndex
  -> CommentPlacement
  -> SrcLoc.RealSrcSpan
  -> Maybe CommentBoundaryId
guardBoundary (GuardIndex regions) placement commentSpan = listToMaybe
  [ CommentBoundaryId (GuardBoundaryPath index) BeforeBoundary
  | (index, region) <- regions
  , placementOwner placement == NodeId (guardOwner region)
  , placementAnchor placement == BeforeNode
  , placementRole placement == LeadingOrdinary
  , placementLineRelation placement == CommentOwnLine
  , ownLineBeforeBar region commentSpan
  ]

-- The final head pattern is an AST child; the guard pipe is not. Move only
-- ordinary own-line runs to the GRHS before lowering computes their placement.
materializeGuardComments :: HsModule GhcPs -> Anns -> Anns
materializeGuardComments module' annotations =
  foldl relocate annotations regions
 where
  GuardIndex regions = buildGuardIndex module'
  entries = distinctEntries
    $ List.sortOn (spanStart . snd)
    $ guardGroupComments module' ++
    [ (comment, commentSpan)
    | annotation <- Map.elems annotations
    , comment <- annotationComments annotation
    , Just commentSpan <- [srcSpanToRealSpan $ commentIdentifier comment]
    ]
  relocate current (_, region)
    | null selected = current
    | otherwise = Map.alter
        (Just . attach . fromMaybe emptyAnnotation)
        (guardOwner region) $ removeComments selected current
   where
    within = filter (inGap region . snd) entries
    runs = commentRuns within
    -- Earlier ordinary prose cannot cross a protected run that remains on the
    -- head or previous RHS. Preserve that prefix and relocate only the suffix.
    selected = List.concat $ reverse $ takeWhile movable $ reverse runs
    movable run = not (null run)
      && all (ownLineBeforeBar region . snd) run
      && all (not . protectedComment . fst) run
    attach annotation = annotation
      { annPriorComments = snd $ mapAccumL rebase preceding priorEntries }
     where
      priorEntries = distinctEntries $ List.sortOn (spanStart . snd)
        $ selected ++
          [ (comment, commentSpan)
          | (comment, _) <- annPriorComments annotation
          , Just commentSpan <- [srcSpanToRealSpan $ commentIdentifier comment]
          ]
      preceding = case priorEntries of
        [] -> spanEnd $ guardPrecedingSpan region
        (_, firstSpan) : _ -> foldl max (spanEnd $ guardPrecedingSpan region)
          [ spanEnd retainedSpan
          | (_, retainedSpan) <- within
          , spanEnd retainedSpan <= spanStart firstSpan
          ]

inGap :: GuardRegion -> SrcLoc.RealSrcSpan -> Bool
inGap region commentSpan =
  SrcLoc.srcSpanFile (guardBarSpan region) == SrcLoc.srcSpanFile commentSpan
    && spanEnd (guardPrecedingSpan region) <= spanStart commentSpan
    && spanEnd commentSpan <= spanStart (guardBarSpan region)

ownLineBeforeBar :: GuardRegion -> SrcLoc.RealSrcSpan -> Bool
ownLineBeforeBar region commentSpan = inGap region commentSpan
  && SrcLoc.srcSpanStartLine commentSpan
    > SrcLoc.srcSpanEndLine (guardPrecedingSpan region)
  && SrcLoc.srcSpanEndLine commentSpan
    < SrcLoc.srcSpanStartLine (guardBarSpan region)

-- Inline-seeded continuations and documentation/pragma continuations remain
-- whole runs. Blank lines provide an unambiguous boundary for ordinary prose.
commentRuns :: [(Comment, SrcLoc.RealSrcSpan)] -> [[(Comment, SrcLoc.RealSrcSpan)]]
commentRuns [] = []
commentRuns (first : remaining) = collect [first] (snd first) remaining
 where
  collect run _ [] = [reverse run]
  collect run previous rest@(current@(_, currentSpan) : following)
    | SrcLoc.srcSpanStartLine currentSpan <= SrcLoc.srcSpanEndLine previous + 1 =
        collect (current : run) currentSpan following
    | otherwise = reverse run : commentRuns rest

protectedComment :: Comment -> Bool
protectedComment comment =
  any (`List.isPrefixOf` stripped) ["{-#", "--#"]
    || case stripped of
      '-' : '-' : rest -> protectedContent rest
      '{' : '-' : rest -> protectedContent rest
      _ -> True
 where
  stripped = dropWhile Char.isSpace $ commentContents comment
  protectedContent rest = case dropWhile Char.isSpace rest of
    marker : _ | marker `elem` ['|', '^', '*', '$', '#'] -> True
    content -> any (`List.isPrefixOf` content) ["BRITTANY", "brittany", "{-#"]

annotationComments :: Annotation -> [Comment]
annotationComments annotation =
  fmap fst (annPriorComments annotation)
    ++ fmap fst (annFollowingComments annotation)
    ++ [comment | (AnnComment comment, _) <- annsDP annotation]

removeComments :: [(Comment, SrcLoc.RealSrcSpan)] -> Anns -> Anns
removeComments entries = Map.map remove
 where
  keys = Set.fromList $ SourceCommentKey . commentIdentifier . fst <$> entries
  keep comment = Set.notMember (SourceCommentKey $ commentIdentifier comment) keys
  remove annotation = annotation
    { annPriorComments = filter (keep . fst) $ annPriorComments annotation
    , annFollowingComments = filter (keep . fst) $ annFollowingComments annotation
    , annsDP = filter (\(keyword, _) -> case keyword of
        AnnComment comment -> keep comment
        _ -> True) $ annsDP annotation
    }

distinctEntries :: [(Comment, SrcLoc.RealSrcSpan)] -> [(Comment, SrcLoc.RealSrcSpan)]
distinctEntries = List.nubBy
  (\left right -> commentIdentifier (fst left) == commentIdentifier (fst right))

rebase
  :: (Int, Int)
  -> (Comment, SrcLoc.RealSrcSpan)
  -> ((Int, Int), (Comment, DeltaPos))
rebase previous (comment, currentSpan) =
  let (line, column) = spanStart currentSpan
      (previousLine, previousColumn) = previous
      delta = if line == previousLine then DP (0, column - previousColumn)
        else DP (line - previousLine, column - 1)
  in (spanEnd currentSpan, (comment, delta))

emptyAnnotation :: Annotation
emptyAnnotation = Ann Nothing Nothing [] [] [] (DP (0, 0))

spanStart :: SrcLoc.RealSrcSpan -> (Int, Int)
spanStart span' = (SrcLoc.srcSpanStartLine span', SrcLoc.srcSpanStartCol span')

spanEnd :: SrcLoc.RealSrcSpan -> (Int, Int)
spanEnd span' = (SrcLoc.srcSpanEndLine span', SrcLoc.srcSpanEndCol span')
