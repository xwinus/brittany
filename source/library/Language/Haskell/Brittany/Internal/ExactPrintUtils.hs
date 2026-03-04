{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Haskell.Brittany.Internal.ExactPrintUtils where

import qualified Control.Monad.State.Class as State.Class
import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import Data.Data
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import qualified Data.Foldable as Foldable
import qualified Data.Generics as SYB
import Data.HList.HList
import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import GHC (GenLocated(L))
import qualified GHC hiding (parseModule)
import GHC.Hs (GhcPs, HsDecl, HsModule(..), LHsDecl, hsmodDecls)
import GHC.Parser.Annotation
  ( AnnContext
  , AnnList
  , AnnListItem
  , EpAnn(..)
  , NoEpAnns
  , epaLocationRealSrcSpan
  , getLocA
  , HasLoc(..)
  )
import GHC.Types.SrcLoc (Located, RealSrcSpan, SrcSpan, unLoc)
import GHC.Types.SrcLoc (EpaLocation'(..))
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.Config.Types
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as ExactPrint
import qualified System.IO



parseModule
  :: [String]
  -> System.IO.FilePath
  -> (GHC.DynFlags -> IO (Either String a))
  -> IO (Either String (ExactPrint.Anns, GHC.ParsedSource, a))
parseModule args fp dynCheck = do
  str <- System.IO.readFile fp
  parseModuleFromString args fp dynCheck str

parseModuleFromString
  :: [String]
  -> System.IO.FilePath
  -> (GHC.DynFlags -> IO (Either String a))
  -> String
  -> IO (Either String (ExactPrint.Anns, GHC.ParsedSource, a))
parseModuleFromString = ParseModule.parseModule


commentAnnFixTransformGlob :: SYB.Data ast => ast -> ExactPrint.Transform ()
commentAnnFixTransformGlob ast = do
  let
    extract :: forall a . SYB.Data a => a -> Seq (SrcSpan, ExactPrint.AnnKey)
    extract = -- traceFunctionWith "extract" (show . SYB.typeOf) show $
      const Seq.empty
        `SYB.ext1Q` (\l@(L span _) ->
                      Seq.singleton (span, ExactPrint.mkAnnKey l)
                    )
  let nodes = SYB.everything (<>) extract ast
  let
    annsMap :: Map SrcLoc.RealSrcLoc ExactPrint.AnnKey
    annsMap = Map.fromListWith
      (const id)
      [ (SrcLoc.realSrcSpanEnd realSpan, annKey)
      | (_, annKey) <- Foldable.toList nodes
      , Just realSpan <- [ExactPrint.annKeyRealSpan annKey]
      ]
  nodes `forM_` (snd .> processComs annsMap)
 where
  processComs annsMap annKey1 = do
    mAnn <- State.Class.gets (Map.lookup annKey1)
    mAnn `forM_` \ann1 -> do
      let
        priors = ExactPrint.annPriorComments ann1
        follows = ExactPrint.annFollowingComments ann1
        assocs = ExactPrint.annsDP ann1
      let
        processCom
          :: (ExactPrint.Comment, ExactPrint.DeltaPos)
          -> ExactPrint.TransformT Identity Bool
        processCom comPair@(com, _) =
          case ExactPrint.srcSpanToRealSpan (ExactPrint.commentIdentifier com) of
            Nothing -> return True
            Just comRealSpan ->
              let comLoc = SrcLoc.realSrcSpanStart comRealSpan
              in case Map.lookupLE comLoc annsMap of
                Just (_, annKey2) ->
                  case (ExactPrint.annKeyRealSpan annKey1, ExactPrint.annKeyRealSpan annKey2) of
                    (Just r1, Just r2) | SrcLoc.realSrcSpanStart r1 /= SrcLoc.realSrcSpanStart r2 ->
                      case (ExactPrint.annKeyCon annKey1, ExactPrint.annKeyCon annKey2) of
                        (ExactPrint.CN "RecordCon", ExactPrint.CN "HsRecField") ->
                          move annKey2 $> False
                        (x, y) | x == y -> move annKey2 $> False
                        _ -> return True
                     where
                      move ak2 = ExactPrint.modifyAnnsT $ \anns ->
                        let ann2 = Data.Maybe.fromJust $ Map.lookup ak2 anns
                            ann2' = ann2
                              { ExactPrint.annFollowingComments =
                                ExactPrint.annFollowingComments ann2 ++ [comPair]
                              }
                        in Map.insert ak2 ann2' anns
                    _ -> return True
                _ -> return True -- retain comment at current node.
      priors' <- filterM processCom priors
      follows' <- filterM processCom follows
      assocs' <- flip filterM assocs $ \case
        (ExactPrint.AnnComment com, dp) -> processCom (com, dp)
        _ -> return True
      let
        ann1' = ann1
          { ExactPrint.annPriorComments = priors'
          , ExactPrint.annFollowingComments = follows'
          , ExactPrint.annsDP = assocs'
          }
      ExactPrint.modifyAnnsT $ \anns -> Map.insert annKey1 ann1' anns


-- TODO: this is unused by now, but it contains one detail that
--       commentAnnFixTransformGlob does not include: Moving of comments for
--       "RecordUpd"s.
-- commentAnnFixTransform :: GHC.ParsedSource -> ExactPrint.Transform ()
-- commentAnnFixTransform modul = SYB.everything (>>) genF modul
--  where
--   genF :: Data.Data.Data a => a -> ExactPrint.Transform ()
--   genF = (\_ -> return ()) `SYB.extQ` exprF
--   exprF :: Located (HsExpr GhcPs) -> ExactPrint.Transform ()
--   exprF lexpr@(L _ expr) = case expr of
-- #if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
--     RecordCon _ _ (HsRecFields fs@(_:_) Nothing) ->
-- #else
--     RecordCon _ _ _ (HsRecFields fs@(_:_) Nothing) ->
-- #endif
--       moveTrailingComments lexpr (List.last fs)
-- #if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
--     RecordUpd _ _e fs@(_:_) ->
-- #else
--     RecordUpd _e fs@(_:_) _cons _ _ _ ->
-- #endif
--       moveTrailingComments lexpr (List.last fs)
--     _ -> return ()

-- commentAnnFixTransform :: GHC.ParsedSource -> ExactPrint.Transform ()
-- commentAnnFixTransform modul = SYB.everything (>>) genF modul
--  where
--   genF :: Data.Data.Data a => a -> ExactPrint.Transform ()
--   genF = (\_ -> return ()) `SYB.extQ` exprF
--   exprF :: Located (HsExpr GhcPs) -> ExactPrint.Transform ()
--   exprF lexpr@(L _ expr) = case expr of
--     RecordCon _ _ (HsRecFields fs@(_:_) Nothing) ->
--       moveTrailingComments lexpr (List.last fs)
--     RecordUpd _ _e fs@(_:_) ->
--       moveTrailingComments lexpr (List.last fs)
--     _ -> return ()

-- moveTrailingComments :: (Data.Data.Data a,Data.Data.Data b)
--                      => GHC.Located a -> GHC.Located b -> ExactPrint.Transform ()
-- moveTrailingComments astFrom astTo = do
--   let
--     k1 = ExactPrint.mkAnnKey astFrom
--     k2 = ExactPrint.mkAnnKey astTo
--     moveComments ans = ans'
--       where
--         an1 = Data.Maybe.fromJust $ Map.lookup k1 ans
--         an2 = Data.Maybe.fromJust $ Map.lookup k2 ans
--         cs1f = ExactPrint.annFollowingComments an1
--         cs2f = ExactPrint.annFollowingComments an2
--         (comments, nonComments) = flip breakEither (ExactPrint.annsDP an1)
--              $ \case
--                (ExactPrint.AnnComment com, dp) -> Left (com, dp)
--                x -> Right x
--         an1' = an1
--           { ExactPrint.annsDP               = nonComments
--           , ExactPrint.annFollowingComments = []
--           }
--         an2' = an2
--           { ExactPrint.annFollowingComments = cs1f ++ cs2f ++ comments
--           }
--         ans' = Map.insert k1 an1' $ Map.insert k2 an2' ans

--   ExactPrint.modifyAnnsT moveComments

-- | split a set of annotations in a module into a map from top-level module
-- elements to the relevant annotations. Avoids quadratic behaviour a trivial
-- implementation would have.
extractToplevelAnns
  :: Located (HsModule GhcPs)
  -> ExactPrint.Anns
  -> Map ExactPrint.AnnKey ExactPrint.Anns
extractToplevelAnns lmod anns = output
 where
  (L _ m) = lmod
  -- GHC 9.14: hsmodDecls returns [LHsDecl GhcPs] with EpAnn location; convert to SrcSpan for mkAnnKey
  ldecls = map (\ldecl -> L (getLocA ldecl) (unLoc ldecl)) (hsmodDecls m)
  declMap1 :: Map ExactPrint.AnnKey ExactPrint.AnnKey
  declMap1 = Map.unions $ ldecls <&> \ldecl ->
    Map.fromSet (const (ExactPrint.mkAnnKey ldecl)) (foldedAnnKeys ldecl)
  declMap2 :: Map ExactPrint.AnnKey ExactPrint.AnnKey
  declMap2 =
    Map.fromList
      $ [ (captured, declMap1 Map.! k)
        | (k, ExactPrint.Ann (Just captured) _ _ _ _ _) <- Map.toList anns
        ]
  declMap = declMap1 `Map.union` declMap2
  -- Use getLocA for consistent SrcSpan-based keys with ExtractAnns (mkAnnKeyL)
  lmodSrc = L (getLocA lmod) (unLoc lmod)
  modKey = ExactPrint.mkAnnKey lmodSrc
  output = groupMap (\k _ -> Map.findWithDefault modKey k declMap) anns

groupMap :: (Ord k, Ord l) => (k -> a -> l) -> Map k a -> Map l (Map k a)
groupMap f = Map.foldlWithKey'
  (\m k a -> Map.alter (insert k a) (f k a) m)
  Map.empty
 where
  insert k a Nothing = Just (Map.singleton k a)
  insert k a (Just m) = Just (Map.insert k a m)

-- | Extract SrcSpan from Dynamic. GHC 9.14 uses EpAnn-based locations;
-- try EpAnn AnnListItem (SrcSpanAnnA) first, then SrcSpan.
tryGetSrcSpanFromDynamic :: Dynamic -> Maybe SrcSpan
tryGetSrcSpanFromDynamic d =
  fromDynamic @SrcSpan d
    <|> (tryEpAnnToSrcSpan =<< fromDynamic @(EpAnn AnnListItem) d)
    <|> (tryEpAnnToSrcSpan =<< fromDynamic @(EpAnn NoEpAnns) d)
    <|> (tryEpAnnToSrcSpan =<< fromDynamic @(EpAnn AnnContext) d)
    <|> (tryEpAnnToSrcSpan =<< fromDynamic @(EpAnn (AnnList ())) d)
    <|> (tryEpAnnToSrcSpan =<< fromDynamic @(EpAnn ()) d)
    <|> (tryEpAnnToSrcSpan =<< fromDynamic @(EpAnn [()]) d)

-- | Safely convert an EpAnn's entry to SrcSpan, returning Nothing for
-- generated spans that would cause epaLocationRealSrcSpan to panic.
tryEpAnnToSrcSpan :: EpAnn a -> Maybe SrcSpan
tryEpAnnToSrcSpan epann = case epann of
  EpAnn anc _ _ -> case anc of
    EpaSpan ss -> case ss of
      SrcLoc.RealSrcSpan rss _ -> Just (ExactPrint.realSpanToSrcSpan rss)
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

foldedAnnKeys :: Data.Data.Data ast => ast -> Set ExactPrint.AnnKey
foldedAnnKeys ast = SYB.everything
  Set.union
  (\x ->
    if locTyCon /= SYB.typeRepTyCon (SYB.typeOf x)
      then Set.empty
      else
        case tryGetSrcSpanFromDynamic (SYB.gmapQi 0 toDyn x) of
          Nothing -> Set.empty
          Just l -> Set.singleton (SYB.gmapQi 1 (ExactPrint.mkAnnKey . L l) x)
  )
  ast
  where locTyCon = SYB.typeRepTyCon (SYB.typeOf (L () ()))


withTransformedAnns
  :: Data ast
  => ast
  -> MultiRWSS.MultiRWS '[Config , ExactPrint.Anns] w s a
  -> MultiRWSS.MultiRWS '[Config , ExactPrint.Anns] w s a
withTransformedAnns ast m = MultiRWSS.mGetRawR >>= \case
  readers@(conf :+: anns :+: HNil) -> do
    -- TODO: implement `local` for MultiReader/MultiRWS
    MultiRWSS.mPutRawR (conf :+: f anns :+: HNil)
    x <- m
    MultiRWSS.mPutRawR readers
    pure x
 where
  f anns =
    let
      (annsBalanced, ()) =
        ExactPrint.runTransform anns (commentAnnFixTransformGlob ast)
    in annsBalanced
