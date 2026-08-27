{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Language.Haskell.Brittany.Internal.Layouters.Type where

import qualified Data.Text as Text
import GHC (GenLocated(L), unLoc)
import Language.Haskell.Brittany.Internal.ExactPrintCompat (AnnKeywordId(..))
import GHC.Hs
import GHC.Parser.Annotation (getLocA)
import qualified GHC.OldList as List
import GHC.Types.Basic
import GHC.Types.SrcLoc (SrcSpan)
import GHC.Types.SourceText (SourceText(..))
import qualified GHC.Data.FastString as FastString
import GHC.Utils.Outputable (ftext, showSDocUnsafe)
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.ExactSource (nodeSourceFragment)
import Language.Haskell.Brittany.Internal.Fallbacks
  ( FallbackId(..)
  , untypedSpliceFamily
  )
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Layouters.IE (toL)
import qualified Language.Haskell.Brittany.Internal.Layouters.Type.Operator as Operator
import Language.Haskell.Brittany.Internal.Utils
  (FirstLastView(..), splitFirstLast)
import Unsafe.Coerce (unsafeCoerce)



layoutType :: ToBriDoc HsType
layoutType ltype = layoutType' (toL ltype)
 where
  layoutType' ltype'@(L _ typ) = docWrapNode ltype' $ case typ of
    HsTyVar _ promoted name -> do
      t <- lrdrNameToTextAnnTypeEqualityIsSpecial (toL name)
      let t' = if t == Text.pack "()" || t == Text.pack "[]"
               then t
               else applyNameAdornmentParensOnly name t
      case promoted of
        IsPromoted -> docSeq [docSeparator, docTick, docWrapNode (toL name) $ docLit t']
        NotPromoted -> docWrapNode (toL name) $ docLit t'
    HsForAllTy _ hsf (L _ (HsQualTy _ (L _ cntxts) typ2)) -> do
      let bndrs = getBinders hsf
      let sep = case hsf of
            HsForAllVis{}   -> " -> "
            _               -> " . "
      typeDoc <- docSharedWrapper layoutType (toL typ2)
      tyVarDocs <- layoutTyVarBndrs bndrs
      cntxtDocs <- map toL cntxts `forM` docSharedWrapper layoutType
      let
        maybeForceML = case typ2 of
          (L _ HsFunTy{}) -> docForceMultiline
          _ -> id
      let
        tyVarDocLineList = processTyVarBndrsSingleline tyVarDocs
        forallDoc = docAlt
          [ let open = docLit $ Text.pack "forall"
            in docSeq ([open] ++ tyVarDocLineList)
          , docPar
            (docLit (Text.pack "forall"))
            (docLines $ tyVarDocs <&> \case
            (tname, Nothing) -> docEnsureIndent BrIndentRegular $ docLit tname
            (tname, Just doc) -> docEnsureIndent BrIndentRegular $ docLines
              [ docCols ColTyOpPrefix [docParenLSep, docLit tname]
              , docCols ColTyOpPrefix [docLit $ Text.pack ":: ", doc]
              , docLit $ Text.pack ")"
              ]
            )
          ]
        contextDoc = case cntxtDocs of
          [] -> docLit $ Text.pack "()"
          [x] -> x
          _ -> docAlt
            [ let open = docLit $ Text.pack "("
                  close = docLit $ Text.pack ")"
                  list = List.intersperse docCommaSep $ docForceSingleline <$> cntxtDocs
                in docSeq ([open] ++ list ++ [close])
            , let
                open = docCols ColTyOpPrefix [docParenLSep, docAddBaseY (BrIndentSpecial 2) $ head cntxtDocs]
                close = docLit $ Text.pack ")"
                list = List.tail cntxtDocs <&> \cntxtDoc -> docCols ColTyOpPrefix [docCommaSep, docAddBaseY (BrIndentSpecial 2) cntxtDoc]
                in docPar open $ docLines $ list ++ [close]
            ]
      docAlt
        -- :: forall a b c . (Foo a b c) => a b -> c
        [ docSeq
          [ if null bndrs
            then docEmpty
            else
              let
                open = docLit $ Text.pack "forall"
                close = docLit $ Text.pack sep
              in docSeq ([open, docSeparator] ++ tyVarDocLineList ++ [close])
          , docForceSingleline contextDoc
          , docLit $ Text.pack " => "
          , docForceSingleline typeDoc
          ]
        -- :: forall a b c
        --  . (Foo a b c)
        -- => a b
        -- -> c
        , docPar
          forallDoc
          (docLines
            [ docCols
              ColTyOpPrefix
              [  docLit $ Text.pack sep
              , docAddBaseY (BrIndentSpecial 3) $ contextDoc
              ]
            , docCols
              ColTyOpPrefix
              [ docLit $ Text.pack "=> "
              , docAddBaseY (BrIndentSpecial 3) $ maybeForceML $ typeDoc
              ]
            ]
          )
        ]
    HsForAllTy _ hsf typ2 -> do
      let bndrs = getBinders hsf
      let sep = case hsf of
            HsForAllVis{}   -> " -> "
            _               -> " . "
      typeDoc <- layoutType (toL typ2)
      tyVarDocs <- layoutTyVarBndrs bndrs
      let
        maybeForceML = case typ2 of
          (L _ HsFunTy{}) -> docForceMultiline
          _ -> id
      let tyVarDocLineList = processTyVarBndrsSingleline tyVarDocs
      docAlt
        -- forall x . x  /  forall x -> x
        [ docSeq
          [ if null bndrs
            then docEmpty
            else
              let
                open = docLit $ Text.pack "forall"
                close = docLit $ Text.pack sep
              in docSeq ([open] ++ tyVarDocLineList ++ [close])
          , docForceSingleline $ return $ typeDoc
          ]
        -- :: forall x
        --  . x
        , docPar
          (docSeq $ docLit (Text.pack "forall") : tyVarDocLineList)
          (docCols
            ColTyOpPrefix
            [  docLit $ Text.pack sep
            , maybeForceML $ return typeDoc
            ]
          )
        -- :: forall
        --      (x :: *)
        --  . x
        , docPar
          (docLit (Text.pack "forall"))
          (docLines
          $ (tyVarDocs <&> \case
              (tname, Nothing) ->
                docEnsureIndent BrIndentRegular $ docLit tname
              (tname, Just doc) -> docEnsureIndent BrIndentRegular $ docLines
                [ docCols ColTyOpPrefix [docParenLSep, docLit tname]
                , docCols ColTyOpPrefix [docLit $ Text.pack ":: ", doc]
                , docLit $ Text.pack ")"
                ]
            )
          ++ [ docCols
                 ColTyOpPrefix
                 [  docLit $ Text.pack sep
                 , maybeForceML $ return typeDoc
                 ]
             ]
          )
        ]
    HsQualTy _ lcntxts typ1 -> do
      let lcntxts' = toL lcntxts
          cntxts' = map toL (unLoc lcntxts)
      typeDoc <- docSharedWrapper layoutType (toL typ1)
      cntxtDocs <- cntxts' `forM` docSharedWrapper layoutType
      let
        contextDoc = docWrapNode lcntxts' $ case cntxtDocs of
          [] -> docLit $ Text.pack "()"
          [x] -> x
          _ -> docAlt
            [ let
              open = docLit $ Text.pack "("
              close = docLit $ Text.pack ")"
              list =
                List.intersperse docCommaSep $ docForceSingleline <$> cntxtDocs
            in docSeq ([open] ++ list ++ [close])
            , let
                open = docCols ColTyOpPrefix [docParenLSep, docAddBaseY (BrIndentSpecial 2) $ head cntxtDocs]
                close = docLit $ Text.pack ")"
                list = List.tail cntxtDocs <&> \cntxtDoc -> docCols ColTyOpPrefix [docCommaSep, docAddBaseY (BrIndentSpecial 2) cntxtDoc]
            in docPar open $ docLines $ list ++ [close]
            ]
      let
        maybeForceML = case toL typ1 of
          (L _ HsFunTy{}) -> docForceMultiline
          _ -> id
      docAlt
        -- (Foo a b c) => a b -> c
        [ docSeq
          [ docForceSingleline contextDoc
          , docLit $ Text.pack " => "
          , docForceSingleline typeDoc
          ]
        --    (Foo a b c)
        -- => a b
        -- -> c
        , docPar
          (docForceSingleline contextDoc)
          (docCols
            ColTyOpPrefix
            [ docLit $ Text.pack "=> "
            , docAddBaseY (BrIndentSpecial 3) $ maybeForceML typeDoc
            ]
          )
        ]
    HsFunTy _ _ typ1 typ2 -> do
      typeDoc1 <- docSharedWrapper layoutType (toL typ1)
      typeDoc2 <- docSharedWrapper layoutType (toL typ2)
      let
        maybeForceML = case toL typ2 of
          (L _ HsFunTy{}) -> docForceMultiline
          _ -> id
      hasComments <- hasAnyCommentsBelow ltype
      docAlt
        $ [ docSeq
              [ appSep $ docForceSingleline typeDoc1
              , appSep $ docLit $ Text.pack "->"
              , docForceSingleline typeDoc2
              ]
          | not hasComments
          ]
        ++ [ docPar
               (docNodeAnnKW ltype Nothing typeDoc1)
               (docCols
                 ColTyOpPrefix
                 [  appSep $ docLit $ Text.pack "->"
                 , docAddBaseY (BrIndentSpecial 3) $ maybeForceML typeDoc2
                 ]
               )
           ]
    HsParTy _ typ1 -> do
      typeDoc1 <- docSharedWrapper layoutType (toL typ1)
      docAlt
        [ docSeq
          [  docLit $ Text.pack "("
          , docForceSingleline typeDoc1
          , docLit $ Text.pack ")"
          ]
        , docPar
          (docCols
            ColTyOpPrefix
            [  docParenLSep
            , docAddBaseY (BrIndentSpecial 2) $ typeDoc1
            ]
          )
          (docLit $ Text.pack ")")
        ]
    HsAppTy _ typ1@(L _ HsAppTy{}) typ2 -> do
      let
        gather
          :: [LHsType GhcPs] -> LHsType GhcPs -> (LHsType GhcPs, [LHsType GhcPs])
        gather list = \case
          L _ (HsAppTy _ ty1 ty2) -> gather (ty2 : list) ty1
          final -> (final, list)
      let (typHead, typRest) = gather [typ2] typ1
      docHead <- docSharedWrapper layoutType (toL typHead)
      docRest <- docSharedWrapper layoutType `mapM` (map toL typRest)
      docAlt
        [ docSeq
        $ docForceSingleline docHead
        : (docRest >>= \d -> [docSeparator, docForceSingleline d])
        , docPar docHead (docLines $ docEnsureIndent BrIndentRegular <$> docRest)
        ]
    HsAppTy _ typ1 typ2 -> do
      typeDoc1 <- docSharedWrapper layoutType (toL typ1)
      typeDoc2 <- docSharedWrapper layoutType (toL typ2)
      docAlt
        [ docSeq
          [docForceSingleline typeDoc1, docSeparator, docForceSingleline typeDoc2]
        , docPar typeDoc1 (docEnsureIndent BrIndentRegular typeDoc2)
        ]
    HsListTy _ typ1 -> do
      typeDoc1 <- docSharedWrapper layoutType (toL typ1)
      docAlt
        [ docSeq
          [ docLit $ Text.pack "["
          , docForceSingleline typeDoc1
          , docLit $ Text.pack "]"
          ]
        , docPar
          (docCols
            ColTyOpPrefix
            [ docLit $ Text.pack "[ "
            , docAddBaseY (BrIndentSpecial 2) $ typeDoc1
            ]
          )
          (docLit $ Text.pack "]")
        ]
    HsTupleTy _ tupleSort typs -> case tupleSort of
      HsUnboxedTuple -> unboxed
      HsBoxedOrConstraintTuple -> simple
      where
        unboxed = if null typs
          then error "brittany internal error: unboxed unit"
          else unboxedL
        simple = if null typs then unitL else simpleL
        unitL = docLit $ Text.pack "()"
        simpleL = do
          docs <- docSharedWrapper layoutType `mapM` (map toL typs)
          let
            end = docLit $ Text.pack ")"
            lines =
              List.tail docs
                <&> \d -> docAddBaseY (BrIndentSpecial 2)
                      $ docCols ColTyOpPrefix [docCommaSep, d]
            commaDocs = List.intersperse docCommaSep (docForceSingleline <$> docs)
          docAlt
            [ docSeq
            $ [docLit $ Text.pack "("]
            ++ commaDocs
            ++ [end]
            , let line1 = docCols ColTyOpPrefix [docParenLSep, head docs]
              in
                docPar
                  (docAddBaseY (BrIndentSpecial 2) $ line1)
                  (docLines $ lines ++ [end])
            ]
        unboxedL = do
          docs <- docSharedWrapper layoutType `mapM` (map toL typs)
          let
            start = docParenHashLSep
            end = docParenHashRSep
          docAlt
            [ docSeq
            $ [start]
            ++ List.intersperse docCommaSep docs
            ++ [end]
            , let
                line1 = docCols ColTyOpPrefix [start, head docs]
                lines =
                  List.tail docs
                    <&> \d -> docAddBaseY (BrIndentSpecial 2)
                          $ docCols ColTyOpPrefix [docCommaSep, d]
              in docPar
                (docAddBaseY (BrIndentSpecial 2) line1)
                (docLines $ lines ++ [end])
            ]
    HsOpTy _ promotion typ1 opName typ2 ->
      Operator.layoutOperatorType layoutType ltype promotion typ1 opName typ2
    HsIParamTy _ (L _ (HsIPName ipName)) typ1 -> do
      typeDoc1 <- docSharedWrapper layoutType (toL typ1)
      docAlt
        [ docSeq
          [  docLit $ Text.pack
            ("?" ++ showSDocUnsafe (ftext ipName) ++ "::")
          , docForceSingleline typeDoc1
          ]
        , docPar
          (docLit $ Text.pack ("?" ++ showSDocUnsafe (ftext ipName)))
          (docCols
            ColTyOpPrefix
            [  docLit $ Text.pack ":: "
            , docAddBaseY (BrIndentSpecial 2) typeDoc1
            ]
          )
        ]
    -- TODO: test KindSig
    HsKindSig _ typ1 kind1 -> do
      typeDoc1 <- docSharedWrapper layoutType (toL typ1)
      kindDoc1 <- docSharedWrapper layoutType (toL kind1)
      hasParens <- hasAnnKeyword ltype AnnOpenP
      docAlt
        [ if hasParens
          then docSeq
            [ docLit $ Text.pack "("
            , docForceSingleline typeDoc1
            , docSeparator
            , docLit $ Text.pack "::"
            , docSeparator
            , docForceSingleline kindDoc1
            , docLit $ Text.pack ")"
            ]
          else docSeq
            [ docForceSingleline typeDoc1
            , docSeparator
            , docLit $ Text.pack "::"
            , docSeparator
            , docForceSingleline kindDoc1
            ]
        , if hasParens
          then docLines
            [ docCols
              ColTyOpPrefix
              [  docParenLSep
              , docAddBaseY (BrIndentSpecial 3) $ typeDoc1
              ]
            , docCols
              ColTyOpPrefix
              [  docLit $ Text.pack ":: "
              , docAddBaseY (BrIndentSpecial 3) kindDoc1
              ]
            , (docLit $ Text.pack ")")
            ]
          else docPar
            typeDoc1
            (docCols
              ColTyOpPrefix
              [  docLit $ Text.pack ":: "
              , docAddBaseY (BrIndentSpecial 3) kindDoc1
              ]
            )
        ]
  -- HsBangTy bang typ1 -> do
  --   let bangStr = case bang of
  --         HsSrcBang _ unpackness strictness ->
  --           (++)
  --             (case unpackness of
  --               SrcUnpack   -> "{-# UNPACK -#} "
  --               SrcNoUnpack -> "{-# NOUNPACK -#} "
  --               NoSrcUnpack -> ""
  --             )
  --             (case strictness of
  --               SrcLazy     -> "~"
  --               SrcStrict   -> "!"
  --               NoSrcStrict -> ""
  --             )
  --   let bangLen = length bangStr
  --   layouter@(Layouter desc _ _) <- layoutType typ1
  --   let line = do -- Maybe
  --         l <- _ldesc_line desc
  --         let len = bangLen + _lColumns_min l
  --         return $ LayoutColumns
  --           { _lColumns_key = ColumnKeyUnique
  --           , _lColumns_lengths = [len]
  --           , _lColumns_min = len
  --           }
  --   let block = do -- Maybe
  --         rol <- descToBlockStart desc
  --         (minR,maxR) <- descToBlockMinMax desc
  --         return $ BlockDesc
  --           { _bdesc_blockStart = rol
  --           , _bdesc_min = minR
  --           , _bdesc_max = maxR
  --           , _bdesc_opIndentFloatUp = Nothing
  --           }
  --   return $ Layouter
  --     { _layouter_desc = LayoutDesc
  --       { _ldesc_line = line
  --       , _ldesc_block = block
  --       }
  --     , _layouter_func = \_params -> do
  --         remaining <- getCurRemaining
  --         case line of
  --           Just (LayoutColumns _ _ m) | m <= remaining -> do
  --             layoutWriteAppend $ Text.pack $ bangStr
  --             applyLayouterRestore layouter defaultParams
  --           _ -> do
  --             layoutWriteAppend $ Text.pack $ bangStr
  --             layoutWritePostCommentsRestore ltype
  --             applyLayouterRestore layouter defaultParams
  --     , _layouter_ast = ltype
  --     }
    HsSpliceTy _ splice -> briDocByOpaqueNoComment
      (untypedSpliceFamily splice)
      TypeFallback
      ltype
    HsDocTy{} -> layoutExactSourceType ltype
    HsExplicitListTy _ _ typs -> do
      typDocs <- (docSharedWrapper layoutType) `mapM` (map toL typs)
      hasComments <- hasAnyCommentsBelow ltype
      let specialCommaSep = appSep $ docLit $ Text.pack " ,"
      docAlt
        [ docSeq
        $ [docLit $ Text.pack "'["]
        ++ List.intersperse specialCommaSep (docForceSingleline <$> typDocs)
        ++ [docLit $ Text.pack "]"]
        , case splitFirstLast typDocs of
          FirstLastEmpty -> docSeq
            [ docLit $ Text.pack "'["
            , docNodeAnnKW ltype (Just AnnOpenS) $ docLit $ Text.pack "]"
            ]
          FirstLastSingleton e -> docAlt
            [ docSeq
              [ docLit $ Text.pack "'["
              , docNodeAnnKW ltype (Just AnnOpenS) $ docForceSingleline e
              , docLit $ Text.pack "]"
              ]
            , docSetBaseY $ docLines
              [ docSeq
                [ docLit $ Text.pack "'["
                , docSeparator
                , docSetBaseY $ docNodeAnnKW ltype (Just AnnOpenS) e
                ]
              , docLit $ Text.pack " ]"
              ]
            ]
          FirstLast e1 ems eN -> runFilteredAlternative $ do
            addAlternativeCond (not hasComments)
              $ docSeq
              $ [docLit $ Text.pack "'["]
              ++ List.intersperse
                   specialCommaSep
                   (docForceSingleline
                   <$> (e1 : ems ++ [docNodeAnnKW ltype (Just AnnOpenS) eN])
                   )
              ++ [docLit $ Text.pack " ]"]
            addAlternative
              $ let
                  start = docCols ColList [appSep $ docLit $ Text.pack "'[", e1]
                  linesM = ems <&> \d -> docCols ColList [specialCommaSep, d]
                  lineN = docCols
                    ColList
                    [specialCommaSep, docNodeAnnKW ltype (Just AnnOpenS) eN]
                  end = docLit $ Text.pack " ]"
                in docSetBaseY $ docLines $ [start] ++ linesM ++ [lineN] ++ [end]
        ]
    HsExplicitTupleTy{} -> layoutExactSourceType ltype
    HsTyLit _ lit -> case lit of
      HsNumTy (SourceText srctext) _ -> docLit $ Text.pack (FastString.unpackFS srctext)
      HsNumTy NoSourceText _ ->
        error "overLitValBriDoc: literal with no SourceText"
      HsStrTy (SourceText srctext) _ -> docLit $ Text.pack (FastString.unpackFS srctext)
      HsStrTy NoSourceText _ ->
        error "overLitValBriDoc: literal with no SourceText"
    HsWildCardTy _ -> docLit $ Text.pack "_"
    HsSumTy{} -> layoutExactSourceType ltype
    HsStarTy _ isUnicode -> do
      if isUnicode
        then docLit $ Text.pack "\x2605" -- Unicode star
        else docLit $ Text.pack "*"
    XHsType{} -> error "brittany internal error: XHsType"
    _ -> layoutExactSourceType ltype
    HsAppKindTy _ ty kind -> do
      t <- docSharedWrapper layoutType (toL ty)
      k <- docSharedWrapper layoutType (toL kind)
      docAlt
        [ docSeq
          [ docForceSingleline t
          , docSeparator
          , docLit $ Text.pack "@"
          , docForceSingleline k
          ]
        , docPar t (docSeq [docLit $ Text.pack "@", k])
        ]

layoutExactSourceType
  :: GenLocated SrcSpan (HsType GhcPs) -> ToBriDocM BriDocNumbered
layoutExactSourceType type' = do
  OriginalSource source <- mAsk
  anns <- mAsk
  commentPlan <- mAsk
  case nodeSourceFragment source type' anns commentPlan of
    Nothing -> briDocByExactInlineOnly TypeFallback type'
    Just fragment -> briDocByExactSourceFragmentNoComment
      TypeFallback type' fragment

layoutTyVarBndrs
  :: forall flag. [LHsTyVarBndr flag GhcPs]
  -> ToBriDocM [(Text, Maybe (ToBriDocM BriDocNumbered))]
layoutTyVarBndrs = mapM $ \case
  (L _ (HsTvb _ _ (HsBndrVar _ lname) (HsBndrNoKind _))) ->
    return $ (lrdrNameToText (toL lname), Nothing)
  (L _ (HsTvb _ _ (HsBndrVar _ lname) (HsBndrKind _ kind))) -> do
    d <- docSharedWrapper layoutType (toL kind)
    return $ (lrdrNameToText (toL lname), Just d)
  (L _ (XTyVarBndr _)) -> error "layoutTyVarBndrs: XTyVarBndr"

-- there is no specific reason this returns a list instead of a single
-- BriDoc node.
processTyVarBndrsSingleline
  :: [(Text, Maybe (ToBriDocM BriDocNumbered))] -> [ToBriDocM BriDocNumbered]
processTyVarBndrsSingleline bndrDocs = bndrDocs >>= \case
  (tname, Nothing) -> [docSeparator, docLit tname]
  (tname, Just doc) ->
    [ docSeparator
    , docLit $ Text.pack "(" <> tname <> Text.pack " :: "
    , docForceSingleline $ doc
    , docLit $ Text.pack ")"
    ]

getBinders :: HsForAllTelescope pass -> [LHsTyVarBndr () pass]
getBinders x = case x of
  HsForAllVis _ b -> b
  HsForAllInvis _ b -> map invisBinderToUnit b
  XHsForAllTelescope _ -> []

-- Convert invisible binder (with specificity) to unit-specificity.
-- XRec does not unify with GenLocated so we coerce to GenLocated SrcSpan, strip, then coerce back.
invisBinderToUnit :: LHsTyVarBndr flag pass -> LHsTyVarBndr () pass
invisBinderToUnit lb =
  let lb' = unsafeCoerce lb :: GenLocated SrcSpan (HsTyVarBndr flag pass)
  in unsafeCoerce (stripBinder (L (getLocA lb') (unLoc lb')))

stripBinder :: GenLocated l (HsTyVarBndr flag pass) -> GenLocated l (HsTyVarBndr () pass)
stripBinder (L loc b) = L loc $ case b of
  HsTvb a _ c d -> HsTvb a () c d
  XTyVarBndr a -> XTyVarBndr a
