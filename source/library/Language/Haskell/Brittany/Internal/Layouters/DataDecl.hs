{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.DataDecl where

import qualified Data.Data
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as Text
import GHC (GenLocated(L), Located, unLoc)
import GHC.Hs
import GHC.Types.SrcLoc (noSrcSpan)
import qualified GHC.OldList as List
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.ExactPrintCompat (AnnKeywordId(..))
import Language.Haskell.Brittany.Internal.Fallbacks (FallbackId(..))
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.IE (toL)
import Language.Haskell.Brittany.Internal.Layouters.Type
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Types



layoutDataDecl
  :: Located (TyClDecl GhcPs)
  -> Located RdrName
  -> LHsQTyVars GhcPs
  -> HsDataDefn GhcPs
  -> ToBriDocM BriDocNumbered
layoutDataDecl ltycl name (HsQTvs _ bndrs) defn = case defn of
  HsDataDefn { dd_ctxt = mCtxt, dd_cons = cons, dd_derivs = mDerivs } -> case cons of
  -- newtype MyType a b = MyType ..
    NewTypeCon lcons ->
      case lcons of
        (L _ (ConDeclH98 _ext consName False _qvars (Just (L _ [])) details _conDoc))
          -> docWrapNode ltycl $ do
              nameStr <- lrdrNameToTextAnn name
              consNameStr <- applyNameAdornment consName <$> lrdrNameToTextAnn (toL consName)
              tyVarLine <- return <$> createBndrDoc bndrs
              rhsDoc <- return <$> createDetailsDoc consNameStr details
              createDerivingPar mDerivs $ docSeq
                [ appSep $ docLitS "newtype"
                , appSep $ docLit nameStr
                , appSep tyVarLine
                , docSeparator
                , docLitS "="
                , docSeparator
                , rhsDoc
                ]
        _ -> briDocByExactNoComment DataDeclarationFallback ltycl

  -- data MyData a b
  -- (zero constructors)
    DataTypeCons _ [] ->
      docWrapNode ltycl $ do
        lhsContextDoc <- docSharedWrapper createContextDoc (unLoc (maybe (L noSrcSpan []) toL mCtxt))
        nameStr <- lrdrNameToTextAnn name
        tyVarLine <- return <$> createBndrDoc bndrs
        createDerivingPar mDerivs $ docSeq
          [ appSep $ docLitS "data"
          , lhsContextDoc
          , appSep $ docLit nameStr
          , appSep tyVarLine
          ]

  -- data MyData = First | Second ..
    DataTypeCons _ constructors@(_ : _ : _) -> do
      hasComments <- hasAnyRegularCommentsConnected ltycl
      case traverse simpleConstructor constructors of
        Just simpleConstructors | not hasComments ->
          layoutMultipleConstructors mCtxt mDerivs simpleConstructors
        _ -> briDocByExactNoComment DataDeclarationFallback ltycl
    DataTypeCons _ [lcons] ->
      (case lcons of
        (L _ (ConDeclH98 _ext consName _hasExt qvars mRhsContext details _conDoc))
          -> docWrapNode ltycl do
              let lhsContext = unLoc (maybe (L noSrcSpan []) toL mCtxt)
              lhsContextDoc <- docSharedWrapper createContextDoc lhsContext
              nameStr <- lrdrNameToTextAnn name
              consNameStr <- applyNameAdornment consName <$> lrdrNameToTextAnn (toL consName)
              tyVarLine <- return <$> createBndrDoc bndrs
              forallDocMay <- case createForallDoc qvars of
                Nothing -> pure Nothing
                Just x -> Just . pure <$> x
              rhsContextDocMay <- case mRhsContext of
                Nothing -> pure Nothing
                Just (L _ ctxt) -> Just . pure <$> createContextDoc ctxt
              rhsDoc <- return <$> createDetailsDoc consNameStr details
              consDoc <-
                fmap pure
                $ docNonBottomSpacing
                $ case (forallDocMay, rhsContextDocMay) of
                (Just forallDoc, Just rhsContextDoc) -> docLines
                  [ docSeq
                    [docLitS "=", docSeparator, docForceSingleline forallDoc]
                  , docSeq
                    [ docLitS "."
                    , docSeparator
                    , docSetBaseY $ docLines [rhsContextDoc, docSetBaseY rhsDoc]
                    ]
                  ]
                (Just forallDoc, Nothing) -> docLines
                  [ docSeq
                    [docLitS "=", docSeparator, docForceSingleline forallDoc]
                  , docSeq [docLitS ".", docSeparator, rhsDoc]
                  ]
                (Nothing, Just rhsContextDoc) -> docSeq
                  [ docLitS "="
                  , docSeparator
                  , docSetBaseY $ docLines [rhsContextDoc, docSetBaseY rhsDoc]
                  ]
                (Nothing, Nothing) ->
                  docSeq [docLitS "=", docSeparator, rhsDoc]
              consAltDoc <- return $ (docAlt
                [ -- data D = forall a . Show a => D a
                  docSeq
                  [ docNodeAnnKW ltycl (Just AnnData) $ docSeq
                    [ appSep $ docLitS "data"
                    , docForceSingleline $ lhsContextDoc
                    , appSep $ docLit nameStr
                    , appSep tyVarLine
                    , docSeparator
                    ]
                  , docLitS "="
                  , docSeparator
                  , docSetIndentLevel $ docSeq
                    [ case forallDocMay of
                      Nothing -> docEmpty
                      Just forallDoc ->
                        docSeq
                          [ docForceSingleline forallDoc
                          , docSeparator
                          , docLitS "."
                          , docSeparator
                          ]
                    , maybe docEmpty docForceSingleline rhsContextDocMay
                    , rhsDoc
                    ]
                  ]
                , -- data D
                  --   = forall a . Show a => D a
                  docAddBaseY BrIndentRegular $ docPar
                  (docNodeAnnKW ltycl (Just AnnData) $ docSeq
                    [ appSep $ docLitS "data"
                    , docForceSingleline lhsContextDoc
                    , appSep $ docLit nameStr
                    , tyVarLine
                    ]
                  )
                  (docSeq
                    [ docLitS "="
                    , docSeparator
                    , docSetIndentLevel $ docSeq
                      [ case forallDocMay of
                        Nothing -> docEmpty
                        Just forallDoc ->
                          docSeq
                            [ docForceSingleline forallDoc
                            , docSeparator
                            , docLitS "."
                            , docSeparator
                            ]
                      , maybe docEmpty docForceSingleline rhsContextDocMay
                      , rhsDoc
                      ]
                    ]
                  )
                , -- data D
                  --   = forall a
                  --   . Show a =>
                  --     D a
                  docAddBaseY BrIndentRegular $ docPar
                  (docNodeAnnKW ltycl (Just AnnData) $ docSeq
                    [ appSep $ docLitS "data"
                    , docForceSingleline lhsContextDoc
                    , appSep $ docLit nameStr
                    , tyVarLine
                    ]
                  )
                  consDoc
                , -- data
                  --   Show a =>
                  --   D
                  --   = forall a
                  --   . Show a =>
                  --     D a
                  -- This alternative is only for -XDatatypeContexts.
                  -- But I think it is rather unlikely this will trigger without
                  -- -XDataTypeContexts, especially with the `docNonBottomSpacing`
                  -- above, so while not strictly necessary, this should not
                  -- hurt.
                  docAddBaseY BrIndentRegular $ docPar
                  (docLitS "data")
                  (docLines
                    [ lhsContextDoc
                    , docNodeAnnKW ltycl (Just AnnData)
                      $ docSeq [appSep $ docLit nameStr, tyVarLine]
                    , consDoc
                    ]
                  )
                ])
              createDerivingPar mDerivs consAltDoc
        _ -> briDocByExactNoComment DataDeclarationFallback ltycl)

  _ -> briDocByExactNoComment DataDeclarationFallback ltycl
 where
  simpleConstructor = \case
    L _ (ConDeclH98 _ constructorName False [] context details _)
      | contextIsEmpty context -> Just (constructorName, details)
    _ -> Nothing

  contextIsEmpty Nothing = True
  contextIsEmpty (Just (L _ context)) = null context

  layoutMultipleConstructors context derivings constructors = docWrapNode ltycl $ do
    lhsContextDoc <- docSharedWrapper createContextDoc
      $ unLoc (maybe (L noSrcSpan []) toL context)
    nameStr <- lrdrNameToTextAnn name
    tyVarLine <- return <$> createBndrDoc bndrs
    constructorDocs <- constructors `forM` \(constructorName, details) -> do
      constructorNameStr <- applyNameAdornment constructorName
        <$> lrdrNameToTextAnn (toL constructorName)
      return <$> createDetailsDoc constructorNameStr details
    let header = docNodeAnnKW ltycl (Just AnnData) $ docSeq
          [ appSep $ docLitS "data"
          , docForceSingleline lhsContextDoc
          , appSep $ docLit nameStr
          , tyVarLine
          ]
        constructorLines = zipWith
          (\separator constructorDoc -> docSeq
            [ docLitS separator
            , docSeparator
            , docSetBaseY constructorDoc
            ])
          ("=" : repeat "|")
          constructorDocs
    createDerivingPar derivings
      $ docAddBaseY BrIndentRegular
      $ docPar header
      $ docLines constructorLines

createContextDoc :: HsContext GhcPs -> ToBriDocM BriDocNumbered
createContextDoc [] = docEmpty
createContextDoc [t] =
  docSeq [layoutType (toL t), docSeparator, docLitS "=>", docSeparator]
createContextDoc (t1 : tR) = do
  t1Doc <- docSharedWrapper layoutType (toL t1)
  tRDocs <- tR `forM` (docSharedWrapper layoutType . toL)
  docAlt
    [ docSeq
      [ docLitS "("
      , docForceSingleline $ docSeq $ List.intersperse
        docCommaSep
        (t1Doc : tRDocs)
      , docLitS ") =>"
      , docSeparator
      ]
    , docLines $ join
      [ [docSeq [docLitS "(", docSeparator, t1Doc]]
      , tRDocs <&> \tRDoc -> docSeq [docLitS ",", docSeparator, tRDoc]
      , [docLitS ") =>", docSeparator]
      ]
    ]

createBndrDoc :: [LHsTyVarBndr flag GhcPs] -> ToBriDocM BriDocNumbered
createBndrDoc bs = do
  tyVarDocs <- bs `forM` \lb -> case unLoc lb of
    (HsTvb _ _ (HsBndrVar _ vname) (HsBndrNoKind _)) ->
      return $ (lrdrNameToText (toL vname), Nothing)
    (HsTvb _ _ (HsBndrVar _ lrdrName) (HsBndrKind _ kind)) -> do
      d <- docSharedWrapper layoutType (toL kind)
      return $ (lrdrNameToText (toL lrdrName), Just d)
    _ -> error "createBndrDoc: unexpected HsTyVarBndr"
  docSeq $ List.intersperse docSeparator $ tyVarDocs <&> \(vname, mKind) ->
    case mKind of
      Nothing -> docLit vname
      Just kind -> docSeq
        [ docLitS "("
        , docLit vname
        , docSeparator
        , docLitS "::"
        , docSeparator
        , kind
        , docLitS ")"
        ]

createDerivingPar
  :: HsDeriving GhcPs -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
createDerivingPar derivs mainDoc = do
  case derivs of
    [] -> mainDoc
    types ->
      docPar mainDoc
        $ docEnsureIndent BrIndentRegular
        $ docLines
        $ derivingClauseDoc
        <$> types

derivingClauseDoc :: LHsDerivingClause GhcPs -> ToBriDocM BriDocNumbered
derivingClauseDoc (L _ (HsDerivingClause _ext mStrategy lTys)) =
  let ts = case lTys of
        L _ (DctSingle _ ty) -> [ty]
        L _ (DctMulti _ tys) -> tys
        _ -> []
      tsLength = length ts
      whenMoreThan1Type val = if tsLength > 1 then docLitS val else docLitS ""
      (lhsStrategy, rhsStrategy) =
        maybe (docEmpty, docEmpty) strategyLeftRight (fmap toL mStrategy)
  in if null ts then docSeq []
     else docSeq
       [ docDeriving
       , docWrapNodePrior (toL lTys) $ lhsStrategy
       , docSeparator
       , whenMoreThan1Type "("
       , docWrapNodeRest (toL lTys)
       $ docSeq
       $ List.intersperse docCommaSep
       $ ((\lty -> case unLoc lty of HsSig _ _ body -> layoutType (toL body); _ -> docLitS "?") <$> ts)
       , whenMoreThan1Type ")"
       , rhsStrategy
       ]
 where
  strategyLeftRight = \case
    (L _ (StockStrategy _)) -> (docLitS " stock", docEmpty)
    (L _ (AnyclassStrategy _)) -> (docLitS " anyclass", docEmpty)
    (L _ (NewtypeStrategy _)) -> (docLitS " newtype", docEmpty)
    lVia@(L _ (ViaStrategy (XViaStrategyPs _ viaType))) ->
      ( docEmpty
      , docSeq
        [ docWrapNode (toL lVia) $ docLitS " via"
        , docSeparator
        , layoutSigType viaType
        ]
      )

  layoutSigType :: LHsSigType GhcPs -> ToBriDocM BriDocNumbered
  layoutSigType (L _ (HsSig _ _ body)) = layoutType (toL body)

docDeriving :: ToBriDocM BriDocNumbered
docDeriving = docLitS "deriving"

createDetailsDoc
  :: Text -> HsConDeclH98Details GhcPs -> (ToBriDocM BriDocNumbered)
createDetailsDoc consNameStr details = case details of
  PrefixCon args -> do
    indentPolicy <- mAsk <&> _conf_layout .> _lconfig_indentPolicy .> confUnpack
    let argTypes = fmap (toL . cdf_type) args
        singleLine = docSeq
          [ docLit consNameStr
          , docSeparator
          , docForceSingleline
          $ docSeq
          $ List.intersperse docSeparator
          $ (layoutType <$> argTypes)
          ]
        leftIndented =
          docSetParSpacing
            . docAddBaseY BrIndentRegular
            . docPar (docLit consNameStr)
            . docLines
            $ layoutType <$> argTypes
        multiAppended = docSeq
          [ docLit consNameStr
          , docSeparator
          , docSetBaseY $ docLines $ layoutType <$> argTypes
          ]
        multiIndented = docSetBaseY $ docAddBaseY BrIndentRegular $ docPar
          (docLit consNameStr)
          (docLines $ layoutType <$> argTypes)
    case indentPolicy of
      IndentPolicyLeft -> docAlt [singleLine, leftIndented]
      IndentPolicyMultiple -> docAlt [singleLine, multiAppended, leftIndented]
      IndentPolicyFree ->
        docAlt [singleLine, multiAppended, multiIndented, leftIndented]
  RecCon (L _ []) ->
    docSeq [docLit consNameStr, docSeparator, docLit $ Text.pack "{}"]
  RecCon lRec@(L _ fields@(_ : _)) -> do
    let ((fName1, fType1) : fDocR) = mkFieldDocs fields
    -- allowSingleline <- mAsk <&> _conf_layout .> _lconfig_allowSinglelineRecord .> confUnpack
    let allowSingleline = False
    docAddBaseY BrIndentRegular $ runFilteredAlternative $ do
        -- single-line: { i :: Int, b :: Bool }
      addAlternativeCond allowSingleline $ docSeq
        [ docLit consNameStr
        , docSeparator
        , docWrapNodePrior (toL lRec) $ docLitS "{"
        , docSeparator
        , docWrapNodeRest (toL lRec)
        $ docForceSingleline
        $ docSeq
        $ join
        $ [fName1, docSeparator, docLitS "::", docSeparator, fType1]
        : [ [ docLitS ","
            , docSeparator
            , fName
            , docSeparator
            , docLitS "::"
            , docSeparator
            , fType
            ]
          | (fName, fType) <- fDocR
          ]
        , docSeparator
        , docLitS "}"
        ]
      addAlternative $ docPar
        (docLit consNameStr)
        (docWrapNodePrior (toL lRec) $ docNonBottomSpacingS $ docLines
          [ docAlt
            [ docCols
              ColRecDecl
              [ appSep (docLitS "{")
              , appSep $ docForceSingleline fName1
              , docSeq [docLitS "::", docSeparator]
              , docForceSingleline $ fType1
              ]
            , docSeq
              [ docLitS "{"
              , docSeparator
              , docSetBaseY $ docAddBaseY BrIndentRegular $ docPar
                fName1
                (docSeq [docLitS "::", docSeparator, fType1])
              ]
            ]
          , docWrapNodeRest (toL lRec) $ docLines $ fDocR <&> \(fName, fType) ->
            docAlt
              [ docCols
                ColRecDecl
                [ docCommaSep
                , appSep $ docForceSingleline fName
                , docSeq [docLitS "::", docSeparator]
                , docForceSingleline fType
                ]
              , docSeq
                [ docLitS ","
                , docSeparator
                , docSetBaseY $ docAddBaseY BrIndentRegular $ docPar
                  fName
                  (docSeq [docLitS "::", docSeparator, fType])
                ]
              ]
          , docLitS "}"
          ]
        )
  InfixCon arg1 arg2 -> docSeq
    [ layoutType $ toL (cdf_type arg1)
    , docSeparator
    , docLit consNameStr
    , docSeparator
    , layoutType $ toL (cdf_type arg2)
    ]
 where
  mkFieldDocs
    :: [LHsConDeclRecField GhcPs]
    -> [(ToBriDocM BriDocNumbered, ToBriDocM BriDocNumbered)]
  mkFieldDocs = fmap $ \lField -> case lField of
    L _ (HsConDeclRecField { cdrf_spec = cdf, cdrf_names = nameList }) ->
      let t = cdf_type cdf
      in createNamesAndTypeDoc (toL lField) nameList (toL t)

createForallDoc
  :: [LHsTyVarBndr flag GhcPs] -> Maybe (ToBriDocM BriDocNumbered)
createForallDoc [] = Nothing
createForallDoc lhsTyVarBndrs =
  Just $ docSeq [docLitS "forall ", createBndrDoc lhsTyVarBndrs]

createNamesAndTypeDoc
  :: Data.Data.Data ast
  => Located ast
  -> [GenLocated t (FieldOcc GhcPs)]
  -> Located (HsType GhcPs)
  -> (ToBriDocM BriDocNumbered, ToBriDocM BriDocNumbered)
createNamesAndTypeDoc lField names t =
  ( docNodeAnnKW lField Nothing $ docWrapNodePrior lField $ docSeq
    [ docSeq $ List.intersperse docCommaSep $ names <&> \case
        L _ (FieldOcc _ lname) -> docLit =<< lrdrNameToTextAnn (toL lname)
    ]
  , docWrapNodeRest lField $ layoutType t
  )
