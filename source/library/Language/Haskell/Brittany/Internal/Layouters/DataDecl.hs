{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.DataDecl where

import GHC (GenLocated(L), Located, unLoc)
import GHC.Hs
import GHC.Types.SrcLoc (noSrcSpan)
import qualified GHC.OldList as List
import Language.Haskell.Brittany.Internal.ExactPrintCompat (AnnKeywordId(..))
import Language.Haskell.Brittany.Internal.Fallbacks (FallbackId(..))
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.DataDecl.Boundary
import Language.Haskell.Brittany.Internal.Layouters.DataDecl.Constructor
import Language.Haskell.Brittany.Internal.Layouters.DataDecl.Deriving
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
  HsDataDefn
    { dd_ctxt = mCtxt
    , dd_cType = mCType
    , dd_kindSig = mKindSig
    , dd_cons = cons
    , dd_derivs = mDerivs
    } -> case cons of
  -- newtype MyType a b = MyType ..
    NewTypeCon lcons ->
      case lcons of
        L _ (ConDeclH98 _ consName False [] context details _)
          | contextIsEmpty mCtxt && contextIsEmpty context -> do
              hasPriorComments <- constructorHasPriorComments (toL lcons)
              if hasPriorComments
                then layoutH98Constructors
                  "newtype" createAnnotatedDetailsDoc mCtxt mDerivs
                  [(lcons, consName, [], Nothing, details)]
                else docWrapNode ltycl $ do
                  nameStr <- lrdrNameToTextAnn name
                  consNameStr <- applyNameAdornment consName
                    <$> lrdrNameToTextAnn (toL consName)
                  tyVarLine <- return <$> createBndrDoc bndrs
                  rhsDoc <- return <$> createDetailsDoc consNameStr details
                  constructorDoc <- docWrapNode (toL lcons) $ docSeq
                    [ appSep $ docLitS "newtype"
                    , appSep $ docLit nameStr
                    , appSep tyVarLine
                    , docSeparator
                    , docLitS "="
                    , docSeparator
                    , rhsDoc
                    ]
                  createDerivingPar mDerivs $ return constructorDoc
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
    DataTypeCons _ constructors@(_ : _ : _) ->
      case traverse h98Constructor constructors of
        Just h98Constructors ->
          layoutH98Constructors
            "data" createMultipleDetailsDoc mCtxt mDerivs h98Constructors
        Nothing -> case (mCType, traverse simpleGadtConstructor constructors) of
          (Nothing, Just simpleConstructors) ->
            layoutGadtConstructors mKindSig mCtxt mDerivs simpleConstructors
          _ -> briDocByExactNoComment DataDeclarationFallback ltycl
    DataTypeCons _ [lcons] -> do
      hasPriorComments <- constructorHasPriorComments (toL lcons)
      case ( simpleConstructor lcons
        , hasPriorComments
        , contextIsEmpty mCtxt
        , mCType
        , mKindSig
        ) of
        (Just constructor, True, True, Nothing, Nothing) ->
          layoutH98Constructors
            "data" createAnnotatedDetailsDoc mCtxt mDerivs [constructor]
        _ -> (case (mCType, simpleGadtConstructor lcons) of
          (Nothing, Just constructor) ->
            layoutGadtConstructors mKindSig mCtxt mDerivs [constructor]
          _ -> case lcons of {
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
              verticalRhsDoc <-
                return <$> createVerticalDetailsDoc consNameStr details
              let useStructuralPrefix = case (details, forallDocMay, rhsContextDocMay) of
                    (PrefixCon{}, Nothing, Nothing) -> True
                    _ -> False
                  chooseStructural structural = if useStructuralPrefix
                    then structural else rhsDoc
                  compactRhsDoc = chooseStructural $ docForceSingleline rhsDoc
                  structuralRhsDoc = chooseStructural verticalRhsDoc
                  anchoredStructuralRhsDoc = chooseStructural
                    $ docSetBaseY verticalRhsDoc
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
                    , docSetBaseY
                    $ docLines [rhsContextDoc, docSetBaseY structuralRhsDoc]
                    ]
                  ]
                (Just forallDoc, Nothing) -> docLines
                  [ docSeq
                    [docLitS "=", docSeparator, docForceSingleline forallDoc]
                  , docSeq
                    [docLitS ".", docSeparator, anchoredStructuralRhsDoc]
                  ]
                (Nothing, Just rhsContextDoc) -> docSeq
                  [ docLitS "="
                  , docSeparator
                  , docSetBaseY
                    $ docLines [rhsContextDoc, docSetBaseY structuralRhsDoc]
                  ]
                (Nothing, Nothing) ->
                  docSeq
                    [docLitS "=", docSeparator, anchoredStructuralRhsDoc]
              consAltDoc <- return <$> docWrapNode (toL lcons) (docAlt
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
                    , compactRhsDoc
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
                      , anchoredStructuralRhsDoc
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
        ; _ -> briDocByExactNoComment DataDeclarationFallback ltycl })

  _ -> briDocByExactNoComment DataDeclarationFallback ltycl
 where
  h98Constructor = \case
    lcons@(L _ (ConDeclH98 _ constructorName _ qvars context details _)) ->
      Just (lcons, constructorName, qvars, context, details)
    _ -> Nothing

  simpleConstructor = \case
    lcons@(L _ (ConDeclH98 _ constructorName False [] context details _))
      | contextIsEmpty context ->
          Just (lcons, constructorName, [], context, details)
    _ -> Nothing

  simpleGadtConstructor = \case
    lcons@(L _ (ConDeclGADT _ (constructorName :| [])
      (L _ (HsOuterImplicit _)) [] Nothing
      (PrefixConGADT _ arguments) resultType _))
      | all simpleGadtArgument arguments ->
          Just (lcons, constructorName, arguments, resultType)
    _ -> Nothing

  simpleGadtArgument = \case
    CDF _ _ _ (HsUnannotated _) _ Nothing -> True
    _ -> False

  contextIsEmpty Nothing = True
  contextIsEmpty (Just (L _ context)) = null context

  layoutH98Constructors
    keyword detailsLayout context derivings constructors =
    docWrapNode ltycl $ do
    lhsContextDoc <- docSharedWrapper createContextDoc
      $ unLoc (maybe (L noSrcSpan []) toL context)
    nameStr <- lrdrNameToTextAnn name
    tyVarLine <- return <$> createBndrDoc bndrs
    constructorLayouts <- constructors `forM`
      \(lcons, constructorName, qvars, rhsContext, details) -> do
      constructorNameStr <- applyNameAdornment constructorName
        <$> lrdrNameToTextAnn (toL constructorName)
      let forallParts = case createForallDoc qvars of
            Nothing -> []
            Just forallDoc ->
              [forallDoc, docSeparator, docLitS ".", docSeparator]
          contextParts = case rhsContext of
            Nothing -> []
            Just (L _ contextTypes) -> [createContextDoc contextTypes]
          detailsDoc = docSeq
            $ forallParts
            ++ contextParts
            ++ [detailsLayout constructorNameStr details]
      buildConstructorLayout (toL lcons) detailsDoc
    let header = docNodeAnnKW ltycl (Just AnnData) $ docSeq
          [ appSep $ docLitS keyword
          , docForceSingleline lhsContextDoc
          , appSep $ docLit nameStr
          , tyVarLine
          ]
        constructorLines = zipWith renderH98Constructor
          ("=" : repeat "|")
          constructorLayouts
        multilineLayout = docAddBaseY BrIndentRegular
          $ docPar header
          $ docLines constructorLines
    createDerivingPar derivings multilineLayout

  layoutGadtConstructors kindSignature context derivings constructors =
    docWrapNode ltycl $ do
    lhsContextDoc <- docSharedWrapper createContextDoc
      $ unLoc (maybe (L noSrcSpan []) toL context)
    nameStr <- lrdrNameToTextAnn name
    tyVarLine <- return <$> createBndrDoc bndrs
    kindDoc <- traverse (docSharedWrapper layoutType . toL) kindSignature
    constructorLayouts <- constructors `forM`
      \(lcons, constructorName, arguments, resultType) -> do
        constructorNameStr <- applyNameAdornment constructorName
          <$> lrdrNameToTextAnn (toL constructorName)
        buildConstructorLayout (toL lcons)
          $ createGadtDetailsDoc
            (toL lcons) constructorNameStr arguments resultType
    let header = docNodeAnnKW ltycl (Just AnnData) $ docSeq
          [ appSep $ docLitS "data"
          , docForceSingleline lhsContextDoc
          , appSep $ docLit nameStr
          , appSep tyVarLine
          , case kindDoc of
              Nothing -> docEmpty
              Just doc -> docSeq
                [ docSeparator
                , docLitS "::"
                , docSeparator
                , doc
                ]
          , docSeparator
          , docLitS "where"
          ]
    createDerivingPar derivings
      $ docAddBaseY BrIndentRegular
      $ docPar header
      $ docLines $ renderGadtConstructor <$> constructorLayouts

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

createForallDoc
  :: [LHsTyVarBndr flag GhcPs] -> Maybe (ToBriDocM BriDocNumbered)
createForallDoc [] = Nothing
createForallDoc lhsTyVarBndrs =
  Just $ docSeq [docLitS "forall ", createBndrDoc lhsTyVarBndrs]
