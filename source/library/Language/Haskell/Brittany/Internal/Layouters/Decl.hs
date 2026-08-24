{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
-- GHC extension constructors are retained as exact-print fallbacks even when
-- their GhcPs instantiations are uninhabited in GHC 9.14.
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Language.Haskell.Brittany.Internal.Layouters.Decl where

import qualified Data.Data
import qualified Data.Foldable
import Data.Kind (Type)
import qualified Data.Maybe
import qualified Data.Map as Map
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as Text
import GHC (GenLocated(L))
import qualified GHC.Data.FastString as FastString
import GHC.Hs hiding (anns)
import qualified GHC.OldList as List
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Types.Basic
  ( Activation(..)
  , InlinePragma(..)
  , InlineSpec(..)
  , RuleMatchInfo(..)
  )
import GHC.Types.Name.Occurrence (isSymOcc)
import GHC.Types.Name.Reader (rdrNameOcc)
import Language.Haskell.Syntax.Basic (LexicalFixity(..))
import GHC.Types.SrcLoc (Located, SrcSpan(..), getLoc, noSrcSpan, srcSpanStartLine, srcSpanEndLine, unLoc)
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.ExactPrintCompat (AnnKeywordId(..), AnnKey, Anns, mkAnnKey, Comment(..))
import Language.Haskell.Brittany.Internal.ExactPrintUtils
import Language.Haskell.Brittany.Internal.Fallbacks (FallbackId(..))
import Language.Haskell.Brittany.Internal.ExpressionComments
  ( commentSensitiveExpressions
  , exactSourceExpressions
  )
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.DataDecl
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Expr
import Language.Haskell.Brittany.Internal.Layouters.Pattern
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Stmt
import Language.Haskell.Brittany.Internal.Layouters.IE (toL)
import Language.Haskell.Brittany.Internal.Layouters.Type
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.PatternComments
  ( commentSensitivePatterns
  , exactSourcePatterns
  )
import Language.Haskell.Brittany.Internal.TypeFallbacks
  ( exactSourceTypes
  , requiresExactTypeDeclaration
  )
import Language.Haskell.Brittany.Internal.Types
import qualified Language.Haskell.GHC.ExactPrint.Utils as ExactPrint



layoutDecl :: ToBriDoc HsDecl
layoutDecl = layoutDeclWithExactText Nothing False

layoutDeclWithExactText :: Maybe Text.Text -> Bool -> ToBriDoc HsDecl
layoutDeclWithExactText exactText hasSourceComments d@(L loc decl) = case decl of
  SigD _ sig@TypeSig{}
    | requiresExactTypes d -> layoutExact TypeFallback d exactText
    | otherwise -> layoutExactWhenCommented d $ layoutSig (L loc sig)
  SigD _ sig
    | requiresExactSignature sig -> layoutExact SignatureFallback d exactText
  SigD _ sig -> withTransformedAnns d $ docWrapNode d $ layoutSig (L loc sig)
  ValD _ bind -> layoutValueDeclaration d bind
  TyClD _ tycl
    | requiresExactTypeDeclaration tycl || requiresExactTypes d ->
        layoutExact TypeClassDeclarationFallback d exactText
    | otherwise -> layoutExactWhenCommented d $ layoutTyCl (L loc tycl)
  InstD _ (TyFamInstD _ tfid) ->
    layoutExactWhenCommented d $ layoutTyFamInstDecl False d tfid
  InstD _ (ClsInstD _ inst) ->
    layoutExactWhenCommented d $ do
      followComments <- astFollowingComments d
      docSeq
        [ layoutClsInst (L loc inst)
        , docSeq [docLitS (" " ++ commentContents c) | (c, _) <- followComments]
        ]
  InstD _ (DataFamInstD _ _) ->
    layoutExactOrCommented d $ withTransformedAnns d $ do
      followComments <- astFollowingComments d
      docSeq $ [briDocByExactNoComment DeclarationFallback d]
        ++ [docLitS (" " ++ commentContents c)
           | (c, _) <- followComments]
  ForD{} -> layoutExact DeclarationFallback d exactText
  WarningD{} -> layoutExact DeclarationFallback d exactText
  AnnD{} -> layoutExact DeclarationFallback d exactText
  RuleD{} -> layoutExact DeclarationFallback d exactText
  SpliceD{} -> layoutExact DeclarationFallback d exactText
  _ -> withTransformedAnns d
    $ docWrapNode d
    $ briDocByExactNoComment DeclarationFallback d
 where
  layoutValueDeclaration declaration bind = do
    let sensitiveExpressions = commentSensitiveExpressions declaration
    let sensitivePatterns = commentSensitivePatterns declaration
    hasConnectedExpressionComments <- orM
      $ hasAnyRegularCommentsConnected
      <$> sensitiveExpressions
    hasConnectedPatternComments <- orM
      $ hasAnyRegularCommentsConnected
      <$> sensitivePatterns
    let hasSensitiveNodes = not
          $ null sensitiveExpressions && null sensitivePatterns
    let hasSensitiveComments = hasConnectedExpressionComments
          || hasConnectedPatternComments
          || (hasSourceComments && hasSensitiveNodes)
    let requiresExactLayout = or
          [ not $ null (exactSourceExpressions declaration)
          , not $ null (exactSourcePatterns declaration)
          , requiresExactTypes declaration
          , requiresExactBinding bind
          ]
    if hasSensitiveComments || requiresExactLayout
      then layoutExact DeclarationFallback declaration exactText
      else withTransformedAnns declaration
        $ docWrapNode declaration
        $ layoutBind (L loc bind) >>= \case
          Left ns -> docLines $ return <$> ns
          Right n -> return n

  layoutExactWhenCommented declaration formatted = do
    layoutExactOrCommented declaration
      $ withTransformedAnns declaration
      $ docWrapNode declaration formatted

  layoutExactOrCommented declaration formatted = do
    hasConnectedComments <- hasAnyRegularCommentsConnected declaration
    let hasComments = hasSourceComments || hasConnectedComments
    if hasComments
      then layoutExact DeclarationFallback declaration exactText
      else formatted

  layoutExact fallback declaration source = case source of
    Just text -> do
      annotationMap :: Anns <- mAsk
      briDocByExactTextWithAnnsNoComment
        fallback
        declaration
        (Map.keysSet annotationMap)
        text
    Nothing -> briDocByExactNoComment fallback declaration

  requiresExactTypes = not . null . exactSourceTypes

  requiresExactBinding = \case
    PatSynBind{} -> True
    _ -> False

  requiresExactSignature = \case
    SpecSig{} -> True
    SpecSigE{} -> True
    SpecInstSig{} -> True
    _ -> False

--------------------------------------------------------------------------------
-- Sig
--------------------------------------------------------------------------------

layoutSig :: ToBriDoc Sig
layoutSig lsig@(L _loc sig) = case sig of
  TypeSig _ names sigTy -> case sigTy of
    HsWC _ body -> case unLoc body of
      HsSig{} -> layoutNamesAndType Nothing names body
      _ -> briDocByExactNoComment SignatureFallback (toL lsig)
    _ -> briDocByExactNoComment SignatureFallback (toL lsig)
  InlineSig _ name (InlinePragma _ spec _arity phaseAct conlike) ->
    docWrapNode (toL lsig) $ do
      nameStr <- applyNameAdornment name <$> lrdrNameToTextAnn (toL name)
      specStr <- specStringCompat (toL lsig) spec
      let
        phaseStr = case phaseAct of
          NeverActive -> "" -- not [] - for NOINLINE NeverActive is
                                 -- in fact the default
          AlwaysActive -> ""
          ActiveBefore _ i -> "[~" ++ show i ++ "] "
          ActiveAfter _ i -> "[" ++ show i ++ "] "
          FinalActive -> error "brittany internal error: FinalActive"
      let
        conlikeStr = case conlike of
          FunLike -> ""
          ConLike -> "CONLIKE "
      docLit
        $ Text.pack ("{-# " ++ specStr ++ conlikeStr ++ phaseStr)
        <> nameStr
        <> Text.pack " #-}"
  ClassOpSig _ False names sigTy -> case unLoc sigTy of
    HsSig{} -> layoutNamesAndType Nothing names sigTy
    _ -> briDocByExactNoComment SignatureFallback (toL lsig)
  PatSynSig _ names sigTy -> case unLoc sigTy of
    HsSig{} -> layoutNamesAndType (Just "pattern") names sigTy
    _ -> briDocByExactNoComment SignatureFallback (toL lsig)
  _ -> briDocByExactNoComment SignatureFallback (toL lsig) -- TODO
 where
  layoutNamesAndType mKeyword names sigType = docWrapNode (toL lsig) $ do
    let
      keyDoc = case mKeyword of
        Just key -> [appSep . docLit $ Text.pack key]
        Nothing -> []
    nameStrs <- names `forM` \n -> applyNameAdornment n <$> lrdrNameToTextAnn (toL n)
    let nameStr = Text.intercalate (Text.pack ", ") $ nameStrs
    (mForallParts, typ) <- case unLoc sigType of
      HsSig _ bndrs typ ->
        let mForallParts = case bndrs of
              HsOuterExplicit _ bndrsList ->
                Just $ do
                  let bndrs' = map invisBinderToUnit bndrsList
                  tyVarDocs <- layoutTyVarBndrs bndrs'
                  let tyVarDocLineList = processTyVarBndrsSingleline tyVarDocs
                  let forallWithDot = docSeq
                        ( [ docLit (Text.pack "forall") ]
                        ++ tyVarDocLineList
                        ++ [ docLit (Text.pack " . ") ]
                        )
                  let forallNoDot = docSeq
                        ( [ docLit (Text.pack "forall") ]
                        ++ tyVarDocLineList
                        )
                  return (forallWithDot, forallNoDot)
              HsOuterImplicit _ -> Nothing
        in return (mForallParts, typ)
      _ -> return (Nothing, error "layoutNamesAndType: unexpected sigType")
    typeDoc <- docSharedWrapper layoutType (toL typ)
    fullTypeDoc <- case mForallParts of
      Nothing -> return typeDoc
      Just mfp -> do
        (forallWithDot, forallNoDot) <- mfp
        -- Check if inner type is HsQualTy to build proper multiline layout
        case unLoc typ of
          HsQualTy _ lcntxts typ2 -> do
            let lcntxts' = toL lcntxts
                cntxts' = map toL (unLoc lcntxts)
            innerTypeDoc <- docSharedWrapper layoutType (toL typ2)
            cntxtDocs <- cntxts' `forM` docSharedWrapper layoutType
            let
              contextDoc = docWrapNode lcntxts' $ case cntxtDocs of
                [] -> docLit $ Text.pack "()"
                [x] -> x
                firstContext : remainingContexts -> docAlt
                  [ let open = docLit $ Text.pack "("
                        close = docLit $ Text.pack ")"
                        list = List.intersperse docCommaSep
                             $ docForceSingleline <$> cntxtDocs
                    in docSeq ([open] ++ list ++ [close])
                  , let open = docCols ColTyOpPrefix
                          [docParenLSep, docAddBaseY (BrIndentSpecial 2) firstContext]
                        close = docLit $ Text.pack ")"
                        list = remainingContexts <&> \cntxtDoc ->
                          docCols ColTyOpPrefix
                            [docCommaSep, docAddBaseY (BrIndentSpecial 2) cntxtDoc]
                    in docPar open $ docLines $ list ++ [close]
                  ]
              maybeForceML = case toL typ2 of
                (L _ HsFunTy{}) -> docForceMultiline
                _ -> id
            return $ docAlt
              -- forall m . Foo => ColMap2 -> ColInfo -> ...  (all on one line)
              [ docSeq
                [ forallWithDot
                , docForceSingleline contextDoc
                , docLit $ Text.pack " => "
                , docForceSingleline innerTypeDoc
                ]
              -- forall m
              --  . Foo
              -- => ColMap2
              -- -> ColInfo
              , docPar
                  forallNoDot
                  (docLines
                    [ docCols ColTyOpPrefix
                      [ docLit $ Text.pack " . "
                      , docAddBaseY (BrIndentSpecial 3) contextDoc
                      ]
                    , docCols ColTyOpPrefix
                      [ docLit $ Text.pack "=> "
                      , docAddBaseY (BrIndentSpecial 3) $ maybeForceML innerTypeDoc
                      ]
                    ]
                  )
              ]
          _ -> do
            let maybeForceML' = case unLoc typ of
                  HsFunTy{} -> docForceMultiline
                  _ -> id
            return $ docAlt
              [ docSeq [forallWithDot, docForceSingleline typeDoc]
              , docPar
                  forallNoDot
                  (docCols
                    ColTyOpPrefix
                    [ docLit $ Text.pack " . "
                    , docAddBaseY (BrIndentSpecial 3) $ maybeForceML' typeDoc
                    ]
                  )
              ]
    hasComments <- hasAnyCommentsBelow (toL lsig)
    shouldBeHanging <-
      mAsk <&> _conf_layout .> _lconfig_hangingTypeSignature .> confUnpack
    if shouldBeHanging
      then
        docSeq
          $ [ appSep
            $ docWrapNodeRest (toL lsig)
            $ docSeq
            $ keyDoc
            <> [docLit nameStr]
            , docSetBaseY $ docLines
              [ docCols
                  ColTyOpPrefix
                  [ docLit $ Text.pack ":: "
                  , docAddBaseY (BrIndentSpecial 3) $ fullTypeDoc
                  ]
              ]
            ]
      else layoutLhsAndType
        hasComments
        (appSep . docWrapNodeRest (toL lsig) . docSeq $ keyDoc <> [docLit nameStr])
        "::"
        fullTypeDoc

specStringCompat
  :: MonadMultiWriter [BrittanyError] m => Located (Sig GhcPs) -> InlineSpec -> m String
specStringCompat ast = \case
  NoUserInlinePrag -> mTell [ErrorUnknownNode "NoUserInlinePrag" ast] $> ""
  Inline _ -> pure "INLINE "
  Inlinable _ -> pure "INLINABLE "
  NoInline _ -> pure "NOINLINE "
  Opaque _ -> pure "OPAQUE "

layoutGuardLStmt :: ToBriDoc' (Stmt GhcPs (LHsExpr GhcPs))
layoutGuardLStmt lgstmt@(L _ stmtLR) = docWrapNode (toL lgstmt) $ case stmtLR of
  BodyStmt _ body _ _ -> layoutExpr (toL body)
  BindStmt _ lPat expr -> do
    patDoc <- docSharedWrapper layoutPat lPat
    expDoc <- docSharedWrapper layoutExpr (toL expr)
    docCols
      ColBindStmt
      [ appSep $ colsWrapPat =<< patDoc
      , docSeq [appSep $ docLit $ Text.pack "<-", expDoc]
      ]
  _ -> unknownNodeError "" lgstmt -- TODO


--------------------------------------------------------------------------------
-- HsBind
--------------------------------------------------------------------------------

layoutBind
  :: ToBriDocC (HsBindLR GhcPs GhcPs) (Either [BriDocNumbered] BriDocNumbered)
layoutBind lbind@(L _ bind) = case bind of
  FunBind _ fId (MG _ lmatches@(L _ matches)) -> do
    idStr <- applyNameAdornment fId <$> lrdrNameToTextAnn (toL fId)
    binderDoc <- docLit $ Text.pack "="
    funcPatDocs <-
      docWrapNode (toL lbind)
      $ docWrapNode (toL lmatches)
      $ layoutPatternBind (Just idStr) binderDoc
      `mapM` matches
    return $ Left $ funcPatDocs
  PatBind _ pat _ (GRHSs _ grhssNE whereBinds) -> do
    let grhss = NonEmpty.toList grhssNE
    patDocs <- colsWrapPat =<< layoutPat pat
    clauseDocs <- layoutGrhs `mapM` grhss
    mWhereDocs <- layoutLocalBinds (L noSrcSpan whereBinds)
    let mWhereArg = mWhereDocs <&> (,) (mkAnnKey (toL lbind)) -- TODO: is this the right AnnKey?
    binderDoc <- docLit $ Text.pack "="
    hasComments <- hasAnyCommentsBelow (toL lbind)
    fmap Right $ docWrapNode (toL lbind) $ layoutPatternBindFinal
      Nothing
      binderDoc
      (Just patDocs)
      clauseDocs
      mWhereArg
      hasComments
  PatSynBind _ (PSB _ patID lpat rpat dir) -> do
    fmap Right $ docWrapNode (toL lbind) $ layoutPatSynBind (toL patID) lpat dir rpat
  _ -> Right <$> unknownNodeError "" (toL lbind)
layoutIPBind :: ToBriDoc IPBind
layoutIPBind lipbind@(L _ bind) = case bind of
  IPBind _ lipName expr -> case unLoc lipName of
    HsIPName name -> do
      ipName <- docLit $ Text.pack $ '?' : FastString.unpackFS name
      binderDoc <- docLit $ Text.pack "="
      exprDoc <- layoutExpr (toL expr)
      hasComments <- hasAnyCommentsBelow (toL lipbind)
      layoutPatternBindFinal
        Nothing
        binderDoc
        (Just ipName)
        [([], exprDoc, expr)]
        Nothing
        hasComments
    _ -> briDocByExactNoComment ImplicitParameterFallback (toL lipbind)


type BagBindOrSig :: Type
data BagBindOrSig = BagBind (LHsBindLR GhcPs GhcPs)
                  | BagSig (LSig GhcPs)

bindOrSigtoSrcSpan :: BagBindOrSig -> SrcSpan
bindOrSigtoSrcSpan (BagBind b) = getLocA b
bindOrSigtoSrcSpan (BagSig s) = getLocA s

layoutLocalBinds
  :: ToBriDocC (HsLocalBindsLR GhcPs GhcPs) (Maybe [BriDocNumbered])
layoutLocalBinds lbinds@(L _ binds) = case binds of
  -- HsValBinds (ValBindsIn lhsBindsLR []) ->
  --   Just . (>>= either id return) . Data.Foldable.toList <$> mapBagM layoutBind lhsBindsLR -- TODO: fix ordering
  -- x@(HsValBinds (ValBindsIn{})) ->
  --   Just . (:[]) <$> unknownNodeError "HsValBinds (ValBindsIn _ (_:_))" x
  HsValBinds _ (ValBinds _ bindlrs sigs) -> do
    let
      unordered =
        [ BagBind b | b <- Data.Foldable.toList bindlrs ]
        ++ [ BagSig s | s <- sigs ]
      ordered = List.sortOn (ExactPrint.rs . bindOrSigtoSrcSpan) unordered
    docs <- docWrapNode lbinds $ join <$> ordered `forM` \case
      BagBind b -> either id return <$> layoutBind (toL b)
      BagSig s -> return <$> layoutSig (toL s)
    return $ Just $ docs
--  x@(HsValBinds (ValBindsOut _binds _lsigs)) ->
  HsValBinds _ (XValBindsLR{}) -> error "brittany internal error: XValBindsLR"
  HsIPBinds _ (IPBinds _ bb) -> Just <$> mapM (layoutIPBind . toL) bb
  EmptyLocalBinds{} -> return $ Nothing

-- TODO: we don't need the `LHsExpr GhcPs` anymore, now that there is
-- parSpacing stuff.B
layoutGrhs
  :: LGRHS GhcPs (LHsExpr GhcPs)
  -> ToBriDocM ([BriDocNumbered], BriDocNumbered, LHsExpr GhcPs)
layoutGrhs lgrhs@(L _ (GRHS _ guards body)) = do
  guardDocs <- sequence (map (layoutStmt . toL) guards)
  bodyDoc <- docWrapNode (toL lgrhs) $ layoutExpr (toL body)
  return (guardDocs, bodyDoc, body)

layoutPatternBind
  :: Maybe Text
  -> BriDocNumbered
  -> LMatch GhcPs (LHsExpr GhcPs)
  -> ToBriDocM BriDocNumbered
layoutPatternBind funId binderDoc lmatch@(L _ match) = do
  let pats = unLoc (m_pats match)
  let (GRHSs _ grhssNE whereBinds) = m_grhss match
      grhss = NonEmpty.toList grhssNE
  patDocs <- pats `forM` \p -> fmap return $ colsWrapPat =<< layoutPat p
  let isInfix = isInfixMatch match
  mIdStr <- case match of
    Match _ (FunRhs matchId _ _ _) _ _ -> Just . applyNameAdornment matchId <$> lrdrNameToTextAnn (toL matchId)
    _ -> pure Nothing
  let mIdStr' = fixPatternBindIdentifier match <$> mIdStr
  patDoc <- docWrapNodePrior (toL lmatch) $ case (mIdStr', patDocs) of
    (Just idStr, p1 : p2 : pr) | isInfix -> if null pr
      then docCols
        ColPatternsFuncInfix
        [ appSep $ docForceSingleline p1
        , appSep $ docLit $ idStr
        , docForceSingleline p2
        ]
      else docCols
        ColPatternsFuncInfix
        ([ docCols
             ColPatterns
             [ docParenL
             , appSep $ docForceSingleline p1
             , appSep $ docLit $ idStr
             , docForceSingleline p2
             , appSep $ docParenR
             ]
         ]
        ++ (spacifyDocs $ docForceSingleline <$> pr)
        )
    (Just idStr, []) -> docLit idStr
    (Just idStr, ps) ->
      docCols ColPatternsFuncPrefix
        $ appSep (docLit $ idStr)
        : (spacifyDocs $ docForceSingleline <$> ps)
    (Nothing, ps) ->
      docCols ColPatterns
        $ (List.intersperse docSeparator $ docForceSingleline <$> ps)
  clauseDocs <- docWrapNodeRest (toL lmatch) $ layoutGrhs `mapM` grhss
  mWhereDocs <- layoutLocalBinds (L noSrcSpan whereBinds)
  let mWhereArg = mWhereDocs <&> (,) (mkAnnKey (toL lmatch))
  let alignmentToken = if null pats then Nothing else funId
  hasComments <- case mWhereDocs of
    Nothing -> hasAnyRegularCommentsConnectedNoFollowing (toL lmatch)
    Just _  -> hasAnyCommentsBelow (toL lmatch)
  layoutPatternBindFinal
    alignmentToken
    binderDoc
    (Just patDoc)
    clauseDocs
    mWhereArg
    hasComments

fixPatternBindIdentifier :: Match GhcPs (LHsExpr GhcPs) -> Text -> Text
fixPatternBindIdentifier match idStr = go $ m_ctxt match
 where
  go = \case
    (FunRhs _ _ SrcLazy _) -> Text.cons '~' idStr
    (FunRhs _ _ SrcStrict _) -> Text.cons '!' idStr
    (FunRhs _ _ NoSrcStrict _) -> idStr
    (StmtCtxt ctx1) -> goInner ctx1
    _ -> idStr
  -- I have really no idea if this path ever occurs, but better safe than
  -- risking another "drop bangpatterns" bugs.
  goInner = \case
    (PatGuard ctx1) -> go ctx1
    (ParStmtCtxt ctx1) -> goInner ctx1
    (TransStmtCtxt ctx1) -> goInner ctx1
    _ -> idStr

layoutPatternBindFinal
  :: Maybe Text
  -> BriDocNumbered
  -> Maybe BriDocNumbered
  -> [([BriDocNumbered], BriDocNumbered, LHsExpr GhcPs)]
  -> Maybe (AnnKey, [BriDocNumbered])
     -- ^ AnnKey for the node that contains the AnnWhere position annotation
  -> Bool
  -> ToBriDocM BriDocNumbered
layoutPatternBindFinal alignmentToken binderDoc mPatDoc clauseDocs mWhereDocs hasComments
  = do
    let
      patPartInline = case mPatDoc of
        Nothing -> []
        Just patDoc -> [appSep $ docForceSingleline $ return patDoc]
      patPartParWrap = case mPatDoc of
        Nothing -> id
        Just patDoc -> docPar (return patDoc)
    whereIndent <- do
      shouldSpecial <-
        mAsk <&> _conf_layout .> _lconfig_indentWhereSpecial .> confUnpack
      regularIndentAmount <-
        mAsk <&> _conf_layout .> _lconfig_indentAmount .> confUnpack
      pure $ if shouldSpecial
        then BrIndentSpecial (max 1 (regularIndentAmount `div` 2))
        else BrIndentRegular
    -- TODO: apart from this, there probably are more nodes below which could
    --       be shared between alternatives.
    wherePartMultiLine :: [ToBriDocM BriDocNumbered] <- case mWhereDocs of
      Nothing -> return $ []
      Just (annKeyWhere, [w]) -> pure . pure <$> docAlt
        [ docEnsureIndent BrIndentRegular
          $ docSeq
              [ docLit $ Text.pack "where"
              , docSeparator
              , docForceSingleline $ return w
              ]
        , docMoveToKWDP annKeyWhere AnnWhere False
        $ docEnsureIndent whereIndent
        $ docLines
            [ docLit $ Text.pack "where"
            , docEnsureIndent whereIndent
            $ docSetIndentLevel
            $ docNonBottomSpacing
            $ return w
            ]
        ]
      Just (annKeyWhere, ws) ->
        fmap (pure . pure)
          $ docMoveToKWDP annKeyWhere AnnWhere False
          $ docEnsureIndent whereIndent
          $ docLines
              [ docLit $ Text.pack "where"
              , docEnsureIndent whereIndent
              $ docSetIndentLevel
              $ docNonBottomSpacing
              $ docLines
              $ return
              <$> ws
              ]
    let
      singleLineGuardsDoc guards = appSep $ case guards of
        [] -> docEmpty
        [g] -> docSeq
          [appSep $ docLit $ Text.pack "|", docForceSingleline $ return g]
        gs ->
          docSeq
            $ [appSep $ docLit $ Text.pack "|"]
            ++ (List.intersperse
                 docCommaSep
                 (docForceSingleline . return <$> gs)
               )
      wherePart = case mWhereDocs of
        Nothing -> Just docEmpty
        Just (_, [w]) -> Just $ docSeq
          [ docSeparator
          , appSep $ docLit $ Text.pack "where"
          , docSetIndentLevel $ docForceSingleline $ return w
          ]
        _ -> Nothing

    indentPolicy <- mAsk <&> _conf_layout .> _lconfig_indentPolicy .> confUnpack

    runFilteredAlternative $ do

      case clauseDocs of
        [(guards, body, _bodyRaw)] -> do
          let guardPart = singleLineGuardsDoc guards
          forM_ wherePart $ \wherePart' ->
            -- one-line solution
            addAlternativeCond (not hasComments) $ docCols
              (ColBindingLine alignmentToken)
              [ docSeq (patPartInline ++ [guardPart])
              , docSeq
                [ appSep $ return binderDoc
                , docForceSingleline $ return body
                , wherePart'
                ]
              ]
          -- one-line solution + where in next line(s)
          addAlternativeCond (Data.Maybe.isJust mWhereDocs)
            $ docLines
            $ [ docCols
                  (ColBindingLine alignmentToken)
                  [ docSeq (patPartInline ++ [guardPart])
                  , docSeq
                    [ appSep $ return binderDoc
                    , docForceParSpacing $ docAddBaseY BrIndentRegular $ return
                      body
                    ]
                  ]
              ]
            ++ wherePartMultiLine
          -- two-line solution + where in next line(s)
          addAlternative
            $ docLines
            $ [ docForceSingleline
                $ docSeq (patPartInline ++ [guardPart, return binderDoc])
              , docEnsureIndent BrIndentRegular $ docForceSingleline $ return
                body
              ]
            ++ wherePartMultiLine
          -- pattern and exactly one clause in single line, body as par;
          -- where in following lines
          addAlternative
            $ docLines
            $ [ docCols
                  (ColBindingLine alignmentToken)
                  [ docSeq (patPartInline ++ [guardPart])
                  , docSeq
                    [ appSep $ return binderDoc
                    , docForceParSpacing $ docAddBaseY BrIndentRegular $ return
                      body
                    ]
                  ]
              ]
             -- , lineMod $ docAlt
             --   [ docSetBaseY $ return body
             --   , docAddBaseY BrIndentRegular $ return body
             --   ]
            ++ wherePartMultiLine
          -- pattern and exactly one clause in single line, body in new line.
          addAlternative
            $ docLines
            $ [ docSeq (patPartInline ++ [guardPart, return binderDoc])
              , docNonBottomSpacing
              $ docEnsureIndent BrIndentRegular
              $ docAddBaseY BrIndentRegular
              $ return body
              ]
            ++ wherePartMultiLine

        _ -> return () -- no alternatives exclusively when `length clauseDocs /= 1`

      case mPatDoc of
        Nothing -> return ()
        Just patDoc ->
          -- multiple clauses added in-paragraph, each in a single line
          -- example: foo | bar = baz
          --              | lll = asd
          addAlternativeCond (indentPolicy == IndentPolicyFree)
            $ docLines
            $ [ docSeq
                  [ appSep $ docForceSingleline $ return patDoc
                  , docSetBaseY
                  $ docLines
                  $ clauseDocs
                  <&> \(guardDocs, bodyDoc, _) -> do
                        let guardPart = singleLineGuardsDoc guardDocs
                        -- the docForceSingleline might seems superflous, but it
                        -- helps the alternative resolving impl.
                        docForceSingleline $ docCols
                          ColGuardedBody
                          [ guardPart
                          , docSeq
                            [ appSep $ return binderDoc
                            , docForceSingleline $ return bodyDoc
                            -- i am not sure if there is a benefit to using
                            -- docForceParSpacing additionally here:
                            -- , docAddBaseY BrIndentRegular $ return bodyDoc
                            ]
                          ]
                  ]
              ]
            ++ wherePartMultiLine
      -- multiple clauses, each in a separate, single line
      addAlternative
        $ docLines
        $ [ docAddBaseY BrIndentRegular
            $ patPartParWrap
            $ docLines
            $ map docSetBaseY
            $ clauseDocs
            <&> \(guardDocs, bodyDoc, _) -> do
                  let guardPart = singleLineGuardsDoc guardDocs
                  -- the docForceSingleline might seems superflous, but it
                  -- helps the alternative resolving impl.
                  docForceSingleline $ docCols
                    ColGuardedBody
                    [ guardPart
                    , docSeq
                      [ appSep $ return binderDoc
                      , docForceSingleline $ return bodyDoc
                      -- i am not sure if there is a benefit to using
                      -- docForceParSpacing additionally here:
                      -- , docAddBaseY BrIndentRegular $ return bodyDoc
                      ]
                    ]
          ]
        ++ wherePartMultiLine
      -- multiple clauses, each with the guard(s) in a single line, body
      -- as a paragraph
      addAlternative
        $ docLines
        $ [ docAddBaseY BrIndentRegular
            $ patPartParWrap
            $ docLines
            $ map docSetBaseY
            $ clauseDocs
            <&> \(guardDocs, bodyDoc, _) ->
                  docSeq
                    $ (case guardDocs of
                        [] -> []
                        [g] ->
                          [ docForceSingleline $ docSeq
                              [appSep $ docLit $ Text.pack "|", return g]
                          ]
                        gs ->
                          [ docForceSingleline
                              $ docSeq
                              $ [appSep $ docLit $ Text.pack "|"]
                              ++ List.intersperse docCommaSep (return <$> gs)
                          ]
                      )
                    ++ [ docSeparator
                       , docCols
                         ColOpPrefix
                         [ appSep $ return binderDoc
                         , docAddBaseY BrIndentRegular
                         $ docForceParSpacing
                         $ return bodyDoc
                         ]
                       ]
          ]
        ++ wherePartMultiLine
      -- multiple clauses, each with the guard(s) in a single line, body
      -- in a new line as a paragraph
      addAlternative
        $ docLines
        $ [ docAddBaseY BrIndentRegular
            $ patPartParWrap
            $ docLines
            $ map docSetBaseY
            $ clauseDocs
            >>= \(guardDocs, bodyDoc, _) ->
                  (case guardDocs of
                      [] -> []
                      [g] ->
                        [ docForceSingleline
                            $ docSeq [appSep $ docLit $ Text.pack "|", return g]
                        ]
                      gs ->
                        [ docForceSingleline
                            $ docSeq
                            $ [appSep $ docLit $ Text.pack "|"]
                            ++ List.intersperse docCommaSep (return <$> gs)
                        ]
                    )
                    ++ [ docCols
                           ColOpPrefix
                           [ appSep $ return binderDoc
                           , docAddBaseY BrIndentRegular
                           $ docForceParSpacing
                           $ return bodyDoc
                           ]
                       ]
          ]
        ++ wherePartMultiLine
      -- conservative approach: everything starts on the left.
      addAlternative
        $ docLines
        $ [ docAddBaseY BrIndentRegular
            $ patPartParWrap
            $ docLines
            $ map docSetBaseY
            $ clauseDocs
            >>= \(guardDocs, bodyDoc, _) ->
                  (case guardDocs of
                      [] -> []
                      [g] -> [docSeq [appSep $ docLit $ Text.pack "|", return g]]
                      (g1 : gr) ->
                        (docSeq [appSep $ docLit $ Text.pack "|", return g1]
                        : (gr <&> \g ->
                            docSeq [appSep $ docLit $ Text.pack ",", return g]
                          )
                        )
                    )
                    ++ [ docCols
                           ColOpPrefix
                           [ appSep $ return binderDoc
                           , docAddBaseY BrIndentRegular $ return bodyDoc
                           ]
                       ]
          ]
        ++ wherePartMultiLine

-- | Layout a pattern synonym binding
layoutPatSynBind
  :: Located (IdP GhcPs)
  -> HsPatSynDetails GhcPs
  -> HsPatSynDir GhcPs
  -> LPat GhcPs
  -> ToBriDocM BriDocNumbered
layoutPatSynBind name patSynDetails patDir rpat = do
  let
    patDoc = docLit $ Text.pack "pattern"
    binderDoc = case patDir of
      ImplicitBidirectional -> docLit $ Text.pack "="
      _ -> docLit $ Text.pack "<-"
    body = colsWrapPat =<< layoutPat rpat
    whereDoc = docLit $ Text.pack "where"
  mWhereDocs <- layoutPatSynWhere patDir
  headDoc <-
    fmap pure
    $ docSeq
    $ [ patDoc
      , docSeparator
      , layoutLPatSyn name patSynDetails
      , docSeparator
      , binderDoc
      ]
  runFilteredAlternative $ do
    addAlternative
      $
      -- pattern .. where
      --   ..
      --   ..
        docAddBaseY BrIndentRegular
      $ docSeq
          ([headDoc, docSeparator, body] ++ case mWhereDocs of
            Just ds -> [docSeparator, docPar whereDoc (docLines ds)]
            Nothing -> []
          )
    addAlternative
      $
      -- pattern .. =
      --   ..
      -- pattern .. <-
      --   .. where
      --   ..
      --   ..
        docAddBaseY BrIndentRegular
      $ docPar
          headDoc
          (case mWhereDocs of
            Nothing -> body
            Just ds -> docLines ([docSeq [body, docSeparator, whereDoc]] ++ ds)
          )

-- | Helper method for the left hand side of a pattern synonym
layoutLPatSyn
  :: Located (IdP GhcPs)
  -> HsPatSynDetails GhcPs
  -> ToBriDocM BriDocNumbered
layoutLPatSyn name (PrefixCon vars) = do
  docName <- lrdrNameToTextAnn name
  names <- mapM (lrdrNameToTextAnn . toL) vars
  docSeq . fmap appSep $ docLit docName : (docLit <$> names)
layoutLPatSyn name (InfixCon left right) = do
  leftDoc <- lrdrNameToTextAnn (toL left)
  nameText <- lrdrNameToTextAnn name
  -- GHC 9.14: annsDP lacks AnnBackquote, so add backticks for non-symbol
  -- names in infix position.
  let isSym = isSymOcc (rdrNameOcc (unLoc name))
      docName = if isSym then nameText else Text.pack "`" <> nameText <> Text.pack "`"
  rightDoc <- lrdrNameToTextAnn (toL right)
  docSeq . fmap (appSep . docLit) $ [leftDoc, docName, rightDoc]
layoutLPatSyn name (RecCon recArgs) = do
  docName <- lrdrNameToTextAnn name
  args <- mapM (\r -> case recordPatSynField r of FieldOcc _ lname -> lrdrNameToTextAnn (toL lname)) recArgs
  docSeq
    . fmap docLit
    $ [docName, Text.pack " { "]
    <> intersperse (Text.pack ", ") args
    <> [Text.pack " }"]

-- | Helper method to get the where clause from of explicitly bidirectional
-- pattern synonyms
layoutPatSynWhere
  :: HsPatSynDir GhcPs -> ToBriDocM (Maybe [ToBriDocM BriDocNumbered])
layoutPatSynWhere hs = case hs of
  ExplicitBidirectional (MG _ (L _ lbinds)) -> do
    binderDoc <- docLit $ Text.pack "="
    Just
      <$> mapM (docSharedWrapper $ layoutPatternBind Nothing binderDoc) lbinds
  _ -> pure Nothing

--------------------------------------------------------------------------------
-- TyClDecl
--------------------------------------------------------------------------------

layoutTyCl :: ToBriDoc TyClDecl
layoutTyCl ltycl@(L _loc tycl) = case tycl of
  SynDecl annSynDecl name vars fixityElem typ -> do
    let
      isInfix = (fixityElem == Infix)
      hasSourceParens = not (null (asd_opens annSynDecl))
    let wrapNodeRest = docWrapNodeRest ltycl
    docWrapNodePrior ltycl
      $ layoutSynDecl isInfix hasSourceParens wrapNodeRest (toL name) (hsq_explicit vars) typ
  DataDecl _ext name tyVars _ dataDefn ->
    layoutDataDecl (toL ltycl) (toL name) tyVars dataDefn
  _ -> briDocByExactNoComment TypeClassDeclarationFallback ltycl

layoutSynDecl
  :: forall flag. Data.Data.Data flag =>
  Bool
  -> Bool   -- hasSourceParens: source had explicit parentheses around LHS
  -> (ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered)
  -> Located (IdP GhcPs)
  -> [LHsTyVarBndr flag GhcPs]
  -> LHsType GhcPs
  -> ToBriDocM BriDocNumbered
layoutSynDecl isInfix hasSourceParens wrapNodeRest name vars typ = do
  nameStr0 <- lrdrNameToTextAnn (toL name)
  let
    -- GHC 9.14: annotations lack AnnOpenP/AnnBackquote, so lrdrNameToTextAnn
    -- returns the raw name. Parenthesize operators in prefix position; add
    -- backticks for alphanumeric names in infix position.
    isSym = isSymOcc (rdrNameOcc (unLoc name))
    nameStr
      | isInfix && not isSym = Text.pack "`" <> nameStr0 <> Text.pack "`"
      | not isInfix && isSym = Text.pack "(" <> nameStr0 <> Text.pack ")"
      | otherwise = nameStr0
    lhs = appSep . wrapNodeRest $ if isInfix
      then do
        let (a : b : rest) = vars
        hasOwnParens <- hasAnnKeywordComment (toL a) AnnOpenP
        -- Parenthesize when there are extra vars, when old-style annotation
        -- says so, or when the source had explicit parens (asd_opens).
        let needsParens = not (null rest) || hasOwnParens || hasSourceParens
        docSeq
          $ [docLit $ Text.pack "type", docSeparator]
          ++ [ docParenL | needsParens ]
          ++ [ layoutTyVarBndr False (toL a)
             , docSeparator
             , docLit nameStr
             , docSeparator
             , layoutTyVarBndr False (toL b)
             ]
          ++ [ docParenR | needsParens ]
          ++ fmap (layoutTyVarBndr True . toL) rest
      else
        docSeq
        $ [ docLit $ Text.pack "type"
          , docSeparator
          , docWrapNode (toL name) $ docLit nameStr
          ]
        ++ fmap (layoutTyVarBndr True . toL) vars
  sharedLhs <- docSharedWrapper id lhs
  typeDoc <- docSharedWrapper layoutType (toL typ)
  hasComments <- hasAnyCommentsConnected (toL typ)
  layoutLhsAndType hasComments sharedLhs "=" typeDoc

layoutTyVarBndr :: forall flag. Data.Data.Data flag => Bool -> ToBriDoc (HsTyVarBndr flag)
layoutTyVarBndr needsSep lbndr@(L _ bndr) = do
  docWrapNodePrior lbndr $ case bndr of
    HsTvb _ _ (HsBndrVar _ name) (HsBndrNoKind _) -> do
      nameStr <- lrdrNameToTextAnn (toL name)
      docSeq $ [ docSeparator | needsSep ] ++ [docLit nameStr]
    HsTvb _ _ (HsBndrVar _ name) (HsBndrKind _ kind) -> do
      nameStr <- lrdrNameToTextAnn (toL name)
      docSeq
        $ [ docSeparator | needsSep ]
        ++ [ docLit $ Text.pack "("
           , appSep $ docLit nameStr
           , appSep . docLit $ Text.pack "::"
           , docForceSingleline $ layoutType (toL kind)
           , docLit $ Text.pack ")"
           ]
    HsTvb _ _ (HsBndrWildCard _) (HsBndrNoKind _) ->
      docSeq $ [docSeparator | needsSep] ++ [docLit $ Text.pack "_"]
    HsTvb _ _ (HsBndrWildCard _) (HsBndrKind _ kind) ->
      docSeq
        $ [docSeparator | needsSep]
        ++ [ docLit $ Text.pack "(_ ::"
           , docSeparator
           , docForceSingleline $ layoutType (toL kind)
           , docLit $ Text.pack ")"
           ]


--------------------------------------------------------------------------------
-- TyFamInstDecl
--------------------------------------------------------------------------------



layoutTyFamInstDecl
  :: Data.Data.Data a
  => Bool
  -> Located a
  -> TyFamInstDecl GhcPs
  -> ToBriDocM BriDocNumbered
layoutTyFamInstDecl inClass outerNode tfid = do
  let
    eqn = tfid_eqn tfid
    name = feqn_tycon eqn
    bndrsMay = case feqn_bndrs eqn of
      HsOuterExplicit _ bndrs -> Just bndrs
      _ -> Nothing
    pats = feqn_pats eqn
    typ = feqn_rhs eqn
  docWrapNodePrior outerNode $ do
    nameStr <- lrdrNameToTextAnn (toL name)
    needsParens <- hasAnnKeyword outerNode AnnOpenP
    let
      instanceDoc = if inClass
        then docLit $ Text.pack "type"
        else docSeq
          [appSep . docLit $ Text.pack "type", docLit $ Text.pack "instance"]
      makeForallDoc :: forall flag. [LHsTyVarBndr flag GhcPs] -> ToBriDocM BriDocNumbered
      makeForallDoc bndrs = do
        bndrDocs <- layoutTyVarBndrs bndrs
        docSeq
          ([docLit (Text.pack "forall")] ++ processTyVarBndrsSingleline bndrDocs
          )
      lhs =
        docSeq
          $ [appSep instanceDoc]
          ++ [ makeForallDoc foralls | Just foralls <- [bndrsMay] ]
          ++ [ docParenL | needsParens ]
          ++ [appSep $ docWrapNode (toL name) $ docLit nameStr]
          ++ intersperse docSeparator (layoutHsTyPats pats)
          ++ [ docParenR | needsParens ]
    hasComments <- hasAnyRegularCommentsConnectedNoFollowing outerNode
    typeDoc <- docSharedWrapper layoutType (toL typ)
    -- If the decl spans multiple lines and has following comments, force multi-line
    -- to avoid single-line layout putting comment before "=" on next line
    followComments <- astFollowingComments outerNode
    let hasFollowingComments = not (null followComments)
        declSpansMultipleLines = case getLoc outerNode of
          RealSrcSpan s _ -> srcSpanStartLine s /= srcSpanEndLine s
          _ -> False
        hasComments' = hasComments || (hasFollowingComments && declSpansMultipleLines)
    layoutLhsAndType hasComments' lhs "=" typeDoc


layoutHsTyPats :: HsFamEqnPats GhcPs -> [ToBriDocM BriDocNumbered]
layoutHsTyPats pats = pats <&> \case
  HsValArg _ tm -> layoutType (toL tm)
  HsTypeArg _ ty -> docSeq [docLit $ Text.pack "@", layoutType (toL ty)]
    -- we ignore the SourceLoc here.. this LPat not being (L _ Pat{}) change
    -- is a bit strange. Hopefully this does not ignore any important
    -- annotations.
  HsArgPar _ -> error "brittany internal error: HsArgPar{}"
  XArg _ -> error "brittany internal error: XArg{}"

--------------------------------------------------------------------------------
-- ClsInstDecl
--------------------------------------------------------------------------------

-- | Layout an @instance@ declaration
--
--   Layout signatures and bindings using the corresponding layouters from the
--   top-level. Layout the instance head, type family instances, and data family
--   instances using ExactPrint.
layoutClsInst :: ToBriDoc ClsInstDecl
layoutClsInst lcid@(L _ cid) = docLines
  [ layoutInstanceHead
  , docEnsureIndent BrIndentRegular
  $ docSetIndentLevel
  $ docSortedLines
  $ fmap (layoutAndLocateSig . toL) (cid_sigs cid)
  ++ fmap (layoutAndLocateBind . toL) (cid_binds cid)
  ++ fmap (layoutAndLocateTyFamInsts . toL) (cid_tyfam_insts cid)
  ++ fmap (layoutAndLocateDataFamInsts . toL) (cid_datafam_insts cid)
  ]
 where
  layoutInstanceHead :: ToBriDocM BriDocNumbered
  layoutInstanceHead =
    briDocByExactNoComment TypeClassDeclarationFallback
      $ InstD NoExtField
      . ClsInstD NoExtField
      . removeChildren
      <$> lcid

  removeChildren :: ClsInstDecl GhcPs -> ClsInstDecl GhcPs
  removeChildren c = c
    { cid_binds = []
    , cid_sigs = []
    , cid_tyfam_insts = []
    , cid_datafam_insts = []
    }

  -- | Like 'docLines', but sorts the lines based on location
  docSortedLines
    :: [ToBriDocM (Located BriDocNumbered)] -> ToBriDocM BriDocNumbered
  docSortedLines l =
    allocateNode
      . BDFLines
      . fmap unLoc
      . List.sortOn (ExactPrint.rs . getLoc)
      =<< sequence l

  layoutAndLocateSig :: ToBriDocC (Sig GhcPs) (Located BriDocNumbered)
  layoutAndLocateSig lsig@(L loc _) = L loc <$> layoutSig lsig

  layoutAndLocateBind :: ToBriDocC (HsBind GhcPs) (Located BriDocNumbered)
  layoutAndLocateBind lbind@(L loc _) =
    L loc <$> (joinBinds =<< layoutBind lbind)

  joinBinds
    :: Either [BriDocNumbered] BriDocNumbered -> ToBriDocM BriDocNumbered
  joinBinds = \case
    Left ns -> docLines $ return <$> ns
    Right n -> return n

  layoutAndLocateTyFamInsts
    :: ToBriDocC (TyFamInstDecl GhcPs) (Located BriDocNumbered)
  layoutAndLocateTyFamInsts ltfid@(L loc tfid) =
    L loc <$> docWrapNode ltfid (layoutTyFamInstDecl True ltfid tfid)

  layoutAndLocateDataFamInsts
    :: ToBriDocC (DataFamInstDecl GhcPs) (Located BriDocNumbered)
  layoutAndLocateDataFamInsts ldfid@(L loc _) =
    L loc <$> docWrapNode ldfid (layoutDataFamInstDecl ldfid)

  -- | Send to ExactPrint then remove unecessary whitespace
  layoutDataFamInstDecl :: ToBriDoc DataFamInstDecl
  layoutDataFamInstDecl ldfid =
    fmap stripWhitespace
      <$> briDocByExactNoComment FamilyDefaultFallback ldfid

  -- | ExactPrint adds indentation/newlines to @data@/@type@ declarations
  stripWhitespace :: BriDocF f -> BriDocF f
  stripWhitespace (BDFExternal ann annotationKeys b t) =
    BDFExternal ann annotationKeys b $ stripWhitespace' t
  stripWhitespace b = b

  -- | This fixes two issues of output coming from Exactprinting
  --   associated (data) type decls. Firstly we place the output into docLines,
  --   so one newline coming from Exactprint is superfluous, so we drop the
  --   first (empty) line. The second issue is Exactprint indents the first
  --   member in a strange fashion:
  --
  --   input:
  --
  --   > instance MyClass Int where
  --   >   -- | This data is very important
  --   >   data MyData = IntData
  --   >     { intData  :: String
  --   >     , intData2 :: Int
  --   >     }
  --
  --   output of just exactprinting the associated data type syntax node
  --
  --   >
  --   >   -- | This data is very important
  --   >   data MyData = IntData
  --   >   { intData  :: String
  --   >   , intData2 :: Int
  --   >   }
  --
  --   To fix this, we strip whitespace from the start of the comments and the
  --   first line of the declaration, stopping when we see "data" or "type" at
  --   the start of a line. I.e., this function yields
  --
  --   > -- | This data is very important
  --   > data MyData = IntData
  --   >   { intData  :: String
  --   >   , intData2 :: Int
  --   >   }
  --
  --   Downside apart from being a hacky and brittle fix is that this removes
  --   possible additional indentation from comments before the first member.
  --
  --   But the whole thing is just a temporary measure until brittany learns
  --   to layout data/type decls.
  stripWhitespace' :: Text -> Text
  stripWhitespace' t =
    Text.intercalate (Text.pack "\n") $ go $ dropLeadingEmpty $ Text.lines t
   where
    -- GHC 9.14: docExt already strips leading newlines, so the first line
    -- may be the actual data/type keyword, not a blank.  Only drop if empty.
    dropLeadingEmpty [] = []
    dropLeadingEmpty (l : ls)
      | Text.null (Text.stripStart l) = ls
      | otherwise = l : ls
    go [] = []
    go (line1 : lineR) = case Text.stripStart line1 of
      st
        | isTypeOrData st ->
          let keyIndent = Text.length line1 - Text.length st
          in st : map (stripNSpaces keyIndent) lineR
        | otherwise -> st : go lineR
    stripNSpaces n textValue
      | n <= 0 = textValue
      | otherwise = case Text.uncons textValue of
          Just (' ', rest) -> stripNSpaces (n - 1) rest
          _ -> textValue
    isTypeOrData t' =
      (Text.pack "type" `Text.isPrefixOf` t')
        || (Text.pack "newtype" `Text.isPrefixOf` t')
        || (Text.pack "data" `Text.isPrefixOf` t')


--------------------------------------------------------------------------------
-- Common Helpers
--------------------------------------------------------------------------------

layoutLhsAndType
  :: Bool
  -> ToBriDocM BriDocNumbered
  -> String
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
layoutLhsAndType hasComments lhs sep typeDoc = do
  runFilteredAlternative $ do
    -- (separators probably are "=" or "::")
    -- lhs = type
    -- lhs :: type
    addAlternativeCond (not hasComments) $ docSeq
      [lhs, docSeparator, docLitS sep, docSeparator, docForceSingleline typeDoc]
    -- lhs
    --   :: typeA
    --   -> typeB
    -- lhs
    --   =  typeA
    --   -> typeB
    addAlternative $ docAddBaseY BrIndentRegular $ docPar lhs $ docCols
      ColTyOpPrefix
      [ appSep $ docLitS sep
      , docAddBaseY (BrIndentSpecial (length sep + 1)) typeDoc
      ]
