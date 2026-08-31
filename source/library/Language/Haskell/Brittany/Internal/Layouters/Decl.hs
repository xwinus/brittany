{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Brittany.Internal.Layouters.Decl where

import qualified Data.Data
import qualified Data.Foldable
import qualified Data.Maybe
import qualified Data.Map as Map
import qualified Data.Semigroup as Semigroup
import qualified Data.Set as Set
import qualified Data.Text as Text
import GHC (GenLocated(L))
import qualified GHC.Data.FastString as FastString
import GHC.Hs
import GHC.Hs.Decls (AnnSynDecl(..), FamEqn(..), TyFamInstDecl(..))
import GHC.Hs.Type (FieldOcc(..), HsBndrKind(..), HsBndrVar(..), HsSigType(HsSig), HsTyVarBndr(..), HsOuterTyVarBndrs(HsOuterExplicit, HsOuterImplicit), HsWildCardBndrs(HsWC))
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
import Language.Haskell.Syntax.Binds (RecordPatSynField(recordPatSynField))
import Language.Haskell.Syntax.BooleanFormula
  ( BooleanFormula(..)
  , LBooleanFormula
  )
import GHC.Parser.Annotation
  ( EpAnn(..)
  , EpaLocation(..)
  , getLocA
  )
import GHC.Types.SrcLoc (Located, RealSrcSpan, SrcSpan(..), getLoc, noSrcSpan, srcSpanStartCol, srcSpanStartLine, srcSpanEndCol, srcSpanEndLine, unLoc)
import Language.Haskell.Brittany.Internal.Alignment
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.CommentPlan
  ( lookupCommentRole )
import Language.Haskell.Brittany.Internal.ExactPrintCompat (AnnKeywordId(..), AnnKey(..), mkAnnKey, Comment(..), realSpanToSrcSpan, srcSpanToRealSpan)
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as ExactPrintCompat
import Language.Haskell.Brittany.Internal.ExactPrintUtils
import Language.Haskell.Brittany.Internal.ExactSource (sourceCommentFragment)
import Language.Haskell.Brittany.Internal.Fallbacks (FallbackId(..))
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.DataDecl
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Expr
import Language.Haskell.Brittany.Internal.Layouters.FixitySignature
import Language.Haskell.Brittany.Internal.Layouters.Pattern
import Language.Haskell.Brittany.Internal.Layouters.StandaloneKindSignature
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Stmt
import Language.Haskell.Brittany.Internal.Layouters.IE (toL)
import Language.Haskell.Brittany.Internal.Layouters.Instance
import Language.Haskell.Brittany.Internal.Layouters.StandaloneDeriving
import Language.Haskell.Brittany.Internal.Layouters.Type
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.TypeFallbacks
  ( requiresExactTypeDeclaration )
import Language.Haskell.Brittany.Internal.Types
import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Utils as ExactPrint



layoutDecl :: ToBriDoc HsDecl
layoutDecl = layoutDeclWithExactText Nothing False []

layoutDeclWithExactText
  :: Maybe ExactSourceFragment
  -> Bool
  -> [SourceComment]
  -> ToBriDoc HsDecl
layoutDeclWithExactText exactText hasSourceComments sourceComments
    d@(L loc decl) = case decl of
  SigD _ sig@TypeSig{}
    -> layoutTypeSignature d sig
  SigD _ sig
    | requiresExactSignature sig -> layoutExact SignatureFallback d exactText
  SigD _ sig -> withTransformedAnns d $ docWrapNode d $ layoutSig (L loc sig)
  KindSigD _ signature -> withTransformedAnns d
    $ docWrapNode d
    $ layoutStandaloneKindSignature (L loc signature)
  ValD _ bind -> layoutValueDeclaration d bind
  TyClD _ tycl
    | requiresExactTypeDeclaration tycl ->
        layoutExact TypeClassDeclarationFallback d exactText
    | SynDecl{} <- tycl -> layoutNativeDeclaration d $ layoutTyCl (L loc tycl)
    | ClassDecl{} <- tycl
    , supportsNativeClass tycl ->
        layoutExactUnlessComposable TypeClassDeclarationFallback d
          isComposableClassComment
          $ layoutTyCl (L loc tycl)
    | FamDecl{} <- tycl
    , supportsNativeFamily tycl ->
        layoutExactUnlessComposable TypeClassDeclarationFallback d
          (isPriorDeclarationComment d)
          $ layoutTyCl (L loc tycl)
    | DataDecl{} <- tycl -> layoutDataDeclaration d tycl
    | otherwise -> layoutExactWhenCommented d $ layoutTyCl (L loc tycl)
  InstD _ (TyFamInstD _ tfid) ->
    layoutExactUnlessComposable TypeClassDeclarationFallback d
      (isPriorDeclarationComment d)
      $ layoutTyFamInstDecl False d tfid
  InstD _ (ClsInstD _ inst) ->
    layoutClassInstance d inst $ do
      followComments <- astFollowingComments d
      docSeq
        [ layoutClsInst (L loc inst)
        , docSeq [docLitS (" " ++ commentContents c) | (c, _) <- followComments]
        ]
  InstD _ (DataFamInstD _ _) ->
    layoutExactOrCommented d $ withTransformedAnns d $ do
      followComments <- astFollowingComments d
      docSeq $ [briDocByExactNoComment DeclarationFallback d]
        ++ [docLitS (" " ++ commentContents c) | (c, _) <- followComments]
  DerivD _ deriv -> case layoutStandaloneDeriving deriv of
    Nothing -> layoutExact DeclarationFallback d exactText
    Just formatted -> layoutExactUnlessComposable TypeClassDeclarationFallback d
      (isPriorDeclarationComment d)
      formatted
  ForD{} -> layoutExact DeclarationFallback d exactText
  WarningD{} -> layoutExact DeclarationFallback d exactText
  AnnD{} -> layoutExact DeclarationFallback d exactText
  RuleD{} -> layoutExact DeclarationFallback d exactText
  SpliceD{} -> layoutExact DeclarationFallback d exactText
  _ -> withTransformedAnns d
    $ docWrapNode d
    $ briDocByExactNoComment DeclarationFallback d
 where
  layoutValueDeclaration
    :: Located (HsDecl GhcPs)
    -> HsBind GhcPs
    -> ToBriDocM BriDocNumbered
  layoutValueDeclaration declaration bind = do
    commentPlan <- mAsk
    grhsComments <- List.concat <$> mapM (astConnectedComments . toL)
      (bindingGrhss bind)
    let declarationCommentKeys = maybe Set.empty fragmentCommentKeys exactText
        fragmentComments = Map.elems $ Map.restrictKeys
          (commentPlanSources commentPlan)
          declarationCommentKeys
        declarationComments = List.nubBy
          (\left right -> sourceCommentKey left == sourceCommentKey right)
          $ sourceComments
          ++ fragmentComments
          ++ Data.Maybe.mapMaybe exactCommentToSourceComment grhsComments
    withTransformedAnns declaration
      $ docWrapNode declaration
      $ layoutBindWithComments declarationComments (L loc bind)
        >>= \case
        Left ns -> docLines $ return <$> ns
        Right n -> return n

  bindingGrhss = \case
    FunBind _ _ (MG _ (L _ matches)) -> List.concatMap matchGrhss matches
    PatBind _ _ _ grhss -> grhssList grhss
    _ -> []

  matchGrhss (L _ (Match _ _ _ grhss)) = grhssList grhss
  matchGrhss (L _ (XMatch _)) = []

  grhssList (GRHSs _ grhssNE _) = NonEmpty.toList grhssNE
  grhssList (XGRHSs _) = []

  exactCommentToSourceComment (sourceComment, _) = do
    commentSpan <- srcSpanToRealSpan $ commentIdentifier sourceComment
    let commentText = Text.pack $ commentContents sourceComment
        syntax = if Text.isPrefixOf (Text.pack "--") commentText
          then LineComment
          else BlockComment
    pure SourceComment
      { sourceCommentKey = SourceCommentKey $ commentIdentifier sourceComment
      , sourceCommentText = commentText
      , sourceCommentSpan = commentSpan
      , sourceCommentSyntax = syntax
      }

  layoutTypeSignature declaration signature = do
    connectedComments <- astConnectedComments declaration
    commentPlan <- mAsk
    let hasPostDocs = any (isSignaturePostDoc commentPlan) connectedComments
        regularSourceComments = filter
          (isRegularSourceComment commentPlan)
          sourceComments
    withTransformedAnns declaration
      $ docWrapNode declaration
      $ layoutSigWithSourceComments
        hasPostDocs regularSourceComments (L loc signature)

  isRegularSourceComment commentPlan sourceComment = case
      Map.lookup (sourceCommentKey sourceComment)
        $ commentPlanPlacements commentPlan of
    Just placement -> case placementRole placement of
      HaddockPostDoc{} -> False
      LeadingDoc -> False
      SectionComment -> False
      PragmaComment -> False
      _ -> True
    Nothing -> True

  isSignaturePostDoc commentPlan (postDocComment, _) =
    lookupCommentRole commentPlan postDocComment `elem`
      [ Just $ HaddockPostDoc SignatureArgument
      , Just $ HaddockPostDoc SignatureResult
      ]

  layoutClassInstance declaration instanceDeclaration formatted = do
    connectedComments <- astConnectedComments declaration
    let hasAssociatedFamilies = not (null $ cid_tyfam_insts instanceDeclaration)
          || not (null $ cid_datafam_insts instanceDeclaration)
        hasRegularComments = any isRegularComment connectedComments
    if hasAssociatedFamilies && hasRegularComments
      then layoutExact TypeClassDeclarationFallback declaration exactText
      else layoutNativeDeclaration declaration formatted

  layoutDataDeclaration declaration dataDeclaration = case dataDeclaration of
    DataDecl{} -> layoutNativeDeclaration declaration
      $ layoutTyCl (L loc dataDeclaration)
    _ -> layoutNativeDeclaration declaration $ layoutTyCl (L loc dataDeclaration)

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

  layoutExactUnlessComposable fallback declaration supportsComment formatted = do
    connectedComments <- astConnectedComments declaration
    commentPlan <- mAsk
    let sourceComments = filter isRegularComment connectedComments
        hasUnownedSourceComments = hasSourceComments && null connectedComments
    if not hasUnownedSourceComments
        && all (supportsComment commentPlan) sourceComments
      then withTransformedAnns declaration
        $ docWrapNode declaration formatted
      else layoutExact fallback declaration exactText

  layoutNativeDeclaration declaration formatted = withTransformedAnns declaration
    $ docWrapNode declaration formatted

  isPriorDeclarationComment declaration commentPlan (sourceComment, _) =
    lookupCommentRole commentPlan sourceComment `elem`
      [Just LeadingDoc, Just LeadingOrdinary, Just SectionComment]
      && isBeforeNode declaration sourceComment

  isBeforeNode node sourceComment = case
    ( srcSpanToRealSpan $ getLoc node
    , srcSpanToRealSpan $ commentIdentifier sourceComment
    ) of
      (Just nodeSpan, Just commentSpan) ->
        (srcSpanEndLine commentSpan, srcSpanEndCol commentSpan)
          <= (srcSpanStartLine nodeSpan, srcSpanStartCol nodeSpan)
      _ -> False

  isComposableClassComment commentPlan (sourceComment, _) =
    lookupCommentRole commentPlan sourceComment `elem`
      [ Just LeadingDoc
      , Just LeadingOrdinary
      , Just SectionComment
      , Just PragmaComment
      , Just $ HaddockPostDoc SignatureArgument
      , Just $ HaddockPostDoc SignatureResult
      ]

  layoutExact fallback declaration source = case source of
    Just fragment -> briDocByExactSourceFragmentNoComment
      fallback declaration fragment
    Nothing -> briDocByExactNoComment fallback declaration

  requiresExactSignature = \case
    SpecSig{} -> True
    SpecSigE{} -> True
    SpecInstSig{} -> True
    _ -> False

--------------------------------------------------------------------------------
-- Sig
--------------------------------------------------------------------------------

layoutSig :: ToBriDoc Sig
layoutSig = layoutSigWithComments False

layoutSigWithComments :: Bool -> ToBriDoc Sig
layoutSigWithComments hasOuterComments =
  layoutSigWithSourceComments hasOuterComments []

layoutSigWithSourceComments :: Bool -> [SourceComment] -> ToBriDoc Sig
layoutSigWithSourceComments hasOuterComments sourceComments
    lsig@(L _loc sig) = case sig of
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
  MinimalSig _ formula -> docWrapNode (toL lsig) $ docSeq
    [ docLitS "{-# MINIMAL "
    , layoutBooleanFormula formula
    , docLitS " #-}"
    ]
  PatSynSig _ names sigTy -> case unLoc sigTy of
    HsSig{} -> layoutNamesAndType (Just "pattern") names sigTy
    _ -> briDocByExactNoComment SignatureFallback (toL lsig)
  FixSig _ fixitySignature ->
    case layoutFixitySignature (toL lsig) fixitySignature of
      Just fixityDoc -> fixityDoc
      Nothing       -> briDocByExactNoComment SignatureFallback (toL lsig)
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
                _ -> docAlt
                  [ let open = docLit $ Text.pack "("
                        close = docLit $ Text.pack ")"
                        list = List.intersperse docCommaSep
                             $ docForceSingleline <$> cntxtDocs
                    in docSeq ([open] ++ list ++ [close])
                  , let open = docCols ColTyOpPrefix
                          [docParenLSep, docAddBaseY (BrIndentSpecial 2) $ head cntxtDocs]
                        close = docLit $ Text.pack ")"
                        list = List.tail cntxtDocs <&> \cntxtDoc ->
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
    hasNestedComments <- hasAnyCommentsBelow (toL lsig)
    let hasComments = hasOuterComments || hasNestedComments
        ( nameComments
          , separatorComments
          , trailingSameLineComments
          , followingComments
          ) =
          signatureBoundaryComments names (toL sigType) sourceComments
        hasBoundaryComments = any (not . null)
          [ nameComments
          , separatorComments
          , trailingSameLineComments
          , followingComments
          ]
        lhsDoc = appSep . docWrapNodeRest (toL lsig) . docSeq
          $ keyDoc <> [docLit nameStr]
    shouldBeHanging <-
      mAsk <&> _conf_layout .> _lconfig_hangingTypeSignature .> confUnpack
    if hasBoundaryComments
      then layoutCommentedSignature
        lhsDoc
        nameComments
        separatorComments
        fullTypeDoc
        trailingSameLineComments
        followingComments
      else if shouldBeHanging
      then
        docSeq
          $ [ lhsDoc
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
        lhsDoc
        "::"
        fullTypeDoc

signatureBoundaryComments
  :: [LocatedN RdrName]
  -> Located ast
  -> [SourceComment]
  -> ( [SourceComment]
     , [SourceComment]
     , [SourceComment]
     , [SourceComment]
     )
signatureBoundaryComments names signatureType sourceComments = case
    (nameEnd, typeRange) of
  (Just namePosition, Just (typeStart, typeEnd)) ->
    ( filter (isNameComment namePosition typeStart) orderedComments
    , filter (isSeparatorComment namePosition typeStart) orderedComments
    , filter (isTrailingSameLineComment typeEnd) orderedComments
    , filter (isFollowingComment typeEnd) orderedComments
    )
  _ -> ([], [], [], [])
 where
  orderedComments = List.sortOn sourceCommentStart sourceComments
  nameEnd = case Data.Maybe.mapMaybe
      (fmap sourceSpanEnd . srcSpanToRealSpan . getLocA)
      names of
    [] -> Nothing
    positions -> Just $ maximum positions
  typeRange = do
    typeSpan <- srcSpanToRealSpan $ getLoc signatureType
    pure (sourceSpanStart typeSpan, sourceSpanEnd typeSpan)
  isNameComment namePosition typeStart sourceComment =
    fst (sourceCommentStart sourceComment) == fst namePosition
      && sourceCommentStart sourceComment >= namePosition
      && sourceCommentEnd sourceComment <= typeStart
  isSeparatorComment namePosition typeStart sourceComment =
    sourceCommentStart sourceComment > namePosition
      && sourceCommentEnd sourceComment <= typeStart
      && not (isNameComment namePosition typeStart sourceComment)
  isTrailingSameLineComment typeEnd sourceComment =
    fst (sourceCommentStart sourceComment) == fst typeEnd
      && sourceCommentStart sourceComment >= typeEnd
  isFollowingComment typeEnd sourceComment =
    fst (sourceCommentStart sourceComment) > fst typeEnd

layoutCommentedSignature
  :: ToBriDocM BriDocNumbered
  -> [SourceComment]
  -> [SourceComment]
  -> ToBriDocM BriDocNumbered
  -> [SourceComment]
  -> [SourceComment]
  -> ToBriDocM BriDocNumbered
layoutCommentedSignature lhs nameComments separatorComments typeDoc
    trailingSameLineComments followingComments =
  docAddBaseY BrIndentRegular $ docPar lhsWithComments $ docLines
    $ [ separatorWithComments
      , docAddBaseY (BrIndentSpecial 3) typeWithComments
      ]
    ++ (layoutPatSynComment <$> followingComments)
 where
  lhsWithComments = appendSourceComments lhs nameComments
  separatorWithComments = appendSourceComments
    (docLitS "::") separatorComments
  typeWithComments = appendSourceComments typeDoc trailingSameLineComments

appendSourceComments
  :: ToBriDocM BriDocNumbered
  -> [SourceComment]
  -> ToBriDocM BriDocNumbered
appendSourceComments base comments = docSeq
  $ base
  : List.concat
    [ [docLitS " ", layoutPatSynComment sourceComment]
    | sourceComment <- comments
    ]

sourceCommentStart :: SourceComment -> (Int, Int)
sourceCommentStart sourceComment =
  ( srcSpanStartLine $ sourceCommentSpan sourceComment
  , srcSpanStartCol $ sourceCommentSpan sourceComment
  )

sourceCommentEnd :: SourceComment -> (Int, Int)
sourceCommentEnd sourceComment =
  ( srcSpanEndLine $ sourceCommentSpan sourceComment
  , srcSpanEndCol $ sourceCommentSpan sourceComment
  )

sourceSpanStart :: RealSrcSpan -> (Int, Int)
sourceSpanStart span' = (srcSpanStartLine span', srcSpanStartCol span')

sourceSpanEnd :: RealSrcSpan -> (Int, Int)
sourceSpanEnd span' = (srcSpanEndLine span', srcSpanEndCol span')

layoutBooleanFormula
  :: LBooleanFormula GhcPs -> ToBriDocM BriDocNumbered
layoutBooleanFormula formula@(L _ body) = docWrapNode (toL formula) $ case body of
  Var name -> docLit =<< lrdrNameToTextAnn (toL name)
  And formulas -> layoutBooleanFormulaList ", " formulas
  Or formulas -> layoutBooleanFormulaList " | " formulas
  Parens nested -> docSeq
    [ docLitS "("
    , layoutBooleanFormula nested
    , docLitS ")"
    ]

layoutBooleanFormulaList
  :: String -> [LBooleanFormula GhcPs] -> ToBriDocM BriDocNumbered
layoutBooleanFormulaList separator formulas = docSeq
  $ List.intersperse (docLitS separator)
  $ layoutBooleanFormula <$> formulas

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
    patDoc <- patternDocument =<< layoutPattern lPat
    expDoc <- docSharedWrapper layoutExpr (toL expr)
    docCols
      ColBindStmt
      [ appSep $ pure patDoc
      , docSeq [appSep $ docLit $ Text.pack "<-", expDoc]
      ]
  _ -> unknownNodeError "" lgstmt -- TODO


--------------------------------------------------------------------------------
-- HsBind
--------------------------------------------------------------------------------

layoutBind
  :: ToBriDocC (HsBindLR GhcPs GhcPs) (Either [BriDocNumbered] BriDocNumbered)
layoutBind = layoutBindWithComments []

layoutBindWithComments
  :: [SourceComment]
  -> ToBriDocC (HsBindLR GhcPs GhcPs) (Either [BriDocNumbered] BriDocNumbered)
layoutBindWithComments declarationComments lbind@(L _ bind) = case bind of
  FunBind _ fId (MG _ lmatches@(L _ matches)) -> do
    idStr <- applyNameAdornment fId <$> lrdrNameToTextAnn (toL fId)
    binderDoc <- docLit $ Text.pack "="
    let previousMatches = Nothing : (Just <$> matches)
    funcPatDocs <-
      docWrapNode (toL lbind)
      $ docWrapAnnKeyList
        (ExactPrintCompat.mkNamedAnnKey "MatchGroup" $ getLoc $ toL lmatches)
      $ sequence
      $ zipWith
          (layoutFunctionMatch declarationComments (Just idStr) binderDoc)
          previousMatches
          matches
    return $ Left $ funcPatDocs
  PatBind _ pat _ (GRHSs _ grhssNE whereBinds) -> do
    let grhss = NonEmpty.toList grhssNE
    commentPlan <- mAsk
    nestedComments <- sourceCommentsWithinNode lbind
    let availableComments = List.nubBy
          (\left right -> sourceCommentKey left == sourceCommentKey right)
          $ declarationComments ++ nestedComments
    let separatorComments = case grhss of
          [grhs] -> commentsAfterGrhsSeparator
            commentPlan availableComments grhs
          _ -> []
        remainingComments = filter
          (`notElem` separatorComments)
          availableComments
    patLayout <- layoutPattern pat
    patDocs <- patternCompactDocument patLayout
    let multilinePatDoc = patternStructuralDocument patLayout
    clauseDocs <- layoutGrhs remainingComments `mapM` grhss
    mWhereDocs <- layoutLocalBinds (L (localBindsSpan whereBinds) whereBinds)
    let mWhereArg = mWhereDocs <&> (,) (mkAnnKey (toL lbind)) -- TODO: is this the right AnnKey?
    rawBinderDoc <- docLit $ Text.pack "="
    binderDoc <- appendSourceComments (pure rawBinderDoc) separatorComments
    hasComments <- hasAnyCommentsBelow (toL lbind)
    formatted <- docWrapNode (toL lbind) $ layoutPatternBindFinal
      OptionalSiblingAlignment Nothing binderDoc (Just patDocs)
      multilinePatDoc clauseDocs mWhereArg
      (hasComments || not (null separatorComments))
    Right <$> prependConsumedComments
      (separatorComments ++ handledClauseComments clauseDocs)
      (pure formatted)
  PatSynBind _ (PSB _ patID lpat rpat dir) -> do
    fmap Right $ docWrapNode (toL lbind) $ layoutPatSynBind (toL patID) lpat dir rpat
  _ -> Right <$> unknownNodeError "" (toL lbind)

layoutFunctionMatch
  :: [SourceComment]
  -> Maybe Text
  -> BriDocNumbered
  -> Maybe (LMatch GhcPs (LHsExpr GhcPs))
  -> LMatch GhcPs (LHsExpr GhcPs)
  -> ToBriDocM BriDocNumbered
layoutFunctionMatch declarationComments funId binderDoc previous current = do
  let boundaryComments = maybe []
        (\prior -> commentsBetweenMatches declarationComments prior current)
        previous
      remainingComments = filter
        (`notElem` boundaryComments)
        declarationComments
  formatted <- layoutPatternBind remainingComments funId binderDoc current
  case boundaryComments of
    [] -> pure formatted
    _ -> prependConsumedComments boundaryComments $ docLines
      $ (layoutPatSynComment <$> boundaryComments) ++ [pure formatted]

commentsBetweenMatches
  :: [SourceComment]
  -> LMatch GhcPs (LHsExpr GhcPs)
  -> LMatch GhcPs (LHsExpr GhcPs)
  -> [SourceComment]
commentsBetweenMatches sourceComments previous current = case
    ( srcSpanToRealSpan $ getLoc $ toL previous
    , srcSpanToRealSpan $ getLoc $ toL current
    ) of
  (Just previousSpan, Just currentSpan) -> List.sortOn sourceCommentStart
    $ filter (isBoundaryComment previousSpan currentSpan) sourceComments
  _ -> []
 where
  isBoundaryComment previousSpan currentSpan sourceComment =
    srcSpanStartLine (sourceCommentSpan sourceComment)
      > srcSpanEndLine previousSpan
      && sourceSpanEnd previousSpan <= sourceCommentStart sourceComment
      && sourceCommentEnd sourceComment <= sourceSpanStart currentSpan

commentsAfterGrhsSeparator
  :: CommentPlan
  -> [SourceComment]
  -> LGRHS GhcPs (LHsExpr GhcPs)
  -> [SourceComment]
commentsAfterGrhsSeparator commentPlan sourceComments lgrhs@(L _ (GRHS _ _ body)) =
  case
      ( srcSpanToRealSpan $ getLoc $ toL lgrhs
      , srcSpanToRealSpan $ getLoc $ toL body
      ) of
  (Just grhsSpan, Just bodySpan) -> List.sortOn sourceCommentStart
    $ filter (isInlineBetween grhsSpan bodySpan) sourceComments
  _ -> []
 where
  isInlineBetween grhsSpan bodySpan sourceComment =
    sourceSpanStart grhsSpan <= sourceCommentStart sourceComment
      && sourceCommentEnd sourceComment <= sourceSpanStart bodySpan
      && case Map.lookup (sourceCommentKey sourceComment)
          $ commentPlanPlacements commentPlan of
        Just placement ->
          placementLineRelation placement == InlineComment
            || srcSpanStartLine grhsSpan
              == srcSpanStartLine (sourceCommentSpan sourceComment)
        Nothing -> False
commentsAfterGrhsSeparator _ _ (L _ (XGRHS _)) = []

layoutIPBind :: ToBriDoc IPBind
layoutIPBind lipbind@(L _ bind) = case bind of
  IPBind _ lipName expr -> case unLoc lipName of
    HsIPName name -> do
      ipName <- docLit $ Text.pack $ '?' : FastString.unpackFS name
      binderDoc <- docLit $ Text.pack "="
      exprDoc <- layoutExpr (toL expr)
      hasComments <- hasAnyCommentsBelow (toL lipbind)
      layoutPatternBindFinal
        OptionalSiblingAlignment
        Nothing
        binderDoc
        (Just ipName)
        Nothing
        [([], exprDoc, expr, [])]
        Nothing
        hasComments
    _ -> briDocByExactNoComment ImplicitParameterFallback (toL lipbind)


data BagBindOrSig = BagBind (LHsBindLR GhcPs GhcPs)
                  | BagSig (LSig GhcPs)

bindOrSigtoSrcSpan :: BagBindOrSig -> SrcSpan
bindOrSigtoSrcSpan (BagBind b) = getLocA b
bindOrSigtoSrcSpan (BagSig s) = getLocA s

layoutLocalBinds
  :: ToBriDocC (HsLocalBindsLR GhcPs GhcPs) (Maybe [BriDocNumbered])
layoutLocalBinds = layoutLocalBindsWithComments []

layoutLocalBindsWithComments
  :: [SourceComment]
  -> ToBriDocC (HsLocalBindsLR GhcPs GhcPs) (Maybe [BriDocNumbered])
layoutLocalBindsWithComments outerComments lbinds@(L _ binds) = case binds of
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
    nestedComments <- sourceCommentsWithinNode lbinds
    let localComments = List.nubBy
          (\left right -> sourceCommentKey left == sourceCommentKey right)
          $ outerComments ++ nestedComments
    itemDocs <- forM ordered $ \case
      BagBind binding -> do
        let bindingComments = filter
              (sourceCommentWithinNodeSpan $ toL binding)
              localComments
        layoutBindWithComments bindingComments (toL binding) >>= \case
          Left docs -> docLines $ pure <$> docs
          Right doc -> pure doc
      BagSig signature -> layoutSig (toL signature)
    docs <- docWrapAnnKeyList
      (ExactPrintCompat.mkNamedAnnKey "HsValBinds" $ getLoc lbinds)
      $ pure itemDocs
    let previousItems = Nothing : (Just <$> ordered)
        leadingComments = zipWith
          (leadingLocalComments localComments)
          previousItems
          ordered
    Just <$> sequence
      (zipWith prependLocalComments leadingComments
        (zip (bindOrSigtoSrcSpan <$> ordered) docs)
      )
--  x@(HsValBinds (ValBindsOut _binds _lsigs)) ->
  HsValBinds _ (XValBindsLR{}) -> error "brittany internal error: XValBindsLR"
  HsIPBinds _ (IPBinds _ bb) -> Just <$> mapM (layoutIPBind . toL) bb
  EmptyLocalBinds{} -> return $ Nothing

localBindsSpan :: HsLocalBindsLR GhcPs GhcPs -> SrcSpan
localBindsSpan (HsValBinds (EpAnn anchor _ _) _) = case anchor of
  EpaSpan span' -> span'
  _ -> noSrcSpan
localBindsSpan _ = noSrcSpan

leadingLocalComments
  :: [SourceComment]
  -> Maybe BagBindOrSig
  -> BagBindOrSig
  -> [SourceComment]
leadingLocalComments sourceComments previous current = List.sortOn
    sourceCommentStart
  $ filter isLeading sourceComments
 where
  isLeading sourceComment = case
      ( srcSpanToRealSpan $ bindOrSigtoSrcSpan current
      , srcSpanToRealSpan . bindOrSigtoSrcSpan =<< previous
      ) of
    (Just currentSpan, previousSpan) ->
      sourceCommentEnd sourceComment
        <= (srcSpanStartLine currentSpan, srcSpanStartCol currentSpan)
        && maybe True
          (\span' ->
            (srcSpanEndLine span', srcSpanEndCol span')
              <= sourceCommentStart sourceComment
          )
          previousSpan
    _ -> False

prependLocalComments
  :: [SourceComment]
  -> (SrcSpan, BriDocNumbered)
  -> ToBriDocM BriDocNumbered
prependLocalComments [] (_, formatted) = pure formatted
prependLocalComments sourceComments (itemSpan, formatted) =
  prependConsumedComments sourceComments $ docLines
    $ (layoutPatSynComment <$> sourceComments)
    ++ blankLineBeforeItem
    ++ [pure formatted]
 where
  blankLineBeforeItem = case
      (List.last sourceComments, srcSpanToRealSpan itemSpan) of
    (lastComment, Just realItemSpan)
      | srcSpanStartLine realItemSpan
          - srcSpanEndLine (sourceCommentSpan lastComment) > 1 -> [docBlankLine]
    _ -> []

-- TODO: we don't need the `LHsExpr GhcPs` anymore, now that there is
-- parSpacing stuff.B
layoutGrhs
  :: [SourceComment]
  -> LGRHS GhcPs (LHsExpr GhcPs)
  -> ToBriDocM
       ([BriDocNumbered], BriDocNumbered, LHsExpr GhcPs, [SourceComment])
layoutGrhs declarationComments lgrhs@(L _ (GRHS _ guards body)) = do
  guardDocs <- forM guards $ \guard -> do
    guardDoc <- layoutStmt (toL guard)
    appendSourceComments (pure guardDoc)
      $ filter (sourceCommentFollowsNodeSameLine $ toL guard)
      declarationComments
  let trailingGuardComments = List.concat
        [ filter (sourceCommentFollowsNodeSameLine $ toL guard)
            declarationComments
        | guard <- guards
        ]
      trailingBodyComments = filter
        (sourceCommentFollowsNodeSameLine $ toL body)
        declarationComments
      boundaryComments = trailingGuardComments ++ trailingBodyComments
      leadingBodyComments = filter
        (\sourceComment ->
          sourceCommentStartsWithinNode (toL lgrhs) sourceComment
            && sourceCommentPrecedesNode (toL body) sourceComment
            && sourceComment `notElem` boundaryComments
        )
        declarationComments
      handledComments = List.nubBy
        (\left right -> sourceCommentKey left == sourceCommentKey right)
        $ leadingBodyComments ++ boundaryComments
  bodyDoc <- docWrapNode (toL lgrhs)
    $ appendSourceComments
      (case leadingBodyComments of
        [] -> layoutExpr (toL body)
        _ -> docLines
          $ [ layoutPatSynComment sourceComment
            | sourceComment <- leadingBodyComments
            ]
          ++ [layoutExpr (toL body)]
      )
      trailingBodyComments
  return (guardDocs, bodyDoc, body, handledComments)

sourceCommentFollowsNodeSameLine
  :: Located ast -> SourceComment -> Bool
sourceCommentFollowsNodeSameLine node sourceComment = case
    srcSpanToRealSpan $ getLoc node of
  Just nodeSpan ->
    srcSpanEndLine nodeSpan == srcSpanStartLine (sourceCommentSpan sourceComment)
      && srcSpanEndCol nodeSpan <= srcSpanStartCol (sourceCommentSpan sourceComment)
  Nothing -> False

handledClauseComments
  :: [([BriDocNumbered], BriDocNumbered, LHsExpr GhcPs, [SourceComment])]
  -> [SourceComment]
handledClauseComments = List.concatMap (\(_, _, _, comments) -> comments)

prependConsumedComments
  :: [SourceComment]
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
prependConsumedComments _ formatted = formatted

sourceCommentPrecedesNode :: Located ast -> SourceComment -> Bool
sourceCommentPrecedesNode node sourceComment = case
    srcSpanToRealSpan $ getLoc node of
  Just nodeSpan ->
      (srcSpanEndLine $ sourceCommentSpan sourceComment, srcSpanEndCol $ sourceCommentSpan sourceComment)
        <= (srcSpanStartLine nodeSpan, srcSpanStartCol nodeSpan)
  Nothing -> False

sourceCommentStartsWithinNode :: Located ast -> SourceComment -> Bool
sourceCommentStartsWithinNode node sourceComment = case
    srcSpanToRealSpan $ getLoc node of
  Just nodeSpan ->
    (srcSpanStartLine nodeSpan, srcSpanStartCol nodeSpan)
      <= ( srcSpanStartLine $ sourceCommentSpan sourceComment
         , srcSpanStartCol $ sourceCommentSpan sourceComment
         )
  Nothing -> False

sourceCommentWithinNodeSpan :: Located ast -> SourceComment -> Bool
sourceCommentWithinNodeSpan node sourceComment = case
    srcSpanToRealSpan $ getLoc node of
  Just nodeSpan ->
      (srcSpanStartLine nodeSpan, srcSpanStartCol nodeSpan)
        <= ( srcSpanStartLine $ sourceCommentSpan sourceComment
           , srcSpanStartCol $ sourceCommentSpan sourceComment
           )
        && ( srcSpanEndLine $ sourceCommentSpan sourceComment
           , srcSpanEndCol $ sourceCommentSpan sourceComment
           )
          <= (srcSpanEndLine nodeSpan, srcSpanEndCol nodeSpan)
  Nothing -> False

sourceCommentsWithinNode
  :: Located ast -> ToBriDocM [SourceComment]
sourceCommentsWithinNode node = do
  commentPlan <- mAsk
  pure $ List.sortOn sourceCommentStart
    $ filter (sourceCommentWithinNodeSpan node)
    $ Map.elems
    $ commentPlanSources commentPlan

layoutPatternBind
  :: [SourceComment]
  -> Maybe Text
  -> BriDocNumbered
  -> LMatch GhcPs (LHsExpr GhcPs)
  -> ToBriDocM BriDocNumbered
layoutPatternBind declarationComments funId binderDoc lmatch@(L _ match) = do
  let pats = unLoc (m_pats match)
  let (GRHSs _ grhssNE whereBinds) = m_grhss match
      grhss = NonEmpty.toList grhssNE
  commentPlan <- mAsk
  nestedComments <- sourceCommentsWithinNode $ toL lmatch
  let availableComments = List.nubBy
        (\left right -> sourceCommentKey left == sourceCommentKey right)
        $ declarationComments ++ nestedComments
  let separatorComments = case grhss of
        [grhs] -> commentsAfterGrhsSeparator
          commentPlan availableComments grhs
        _ -> []
      remainingComments = filter
        (`notElem` separatorComments)
        availableComments
  binderWithComments <- appendSourceComments
    (pure binderDoc)
    separatorComments
  patLayouts <- mapM layoutPattern pats
  patDocs <- mapM (fmap pure . patternCompactDocument) patLayouts
  let isInfix = isInfixMatch match
  mIdStr <- case match of
    Match _ (FunRhs matchId _ _ _) _ _ -> Just . applyNameAdornment matchId <$> lrdrNameToTextAnn (toL matchId)
    _ -> pure Nothing
  let mIdStr' = fixPatternBindIdentifier match <$> mIdStr
  let multilinePatDocs = patternStructuralDocument <$> patLayouts
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
        $ appSep (docLit idStr)
        : spacifyDocs (docForceSingleline <$> ps)
    (Nothing, ps) ->
      docCols ColPatterns
        $ (List.intersperse docSeparator $ docForceSingleline <$> ps)
  mMultilinePatDoc <- if isInfix || not (any Data.Maybe.isJust multilinePatDocs)
    then return Nothing
    else do
      selectedDocs <- sequence $ zipWith
        (\flatDoc -> maybe flatDoc $ \multilineDoc -> docAlt
          [ docForceSingleline flatDoc
          , return multilineDoc
          ])
        patDocs
        multilinePatDocs
      case (mIdStr', selectedDocs) of
        (Just idStr, ps@(_ : _)) -> fmap Just
          $ docWrapNodePrior (toL lmatch)
          $ docAddBaseY BrIndentRegular
          $ docPar (docLit idStr) (docLines $ return <$> ps)
        (Nothing, [p]) -> fmap Just
          $ docWrapNodePrior (toL lmatch)
          $ return p
        _ -> return Nothing
  let matchComments = filter
        (sourceCommentWithinNodeSpan $ toL lmatch)
        remainingComments
  clauseDocs <- docWrapNodeRest (toL lmatch)
    $ layoutGrhs matchComments `mapM` grhss
  mWhereDocs <- layoutLocalBinds (L (localBindsSpan whereBinds) whereBinds)
  let mWhereArg = mWhereDocs <&> (,) (mkAnnKey (toL lmatch))
  let alignmentToken = if null pats then Nothing else funId
      alignmentScope = case m_ctxt match of
        FunRhs{} -> OptionalSiblingAlignment
        _ -> RequiredPatternAlignment
  hasComments <- case mWhereDocs of
    Nothing -> hasAnyRegularCommentsConnectedNoFollowing (toL lmatch)
    Just _  -> hasAnyCommentsBelow (toL lmatch)
  prependConsumedComments
    (separatorComments ++ handledClauseComments clauseDocs)
    $ layoutPatternBindFinal alignmentScope alignmentToken binderWithComments
      (Just patDoc) mMultilinePatDoc clauseDocs mWhereArg
      (hasComments || not (null separatorComments))

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

data BindingAlignmentScope
  = RequiredPatternAlignment
  | OptionalSiblingAlignment

layoutPatternBindFinal
  :: BindingAlignmentScope
  -> Maybe Text
  -> BriDocNumbered
  -> Maybe BriDocNumbered
  -> Maybe BriDocNumbered
  -> [([BriDocNumbered], BriDocNumbered, LHsExpr GhcPs, [SourceComment])]
  -> Maybe (AnnKey, [BriDocNumbered])
     -- ^ AnnKey for the node that contains the AnnWhere position annotation
  -> Bool
  -> ToBriDocM BriDocNumbered
layoutPatternBindFinal alignmentScope alignmentToken binderDoc mPatDoc mMultilinePatDoc clauseDocs mWhereDocs hasComments
  = do
    let alignmentCandidates = case alignmentScope of
          RequiredPatternAlignment -> [StructuralAffinity $ Right ()]
          OptionalSiblingAlignment ->
            [StructuralAffinity $ Left token | token <- maybeToList alignmentToken]
              ++ [OptionalAlignment $ Right ()]
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
    indentAmount <-
      mAsk <&> _conf_layout .> _lconfig_indentAmount .> confUnpack
    let multilinePatternBodyIndent = BrIndentSpecial (2 * indentAmount)

    runFilteredAlternative $ do

      case clauseDocs of
        [(guards, body, _bodyRaw, _)] -> do
          let guardPart = singleLineGuardsDoc guards
          addAlternativeCond hasComments $ docLines
            $ [ docSeq (patPartInline ++ [guardPart, pure binderDoc])
              , docNonBottomSpacing
                $ docEnsureIndent BrIndentRegular
                $ docAddBaseY BrIndentRegular
                $ pure body
              ]
            ++ wherePartMultiLine
          forM_ wherePart $ \wherePart' ->
            -- one-line solution
            addAlternativeCond (not hasComments) $ docCols
              (ColBindingLine alignmentCandidates)
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
                  (ColBindingLine alignmentCandidates)
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
          addAlternativeCond (not hasComments)
            $ docLines
            $ [ docForceSingleline
                $ docSeq (patPartInline ++ [guardPart, return binderDoc])
              , docEnsureIndent BrIndentRegular $ docForceSingleline $ return
                body
              ]
            ++ wherePartMultiLine
          -- pattern and exactly one clause in single line, body as par;
          -- where in following lines
          addAlternativeCond (not hasComments)
            $ docLines
            $ [ docCols
                  (ColBindingLine alignmentCandidates)
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
          addAlternativeCond (not hasComments)
            $ docLines
            $ [ docSeq (patPartInline ++ [guardPart, return binderDoc])
              , docNonBottomSpacing
              $ docEnsureIndent BrIndentRegular
              $ docAddBaseY BrIndentRegular
              $ return body
              ]
            ++ wherePartMultiLine
          case mMultilinePatDoc of
            Nothing -> return ()
            Just patDoc ->
              addAlternative
                $ docLines
                $ [ docSeq
                      [ appSep $ return patDoc
                      , guardPart
                      , return binderDoc
                      ]
                  , docNonBottomSpacing
                  $ docEnsureIndent multilinePatternBodyIndent
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
                  <&> \(guardDocs, bodyDoc, _, _) -> do
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
      addAlternativeCond (not hasComments)
        $ docLines
        $ [ docAddBaseY BrIndentRegular
            $ patPartParWrap
            $ docLines
            $ map docSetBaseY
            $ clauseDocs
            <&> \(guardDocs, bodyDoc, _, _) -> do
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
      addAlternativeCond (not hasComments)
        $ docLines
        $ [ docAddBaseY BrIndentRegular
            $ patPartParWrap
            $ docLines
            $ map docSetBaseY
            $ clauseDocs
            <&> \(guardDocs, bodyDoc, _, _) ->
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
            >>= \(guardDocs, bodyDoc, _, _) ->
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
            >>= \(guardDocs, bodyDoc, _, _) ->
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
      case mMultilinePatDoc of
        Nothing -> return ()
        Just patDoc ->
          addAlternativeCond (length clauseDocs > 1 && not hasComments)
            $ docLines
            $ [ return patDoc
              , docEnsureIndent BrIndentRegular
              $ docLines
              $ clauseDocs
              <&> \(guardDocs, bodyDoc, _, _) -> do
                    let guardPart = singleLineGuardsDoc guardDocs
                    docForceSingleline $ docCols
                      ColGuardedBody
                      [ guardPart
                      , docSeq
                        [ appSep $ return binderDoc
                        , docForceSingleline $ return bodyDoc
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
    whereDoc = docLit $ Text.pack "where"
  bodyLayout <- layoutPattern rpat
  body <- docSharedWrapper patternCompactDocument bodyLayout
  hasBodyComments <- hasAnyRegularCommentsConnectedNoFollowing (toL rpat)
  let selectedBody = case patternStructuralDocument bodyLayout of
        Nothing -> body
        Just structuralBody
          | hasBodyComments -> pure structuralBody
          | otherwise -> docAlt
            [ docForceSingleline body
            , pure structuralBody
            ]
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
    addAlternativeCond (not hasBodyComments)
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
            Nothing -> selectedBody
            Just ds -> docLines
              ( [ docSeq
                    [ selectedBody
                    , docSeparator
                    , whereDoc
                    ]
                ]
                ++ ds
              )
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
    Just <$> mapM (layoutPatSynBuilder binderDoc) lbinds
  _ -> pure Nothing

layoutPatSynBuilder
  :: BriDocNumbered
  -> LMatch GhcPs (LHsExpr GhcPs)
  -> ToBriDocM (ToBriDocM BriDocNumbered)
layoutPatSynBuilder binderDoc match = do
  builderDoc <- docSharedWrapper (layoutPatternBind [] Nothing binderDoc) match
  commentPlan <- mAsk
  let matchKeys = foldedAnnKeys match
      leadingComments =
        [ sourceComment
        | (key, placement) <- List.sortOn
            (placementRelativeOrder . snd)
            $ Map.toList
            $ commentPlanPlacements commentPlan
        , NodeId ownerKey <- [placementOwner placement]
        , Set.member ownerKey matchKeys
        , placementRole placement `elem` [LeadingDoc, LeadingOrdinary]
        , Just sourceComment <- [Map.lookup key $ commentPlanSources commentPlan]
        , commentBeforeMatch sourceComment
        ]
  pure $ case leadingComments of
    [] -> builderDoc
    _  -> docLines
      $ (layoutPatSynComment <$> leadingComments) ++ [builderDoc]
 where
  commentBeforeMatch sourceComment = case
      srcSpanToRealSpan $ getLoc $ toL match
    of
      Nothing -> False
      Just matchSpan ->
        ( srcSpanEndLine $ sourceCommentSpan sourceComment
        , srcSpanEndCol $ sourceCommentSpan sourceComment
        ) <= (srcSpanStartLine matchSpan, srcSpanStartCol matchSpan)

layoutPatSynComment :: SourceComment -> ToBriDocM BriDocNumbered
layoutPatSynComment sourceComment = briDocBySourceFragmentNoComment
  (L (realSpanToSrcSpan $ sourceCommentSpan sourceComment) sourceComment)
  (sourceCommentFragment sourceComment)

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
  ClassDecl _ context name variables Prefix [] signatures methods [] [] [] ->
    layoutClassDecl ltycl context (toL name) variables signatures methods
  FamDecl _ family
    | supportsNativeFamily tycl -> layoutFamilyDecl ltycl family
  _ -> briDocByExactNoComment TypeClassDeclarationFallback ltycl

supportsNativeFamily :: TyClDecl GhcPs -> Bool
supportsNativeFamily = \case
  FamDecl _ FamilyDecl
    { fdInfo = ClosedTypeFamily (Just _)
    , fdFixity = Infix
    , fdResultSig = L _ NoSig{}
    , fdInjectivityAnn = Nothing
    , fdTyVars = HsQTvs _ [_first, _second]
    } -> True
  _ -> False

layoutFamilyDecl
  :: Located (TyClDecl GhcPs)
  -> FamilyDecl GhcPs
  -> ToBriDocM BriDocNumbered
layoutFamilyDecl _ FamilyDecl
    { fdInfo = ClosedTypeFamily (Just equations)
    , fdLName = name
    , fdTyVars = HsQTvs _ [firstVariable, secondVariable]
    } = do
  nameText <- lrdrNameToTextAnn $ toL name
  let familyHead = docSeq
        [ appSep $ docLitS "type"
        , appSep $ docLitS "family"
        , layoutTyVarBndr False $ toL firstVariable
        , docSeparator
        , docWrapNode (toL name) $ docLit nameText
        , docSeparator
        , layoutTyVarBndr False $ toL secondVariable
        , docSeparator
        , docLitS "where"
        ]
      equationDocs = fmap layoutEquation equations
  renderedEquations <- docSortedLocatedLines equationDocs
  docLines
    [ familyHead
    , docNonBottomSpacingS
      $ docEnsureIndent BrIndentRegular
      $ docSetIndentLevel
      $ pure renderedEquations
    ]
 where
  layoutEquation equation@(L _ body) =
    L (getLoc $ toL equation) <$> docWrapNode (toL equation)
      (layoutInfixTyFamInstEqn (toL equation) (toL name) body)
layoutFamilyDecl outer _ =
  briDocByExactNoComment TypeClassDeclarationFallback outer

supportsNativeClass :: TyClDecl GhcPs -> Bool
supportsNativeClass = \case
  ClassDecl _ _ _ _ Prefix [] _ _ [] [] [] -> True
  _ -> False

layoutClassDecl
  :: Located (TyClDecl GhcPs)
  -> Maybe (LHsContext GhcPs)
  -> Located RdrName
  -> LHsQTyVars GhcPs
  -> [LSig GhcPs]
  -> LHsBinds GhcPs
  -> ToBriDocM BriDocNumbered
layoutClassDecl outer context name variables signatures methods = do
  contextPrefix <- layoutClassContext context
  nameText <- lrdrNameToTextAnn name
  commentPlan <- mAsk
  let binderDocs = layoutTyVarBndr True . toL <$> hsq_explicit variables
      classHead = docSeq
        $ [appSep $ docLitS "class"]
        ++ [appSep $ pure contextPrefix | Data.Maybe.isJust context]
        ++ [docWrapNode name $ docLit nameText]
        ++ binderDocs
        ++ [docSeparator, docLitS "where"]
      memberDocs = fmap (layoutAndLocateSig commentPlan . toL) signatures
        ++ fmap (layoutAndLocateBind . toL) methods
        ++ classMemberComments outer signatures commentPlan
  members <- docSortedLocatedLines memberDocs
  docLines
    [ classHead
    , docNonBottomSpacingS
      $ docEnsureIndent BrIndentRegular
      $ docSetIndentLevel
      $ pure members
    ]
 where
  layoutAndLocateSig commentPlan lsig@(L location _) = do
    L location
      <$> layoutSigWithComments (signatureHasPostDocs commentPlan lsig) lsig

  signatureHasPostDocs commentPlan signature = case
      srcSpanToRealSpan $ getLoc signature of
    Nothing -> False
    Just signatureSpan -> any
      (\(key, placement) ->
        placementRole placement `elem`
          [HaddockPostDoc SignatureArgument, HaddockPostDoc SignatureResult]
          && case Map.lookup key $ commentPlanSources commentPlan of
            Nothing -> False
            Just sourceComment ->
              (srcSpanStartLine signatureSpan, srcSpanStartCol signatureSpan)
                <= ( srcSpanStartLine $ sourceCommentSpan sourceComment
                   , srcSpanStartCol $ sourceCommentSpan sourceComment
                   )
                && ( srcSpanEndLine $ sourceCommentSpan sourceComment
                   , srcSpanEndCol $ sourceCommentSpan sourceComment
                   ) <= (srcSpanEndLine signatureSpan, srcSpanEndCol signatureSpan)
      )
      $ Map.toList $ commentPlanPlacements commentPlan

  layoutAndLocateBind lbind@(L location _) =
    L location <$> (joinBinds =<< layoutBind lbind)

classMemberComments
  :: Located (TyClDecl GhcPs)
  -> [LSig GhcPs]
  -> CommentPlan
  -> [ToBriDocM (Located BriDocNumbered)]
classMemberComments outer signatures commentPlan = case
    srcSpanToRealSpan $ getLoc outer of
  Nothing -> []
  Just outerSpan ->
    [ L commentSpan
        <$> layoutClassComment placement sourceComment
    | (key, placement) <- Map.toList $ commentPlanPlacements commentPlan
    , placementRole placement `elem`
        [ LeadingDoc
        , LeadingOrdinary
        , SectionComment
        , PragmaComment
        , HaddockPostDoc SignatureArgument
        , HaddockPostDoc SignatureResult
        ]
    , Just sourceComment <- [Map.lookup key $ commentPlanSources commentPlan]
    , not $ isSignaturePostDocPlacement placement
        && commentInsideSignature sourceComment
    , let realCommentSpan = sourceCommentSpan sourceComment
    , ( spanStart realCommentSpan > spanStart outerSpan
          && spanEnd realCommentSpan <= spanEnd outerSpan
      ) || placementOwner placement == NodeId (mkAnnKey outer)
          && isSignaturePostDocPlacement placement
        || isSignaturePostDocPlacement placement
          && fst (spanStart realCommentSpan) == fst (spanEnd outerSpan) + 1
    , let commentSpan = realSpanToSrcSpan realCommentSpan
    ]
 where
  layoutClassComment placement sourceComment
    | isSignaturePostDocPlacement placement = do
          indentAmount <- askIndent
          let fragment = (sourceCommentFragment sourceComment)
                { fragmentAbsoluteColumn = Just $ 2 * indentAmount }
          briDocBySourceFragmentNoComment
            (L (realSpanToSrcSpan $ sourceCommentSpan sourceComment)
              sourceComment)
            fragment
    | otherwise = briDocBySourceFragmentNoComment
        (L (realSpanToSrcSpan $ sourceCommentSpan sourceComment) sourceComment)
        (sourceCommentFragment sourceComment)

  isSignaturePostDocPlacement placement = placementRole placement `elem`
    [HaddockPostDoc SignatureArgument, HaddockPostDoc SignatureResult]

  commentInsideSignature sourceComment = any
    (\signature -> case srcSpanToRealSpan $ getLoc $ toL signature of
      Nothing -> False
      Just signatureSpan ->
        spanStart signatureSpan <= spanStart (sourceCommentSpan sourceComment)
          && spanEnd (sourceCommentSpan sourceComment) <= spanEnd signatureSpan
    )
    signatures

  spanStart span' = (srcSpanStartLine span', srcSpanStartCol span')
  spanEnd span' = (srcSpanEndLine span', srcSpanEndCol span')

layoutClassContext
  :: Maybe (LHsContext GhcPs) -> ToBriDocM BriDocNumbered
layoutClassContext = \case
  Nothing -> docEmpty
  Just (L _ constraints) -> do
    constraintDocs <- mapM (docSharedWrapper layoutType . toL) constraints
    case constraintDocs of
      [] -> docLitS "() =>"
      [constraint] -> docSeq [constraint, docLitS " =>"]
      firstConstraint : remainingConstraints -> docAlt
        [ docSeq
          $ [docLitS "(", docForceSingleline firstConstraint]
          ++ List.concatMap
            (\constraint -> [docLitS ", ", docForceSingleline constraint])
            remainingConstraints
          ++ [docLitS ") =>"]
        , docSetBaseY $ docLines
          $ [docSeq [appSep $ docLitS "(", firstConstraint]]
          ++ fmap
            (\constraint -> docCols ColList [docCommaSep, constraint])
            remainingConstraints
          ++ [docLitS ") =>"]
        ]

docSortedLocatedLines
  :: [ToBriDocM (Located BriDocNumbered)] -> ToBriDocM BriDocNumbered
docSortedLocatedLines documents =
  allocateNode
    . BDFLines
    . fmap unLoc
    . List.sortOn (ExactPrint.rs . getLoc)
    =<< sequence documents

joinBinds
  :: Either [BriDocNumbered] BriDocNumbered -> ToBriDocM BriDocNumbered
joinBinds = \case
  Left nodes -> docLines $ pure <$> nodes
  Right node -> pure node

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


--------------------------------------------------------------------------------
-- TyFamInstDecl
--------------------------------------------------------------------------------



layoutTyFamInstDecl
  :: Data.Data.Data a
  => Bool
  -> Located a
  -> TyFamInstDecl GhcPs
  -> ToBriDocM BriDocNumbered
layoutTyFamInstDecl inClass outerNode tfid =
  layoutTyFamInstEqn
    (Just $ if inClass then "type" else "type instance")
    outerNode
    (tfid_eqn tfid)

layoutInfixTyFamInstEqn
  :: (Data.Data.Data a, ExactPrint.ExactPrint a)
  => Located a
  -> Located RdrName
  -> TyFamInstEqn GhcPs
  -> ToBriDocM BriDocNumbered
layoutInfixTyFamInstEqn outerNode name eqn = case layoutHsTyPats $ feqn_pats eqn of
  [firstPattern, secondPattern] -> docWrapNodePrior outerNode $ do
    nameText <- lrdrNameToTextAnn name
    let lhs = docSeq
          [ appSep firstPattern
          , appSep $ docWrapNode name $ docLit nameText
          , secondPattern
          ]
    hasComments <- hasAnyRegularCommentsConnectedNoFollowing outerNode
    typeDoc <- docSharedWrapper layoutType $ toL $ feqn_rhs eqn
    layoutLhsAndType hasComments lhs "=" typeDoc
  _ -> briDocByExactNoComment FamilyDefaultFallback outerNode

layoutTyFamInstEqn
  :: Data.Data.Data a
  => Maybe String
  -> Located a
  -> TyFamInstEqn GhcPs
  -> ToBriDocM BriDocNumbered
layoutTyFamInstEqn keyword outerNode eqn = do
  let
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
      makeForallDoc :: forall flag. [LHsTyVarBndr flag GhcPs] -> ToBriDocM BriDocNumbered
      makeForallDoc bndrs = do
        bndrDocs <- layoutTyVarBndrs bndrs
        docSeq
          ([docLit (Text.pack "forall")] ++ processTyVarBndrsSingleline bndrDocs
          )
      lhs =
        docSeq
          $ [appSep $ docLit $ Text.pack word | Just word <- [keyword]]
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


layoutHsTyPats pats = pats <&> \case
  HsValArg _ tm -> layoutType (toL tm)
  HsTypeArg _ ty -> docSeq [docLit $ Text.pack "@", layoutType (toL ty)]
    -- we ignore the SourceLoc here.. this LPat not being (L _ Pat{}) change
    -- is a bit strange. Hopefully this does not ignore any important
    -- annotations.
  HsArgPar _ -> error "brittany internal error: HsArgPar{}"

--------------------------------------------------------------------------------
-- ClsInstDecl
--------------------------------------------------------------------------------

-- | Layout an @instance@ declaration
--
--   Layout signatures and bindings using the corresponding top-level
--   layouters. The instance layouter owns head and member indentation while
--   associated family declarations retain their existing layout paths.
layoutClsInst :: ToBriDoc ClsInstDecl
layoutClsInst lcid@(L _ cid) = layoutInstance lcid
  $ docSortedLines
  $ fmap (layoutAndLocateSig . toL) (cid_sigs cid)
  ++ fmap (layoutAndLocateBind . toL) (cid_binds cid)
  ++ fmap (layoutAndLocateTyFamInsts . toL) (cid_tyfam_insts cid)
  ++ fmap (layoutAndLocateDataFamInsts . toL) (cid_datafam_insts cid)
 where
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
  stripWhitespace (BDFExternal ann shouldAddComment source) =
    BDFExternal ann shouldAddComment $ mapExternalSourceText stripWhitespace' source
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
    stripNSpaces n t
      | n <= 0 = t
      | otherwise = case Text.uncons t of
          Just (' ', rest) -> stripNSpaces (n - 1) rest
          _ -> t
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
