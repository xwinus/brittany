{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Expr where

import qualified Data.Data
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import GHC (GenLocated(L), RdrName(..), SrcSpan, unLoc)
import GHC.Types.Name.Reader (RdrName(Exact))
import GHC.Types.SrcLoc (Located, getLoc, noSrcSpan)
import GHC.Parser.Annotation (EpAnn(..), EpaLocation(..), HasLoc(getHasLoc))
import Language.Haskell.Brittany.Internal.ExactPrintCompat (AnnKeywordId(..))
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as ExactPrintCompat
import qualified GHC.Data.FastString as FastString
import GHC.Hs
import GHC.Hs.Type (FieldOcc(..), HsSigType(HsSig), HsWildCardBndrs(HsWC))
import GHC.Types.SourceText (FractionalLit(..), IntegralLit(..), SourceText(..))
import Language.Haskell.Syntax.Basic (FieldLabelString(..))
import Language.Haskell.Brittany.Internal.Layouters.IE (toL)
import qualified Data.List.NonEmpty as NonEmpty
import qualified GHC.OldList as List
import GHC.Types.Basic
import GHC.Types.Name
import GHC.Types.Name.Occurrence (occNameString)
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Delimiter.Types
import Language.Haskell.Brittany.Internal.Fallbacks
  ( FallbackId(..)
  , OpaqueFamily(..)
  , untypedSpliceFamily
  )
import Language.Haskell.Brittany.Internal.ExpressionComments
  ( requiresExactSourceExpression )
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.Decl
import Language.Haskell.Brittany.Internal.Layouters.Pattern
import Language.Haskell.Brittany.Internal.Layouters.Stmt
import Language.Haskell.Brittany.Internal.Layouters.Type
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.SourceComment.Types
  ( CommentLineRelation(..)
  , CommentPlacement(..)
  , CommentPlan(..)
  , SourceComment(..)
  )
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Utils
import qualified Language.Haskell.GHC.ExactPrint as ExactPrint



-- | True if the section operator is symbolic (e.g. +, -) and thus needs no backticks.
isSymbolicSectionOp :: LHsExpr GhcPs -> ToBriDocM Bool
isSymbolicSectionOp (L _ expr) =
  pure $ case expr of
    HsVar _ (L _ (Unqual occ)) -> isSymbolic $ occNameString occ
    HsVar _ (L _ (Qual _ occ)) -> isSymbolic $ occNameString occ
    HsVar _ (L _ (Exact name)) -> isSymbolic $ getOccString name
    _ -> False
  where
    isSymbolic s = not (null s) && head s `elem` ("!:#$%&*+./<=>?@\\^|-~" :: String)

matchGroupKey
  :: HasLoc l => GenLocated l e -> ExactPrintCompat.AnnKey
matchGroupKey lmatches = ExactPrintCompat.mkNamedAnnKey
  "MatchGroup"
  (getLoc $ toL lmatches)

layoutOpaqueExpression
  :: OpaqueFamily
  -> GenLocated SrcSpan (HsExpr GhcPs)
  -> ToBriDocM BriDocNumbered
layoutOpaqueExpression family =
  briDocByOpaqueNoComment family ExpressionFallback

isBlockLikeExpression :: HsExpr GhcPs -> Bool
isBlockLikeExpression = \case
  HsLam _ LamCase _ -> True
  HsCase{} -> True
  HsMultiIf{} -> True
  HsDo _ stmtContext _ -> case stmtContext of
    DoExpr _ -> True
    MDoExpr _ -> True
    _ -> False
  HsPar _ inner -> isBlockLikeExpression $ unLoc inner
  _ -> False

isParenthesizedBlockExpression :: HsExpr GhcPs -> Bool
isParenthesizedBlockExpression = \case
  HsPar _ inner -> isBlockLikeExpression $ unLoc inner
  _ -> False

layoutOperatorLeftOperand
  :: LHsExpr GhcPs -> ToBriDocM (ToBriDocM BriDocNumbered)
layoutOperatorLeftOperand expLeft@(L _ (HsPar _ inner))
  | isBlockLikeExpression $ unLoc inner = do
    innerDoc <- docSharedWrapper
      (docWrapNode (toL expLeft) . layoutExpr')
      (toL inner)
    fmap pure $ docWrapNode (toL expLeft)
      $ docDelimitedBlock
        DelimiterAttached
        docParenL
        innerDoc
        (docEnsureIndent (BrIndentSpecial 1) docParenR)
layoutOperatorLeftOperand expLeft = docSharedWrapper layoutExpr' (toL expLeft)

layoutFlattenedOperatorApplication
  :: LHsExpr GhcPs
  -> LHsExpr GhcPs
  -> LHsExpr GhcPs
  -> ToBriDocM BriDocNumbered
layoutFlattenedOperatorApplication expLeft expOp expRight = do
  let gather opExprList = \case
        L _ (OpApp _ left op right) ->
          gather ((op, right) : opExprList) left
        final -> (final, opExprList)
      (leftOperand, appList) = gather [] expLeft
  leftOperandDoc <- docSharedWrapper layoutExpr' (toL leftOperand)
  appListDocs <- appList `forM` \(op, operand) -> do
    isSymbolic <- isSymbolicSectionOp op
    opDoc <- docSharedWrapper layoutExpr' (toL op)
    operandDoc <- docSharedWrapper layoutExpr' (toL operand)
    let wrappedOp = if isSymbolic
          then opDoc
          else docSeq [docLit $ Text.pack "`", opDoc, docLit $ Text.pack "`"]
    pure (wrappedOp, operandDoc)
  isLastSymbolic <- isSymbolicSectionOp expOp
  lastOpDoc' <- docSharedWrapper layoutExpr' (toL expOp)
  let lastOpDoc = if isLastSymbolic
        then lastOpDoc'
        else docSeq
          [docLit $ Text.pack "`", lastOpDoc', docLit $ Text.pack "`"]
  lastOperandDoc <- docSharedWrapper layoutExpr' (toL expRight)
  let allowPar = case (expOp, expRight) of
        (L _ (HsVar _ (L _ (Unqual occname))), _)
          | occNameString occname == "$" -> True
        (_, L _ (HsApp _ _ (L _ HsVar{}))) -> False
        _ -> True
  runFilteredAlternative $ do
    addAlternative $ docSeq
      [ appSep $ docForceSingleline leftOperandDoc
      , docSeq $ appListDocs <&> \(opDoc, operandDoc) -> docSeq
        [ appSep $ docForceSingleline opDoc
        , appSep $ docForceSingleline operandDoc
        ]
      , appSep $ docForceSingleline lastOpDoc
      , (if allowPar then docForceParSpacing else docForceSingleline)
        lastOperandDoc
      ]
    addAlternative $ docPar
      leftOperandDoc
      (docLines
      $ (appListDocs <&> \(opDoc, operandDoc) ->
          docCols ColOpPrefix [appSep opDoc, docSetBaseY operandDoc]
        )
      ++ [ docCols
             ColOpPrefix
             [appSep lastOpDoc, docSetBaseY lastOperandDoc]
         ]
      )

layoutOperatorApplication
  :: LHsExpr GhcPs
  -> LHsExpr GhcPs
  -> LHsExpr GhcPs
  -> ToBriDocM BriDocNumbered
layoutOperatorApplication expLeft expOp expRight = do
  expDocLeft <- layoutOperatorLeftOperand expLeft
  expDocOp <- docSharedWrapper layoutExpr' (toL expOp)
  isSymOp <- isSymbolicSectionOp expOp
  let expDocOp' = if isSymOp
        then expDocOp
        else docSeq [docLit $ Text.pack "`", expDocOp, docLit $ Text.pack "`"]
  expDocRight <- docSharedWrapper layoutExpr' (toL expRight)
  let allowPar = case (expOp, expRight) of
        (L _ (HsVar _ (L _ (Unqual occname))), _)
          | occNameString occname == "$" -> True
        (_, L _ (HsApp _ _ (L _ HsVar{}))) -> False
        _ -> True
      leftIsDoBlock = case expLeft of
        L _ HsDo{} -> True
        _ -> False
      leftIsParenthesizedBlock =
        isParenthesizedBlockExpression $ unLoc expLeft
      layoutRight = if isParenthesizedBlockExpression $ unLoc expRight
        then expDocRight
        else docSetBaseY expDocRight
      layoutMultiline opAndRight
        | leftIsParenthesizedBlock = docSeq [appSep expDocLeft, opAndRight]
        | leftIsDoBlock = docLines [expDocLeft, opAndRight]
        | otherwise = docAddBaseY BrIndentRegular
          $ docPar expDocLeft opAndRight
  runFilteredAlternative $ do
    addAlternative $ docSeq
      [ appSep $ docForceSingleline expDocLeft
      , appSep $ docForceSingleline expDocOp'
      , docForceSingleline expDocRight
      ]
    addAlternative $ do
      let expDocOpAndRight = docForceSingleline $ docCols
            ColOpPrefix
            [appSep expDocOp', layoutRight]
      layoutMultiline expDocOpAndRight
    addAlternativeCond allowPar $ docSeq
      [ appSep $ docForceSingleline expDocLeft
      , appSep $ docForceSingleline expDocOp'
      , docForceParSpacing expDocRight
      ]
    addAlternative $ do
      let expDocOpAndRight =
            docCols ColOpPrefix [appSep expDocOp', layoutRight]
      layoutMultiline expDocOpAndRight

layoutExpr :: ToBriDoc HsExpr
layoutExpr lexpr = layoutExpr' (toL lexpr)

commentsAfterLambdaArrow
  :: EpAnn GrhsAnn
  -> LHsExpr GhcPs
  -> ToBriDocM [SourceComment]
commentsAfterLambdaArrow (EpAnn _ grhsAnnotations _) body = case
    ga_sep grhsAnnotations of
  Left _ -> pure []
  Right arrow -> case
      ( ExactPrintCompat.srcSpanToRealSpan $ getHasLoc arrow
      , ExactPrintCompat.srcSpanToRealSpan $ getLoc $ toL body
      ) of
    (Just arrowSpan, Just bodySpan) -> do
      commentPlan <- mAsk
      pure $ List.sortOn sourceCommentStart
        $ filter
          (\sourceComment ->
            sourceSpanEnd arrowSpan <= sourceCommentStart sourceComment
              && sourceCommentEnd sourceComment <= sourceSpanStart bodySpan
          )
        $ Map.elems
        $ commentPlanSources commentPlan
    _ -> pure []

isInlineComment :: CommentPlan -> SourceComment -> Bool
isInlineComment commentPlan sourceComment = case
    Map.lookup (sourceCommentKey sourceComment)
      $ commentPlanPlacements commentPlan of
  Just placement -> placementLineRelation placement == InlineComment
  Nothing -> False

layoutExpr' :: ToBriDoc HsExpr
layoutExpr' lexpr@(L _ expr) =
  if requiresExactSourceExpression expr
    then briDocByExactInlineOnly ExpressionFallback lexpr
    else layoutExprNative lexpr

layoutExprNative :: ToBriDoc HsExpr
layoutExprNative lexpr@(L _ expr) = do
  indentPolicy <- mAsk <&> _conf_layout .> _lconfig_indentPolicy .> confUnpack
  let allowFreeIndent = indentPolicy == IndentPolicyFree
  docWrapNode lexpr $ case expr of
    HsVar _ vname -> do
      t <- lrdrNameToTextAnn (toL vname)
      -- GHC 9.14: apply paren adornment from NameAnn for operators in
      -- expression position. Backticks are handled by expression callers.
      let t' = applyNameAdornmentParensOnly vname t
      docLit t'
    XExpr _ -> briDocByExactInlineOnly ExpressionFallback lexpr
    HsOverLabel _ name ->
      let label = FastString.unpackFS name in docLit . Text.pack $ '#' : label
    HsIPVar _ext (HsIPName name) ->
      let label = FastString.unpackFS name in docLit . Text.pack $ '?' : label
    HsOverLit _ olit -> do
      allocateNode $ overLitValBriDoc $ ol_val olit
    HsLit _ lit -> do
      allocateNode $ litBriDoc lit
    HsLam _ LamCase (MG _ lmatches) | null (unLoc lmatches) -> do
      docWrapAnnKey (matchGroupKey lmatches)
        $ docSetParSpacing
        $ docAddBaseY BrIndentRegular
        $ (docLit $ Text.pack "\\case {}")
    HsLam _ LamCase (MG _ lmatches) -> do
      let matches = unLoc lmatches
      binderDoc <- docLit $ Text.pack "->"
      funcPatDocs <-
        docWrapAnnKeyList (matchGroupKey lmatches)
        $ layoutPatternBind [] Nothing binderDoc
        `mapM` matches
      docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
        (docLit $ Text.pack "\\case")
        (docSetBaseAndIndent
        $ docNonBottomSpacing
        $ docLines
        $ return
        <$> funcPatDocs
        )
    HsLam _ LamSingle (MG _ lmatches)
      | [lmatch@(L _ match)] <- unLoc lmatches
      , pats <- unLoc (m_pats match)
      , GRHSs _ grhssNE llocals <- m_grhss match
      , [lgrhs] <- NonEmpty.toList grhssNE
      , EmptyLocalBinds{} <- llocals
      , GRHS grhsAnnotation [] body <- unLoc lgrhs
      -> do
        patternLayouts <- zip (True : repeat False) pats `forM` \(isFirst, p) -> do
          layout <- layoutPattern p
          compactDocument <- do
            -- this code could be as simple as `colsWrapPat =<< layoutPat p`
            -- if it was not for the following two cases:
            -- \ !x -> x
            -- \ ~x -> x
            -- These make it necessary to special-case an additional separator.
            -- (TODO: we create a BDCols here, but then make it ineffective
            -- by wrapping it in docSeq below. We _could_ add alignments for
            -- stuff like lists-of-lambdas. Nothing terribly important..)
            let
              shouldPrefixSeparator = case unLoc p of
                LazyPat{} -> isFirst
                BangPat{} -> isFirst
                _ -> False
            fixed <- case Seq.viewl $ patternCompactColumns layout of
              p1 Seq.:< pr | shouldPrefixSeparator -> do
                p1' <- docSeq [docSeparator, pure p1]
                pure (p1' Seq.<| pr)
              _ -> pure $ patternCompactColumns layout
            colsWrapPat fixed
          pure (pure compactDocument, patternStructuralDocument layout)
        let patDocs = fst <$> patternLayouts
        bodyDoc <-
          docAddBaseY BrIndentRegular <$> docSharedWrapper layoutExpr' (toL body)
        arrowComments <- commentsAfterLambdaArrow grhsAnnotation body
        commentPlan <- mAsk
        let
          (inlineArrowComments, ownLineArrowComments) = List.partition
            (isInlineComment commentPlan)
            arrowComments
          funcPatternPartLine =
            docCols ColCasePattern
              (patDocs <&> (\p -> docSeq
                [docForceSingleline p, docSeparator]
              ))
          arrowDoc = appendSourceComments
            (docLit $ Text.pack "->")
            inlineArrowComments
          bodyWithComments = case ownLineArrowComments of
            [] -> docNonBottomSpacing bodyDoc
            _ -> docLines
              $ (layoutPatSynComment <$> ownLineArrowComments)
              ++ [docNonBottomSpacing bodyDoc]
          commentedArrowLayout = docSetParSpacing
            $ docAddBaseY BrIndentRegular
            $ docPar
              (docSeq
                [ docLit $ Text.pack "\\"
                , docWrapNode (toL lmatch) $ appSep $ docForceSingleline
                  funcPatternPartLine
                , arrowDoc
                ]
              )
              (docWrapNode (toL lgrhs) bodyWithComments)
        structuralLambda <- if not $ any (Maybe.isJust . snd) patternLayouts
          then pure Nothing
          else do
            selectedPatterns <- patternLayouts `forM` \case
              (compactPattern, Nothing) -> docForceSingleline compactPattern
              (compactPattern, Just structuralPattern) -> docAlt
                [ docForceSingleline compactPattern
                , pure structuralPattern
                ]
            case selectedPatterns of
              [] -> pure Nothing
              firstPattern : remainingPatterns -> fmap Just
                $ docSetParSpacing
                $ docAddBaseY BrIndentRegular
                $ docLines
                $ [ docSeq
                      [ docLit $ Text.pack "\\"
                      , docSeparator
                      , pure firstPattern
                      ]
                  ]
                ++ [ docEnsureIndent BrIndentRegular $ pure patternDoc
                   | patternDoc <- remainingPatterns
                   ]
                ++ [ docEnsureIndent BrIndentRegular
                     $ docSeq
                       [ appSep arrowDoc
                       , docWrapNode (toL lgrhs) bodyWithComments
                       ]
                   ]
        if not $ null arrowComments
          then prependConsumedComments arrowComments $ case structuralLambda of
            Nothing -> commentedArrowLayout
            Just multilineLambda -> docAlt
              [ commentedArrowLayout
              , pure multilineLambda
              ]
          else docAlt $
            [ -- single line
            docSeq
            [ docLit $ Text.pack "\\"
            , docWrapNode (toL lmatch) $ docForceSingleline funcPatternPartLine
            , appSep $ docLit $ Text.pack "->"
            , docWrapNode (toL lgrhs) $ docForceSingleline bodyDoc
            ]
            -- double line
          , docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
            (docSeq
              [ docLit $ Text.pack "\\"
              , docWrapNode (toL lmatch) $ appSep $ docForceSingleline
                funcPatternPartLine
              , docLit $ Text.pack "->"
              ]
            )
            (docWrapNode (toL lgrhs) $ docForceSingleline bodyDoc)
            -- wrapped par spacing
          , docSetParSpacing $ docSeq
            [ docLit $ Text.pack "\\"
            , docWrapNode (toL lmatch) $ docForceSingleline funcPatternPartLine
            , appSep $ docLit $ Text.pack "->"
            , docWrapNode (toL lgrhs) $ docForceParSpacing bodyDoc
            ]
            -- conservative
          , docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
            (docSeq
              [ docLit $ Text.pack "\\"
              , docWrapNode (toL lmatch) $ appSep $ docForceSingleline
                funcPatternPartLine
              , docLit $ Text.pack "->"
              ]
            )
            (docWrapNode (toL lgrhs) $ docNonBottomSpacing bodyDoc)
            ]
            ++ maybe [] (pure . pure) structuralLambda
    HsLam _ _ _ -> unknownNodeError "HsLam too complex" lexpr
    HsApp _ exp1@(L _ HsApp{}) exp2 -> do
      let
        gather
          :: [LHsExpr GhcPs]
          -> LHsExpr GhcPs
          -> (LHsExpr GhcPs, [LHsExpr GhcPs])
        gather list = \case
          L _ (HsApp _ l r) -> gather (r : list) l
          x -> (x, list)
      let (headE, paramEs) = gather [exp2] exp1
      let
        colsOrSequence = case headE of
          L _ (HsVar _ (L _ (Unqual occname))) ->
            docCols (ColApp $ Text.pack $ occNameString occname)
          _ -> docSeq
      headDoc <- docSharedWrapper layoutExpr' (toL headE)
      paramDocs <- docSharedWrapper layoutExpr' `mapM` (map toL paramEs)
      hasComments <- orM
        ( hasAnyCommentsConnected lexpr
        : map (hasAnyCommentsConnected . toL) (headE : paramEs)
        )
      runFilteredAlternative $ do
        -- foo x y
        addAlternativeCond (not hasComments)
          $ colsOrSequence
          $ appSep (docForceSingleline headDoc)
          : spacifyDocs (docForceSingleline <$> paramDocs)
        -- foo x
        --     y
        addAlternativeCond allowFreeIndent $ docSeq
          [ appSep (docForceSingleline headDoc)
          , docSetBaseY
          $ docAddBaseY BrIndentRegular
          $ docLines
          $ docForceSingleline
          <$> paramDocs
          ]
        -- foo
        --   x
        --   y
        addAlternative $ docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
          (docForceSingleline headDoc)
          (docNonBottomSpacing $ docLines paramDocs)
        -- ( multi
        --   line
        --   function
        -- )
        --   x
        --   y
        addAlternative $ docAddBaseY BrIndentRegular $ docPar
          headDoc
          (docNonBottomSpacing $ docLines paramDocs)
    HsApp _ exp1 exp2 -> do
      -- TODO: if expDoc1 is some literal, we may want to create a docCols here.
      expDoc1 <- docSharedWrapper layoutExpr' (toL exp1)
      expDoc2 <- docSharedWrapper layoutExpr' (toL exp2)
      docAlt
        [ -- func arg
          docSeq
          [appSep $ docForceSingleline expDoc1, docForceSingleline expDoc2]
        , -- func argline1
          --   arglines
          -- e.g.
          -- func Abc
          --   { member1 = True
          --   , member2 = 13
          --   }
          docSetParSpacing -- this is most likely superfluous because
                           -- this is a sequence of a one-line and a par-space
                           -- anyways, so it is _always_ par-spaced.
        $ docAddBaseY BrIndentRegular
        $ docSeq
            [appSep $ docForceSingleline expDoc1, docForceParSpacing expDoc2]
        , -- func
          --   arg
          docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
          (docForceSingleline expDoc1)
          (docNonBottomSpacing expDoc2)
        , -- fu
          --   nc
          --   ar
          --     gument
          docAddBaseY BrIndentRegular $ docPar expDoc1 expDoc2
        ]
    HsAppType _ exp1 (HsWC _ ty1) -> do
      t <- docSharedWrapper layoutType (toL ty1)
      e <- docSharedWrapper layoutExpr' (toL exp1)
      docAlt
        [ docSeq
          [ docForceSingleline e
          , docSeparator
          , docLit $ Text.pack "@"
          , docForceSingleline t
          ]
        , docPar e (docSeq [docLit $ Text.pack "@", t])
        ]
    OpApp _ expLeft@(L _ OpApp{}) expOp expRight -> do
      hasNestedComments <- hasAnyCommentsConnected (toL expLeft)
      if hasNestedComments
        then layoutOperatorApplication expLeft expOp expRight
        else layoutFlattenedOperatorApplication expLeft expOp expRight
    OpApp _ expLeft expOp expRight ->
      layoutOperatorApplication expLeft expOp expRight
    NegApp _ op _ -> do
      opDoc <- docSharedWrapper layoutExpr' (toL op)
      docSeq [docLit $ Text.pack "-", opDoc]
    HsPar _ innerExp -> do
      innerExpDoc <- docSharedWrapper layoutExpr' (toL innerExp)
      let attached
            | isBlockLikeExpression $ unLoc innerExp = docPar
                (docSeq [docParenL, docSetIndentLevel innerExpDoc])
                docParenR
            | otherwise = docSetBaseY $ docLines
                [ docCols ColOpPrefix
                  [ docParenL
                  , docAddBaseY (BrIndentSpecial 2) innerExpDoc
                  ]
                , docParenR
                ]
      docDelimitedAlternatives
        ParenthesesDelimiter
        (Text.pack "(")
        (Text.pack ")")
        (Just $ ExactPrintCompat.mkAnnKey lexpr)
        [Just $ ExactPrintCompat.mkAnnKey $ toL innerExp]
        []
        [ ( DelimiterCompact
          , docSeq [docParenL, docForceSingleline innerExpDoc, docParenR]
          )
        , (DelimiterAttached, attached)
        ]
    SectionL _ left op -> do -- (left op) or (left `op`) for alphanumeric
      leftDoc <- docSharedWrapper layoutExpr' (toL left)
      opDoc <- docSharedWrapper layoutExpr' (toL op)
      needsBackticks <- not <$> isSymbolicSectionOp op
      if needsBackticks
        then docSeq [leftDoc, docLit $ Text.pack " `", opDoc, docLit $ Text.pack "`"]
        else docSeq [leftDoc, docLit $ Text.pack " ", opDoc]
    SectionR _ op right -> do -- (op right) or (`op` right) for alphanumeric
      opDoc <- docSharedWrapper layoutExpr' (toL op)
      rightDoc <- docSharedWrapper layoutExpr' (toL right)
      needsBackticks <- not <$> isSymbolicSectionOp op
      if needsBackticks
        then docSeq [docLit $ Text.pack "`", opDoc, docLit $ Text.pack "` ", rightDoc]
        else docSeq [opDoc, docLit $ Text.pack " ", rightDoc]
    ExplicitTuple _ args boxity -> do
      let argExprs = args <&> \arg -> case arg of
            Present _ e -> (L noSrcSpan arg, Just (toL e))
            Missing _ -> (L noSrcSpan arg, Nothing)
      argDocs <- forM argExprs $ docSharedWrapper $ \(arg, exprM) ->
        docWrapNode arg $ maybe docEmpty layoutExpr' exprM
      let argSubExprs = [ toL e | Present _ e <- args ]
      hasComments <-
        orM
          (hasCommentsBetween lexpr AnnOpenP AnnCloseP
          : map hasAnyCommentsBelow argSubExprs
          )
      let
        (openLit, closeLit) = case boxity of
          Boxed -> (docLit $ Text.pack "(", docLit $ Text.pack ")")
          Unboxed -> (docParenHashLSep, docParenHashRSep)
        (kind, openToken, closeToken) = case boxity of
          Boxed -> (ParenthesesDelimiter, Text.pack "(", Text.pack ")")
          Unboxed ->
            (UnboxedParenthesesDelimiter, Text.pack "(#", Text.pack "#)")
        children = args <&> \case
          Present _ expression -> Just $ ExactPrintCompat.mkAnnKey $ toL expression
          Missing _ -> Nothing
        separators = replicate (max 0 $ length argDocs - 1) $ Text.pack ","
        delimited = docDelimitedAlternatives
          kind
          openToken
          closeToken
          (Just $ ExactPrintCompat.mkAnnKey lexpr)
          children
          separators
      case splitFirstLast argDocs of
        FirstLastEmpty -> delimited
          [(DelimiterCompact, docSeq
            [openLit, docNodeAnnKW lexpr (Just AnnOpenP) closeLit]
          )]
        FirstLastSingleton e -> delimited
          [ ( DelimiterCompact
            , docCols
            ColTuple
            [ openLit
            , docNodeAnnKW lexpr (Just AnnOpenP) $ docForceSingleline e
            , closeLit
            ]
            )
          , ( DelimiterAttached
            , docSetBaseY $ docLines
            [ docSeq
              [ openLit
              , docNodeAnnKW lexpr (Just AnnOpenP) $ docForceSingleline e
              ]
            , closeLit
            ]
            )
          ]
        FirstLast e1 ems eN ->
          let
            compact = docCols ColTuple
              $ [docSeq [openLit, docForceSingleline e1]]
              ++ (ems <&> \e -> docSeq [docCommaSep, docForceSingleline e])
              ++ [ docSeq
                     [ docCommaSep
                     , docNodeAnnKW lexpr (Just AnnOpenP)
                       $ docForceSingleline eN
                     , closeLit
                     ]
                 ]
            attached =
              let
                start = docCols ColTuples [appSep openLit, e1]
                linesM = ems <&> \d -> docCols ColTuples [docCommaSep, d]
                lineN = docCols ColTuples
                  [docCommaSep, docNodeAnnKW lexpr (Just AnnOpenP) eN]
              in docSetBaseY
                $ docLines $ [start] ++ linesM ++ [lineN, closeLit]
          in if hasComments
            then delimited [(DelimiterAttached, attached)]
            else docAlt
              [compact, delimited [(DelimiterAttached, attached)]]
    HsCase _ cExp (MG _ lmatches@(L _ [])) -> do
      cExpDoc <- docSharedWrapper layoutExpr' (toL cExp)
      docWrapAnnKey (matchGroupKey lmatches) $ docAlt
        [ docAddBaseY BrIndentRegular $ docSeq
          [ appSep $ docLit $ Text.pack "case"
          , appSep $ docForceSingleline cExpDoc
          , docLit $ Text.pack "of {}"
          ]
        , docPar
          (docAddBaseY BrIndentRegular
          $ docPar (docLit $ Text.pack "case") cExpDoc
          )
          (docLit $ Text.pack "of {}")
        ]
    HsCase _ cExp (MG _ lmatches) -> do
      let matches = unLoc lmatches
      cExpDoc <- docSharedWrapper layoutExpr' (toL cExp)
      binderDoc <- docLit $ Text.pack "->"
      funcPatDocs <-
        docWrapAnnKeyList (matchGroupKey lmatches)
        $ layoutPatternBind [] Nothing binderDoc
        `mapM` matches
      docAlt
        [ docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
          (docSeq
            [ appSep $ docLit $ Text.pack "case"
            , appSep $ docForceSingleline cExpDoc
            , docLit $ Text.pack "of"
            ]
          )
          (docSetBaseAndIndent
          $ docNonBottomSpacing
          $ docLines
          $ return
          <$> funcPatDocs
          )
        , docPar
          (docAddBaseY BrIndentRegular
          $ docPar (docLit $ Text.pack "case") cExpDoc
          )
          (docAddBaseY BrIndentRegular $ docPar
            (docLit $ Text.pack "of")
            (docSetBaseAndIndent
            $ docNonBottomSpacing
            $ docLines
            $ return
            <$> funcPatDocs
            )
          )
        ]
    HsIf _ ifExpr thenExpr elseExpr -> do
      -- Always use normal layout path so then/else prior comments (from EpAnn extraction)
      -- are emitted via docWrapNode. ExactPrint fallback for HsIf was dropping comments.
      ifExprDoc <- docSharedWrapper layoutExpr' (toL ifExpr)
      thenExprDoc <- docSharedWrapper layoutExpr' (toL thenExpr)
      elseExprDoc <- docSharedWrapper layoutExpr' (toL elseExpr)
      hasComments <- hasAnyCommentsBelow lexpr
      let
        maySpecialIndent = case indentPolicy of
          IndentPolicyLeft -> BrIndentRegular
          IndentPolicyMultiple -> BrIndentRegular
          IndentPolicyFree -> BrIndentSpecial 3
      -- TODO: some of the alternatives (especially last and last-but-one)
      -- overlap.
      docSetIndentLevel $ runFilteredAlternative $ do
        -- if _ then _ else _
        addAlternativeCond (not hasComments) $ docSeq
          [ appSep $ docLit $ Text.pack "if"
          , appSep $ docForceSingleline ifExprDoc
          , appSep $ docLit $ Text.pack "then"
          , appSep $ docForceSingleline thenExprDoc
          , appSep $ docLit $ Text.pack "else"
          , docForceSingleline elseExprDoc
          ]
        -- either
        --   if expr
        --   then foo
        --     bar
        --   else foo
        --     bar
        -- or
        --   if expr
        --   then
        --     stuff
        --   else
        --     stuff
        -- note that this has par-spacing
        addAlternative $ docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
          (docSeq
            [ docNodeAnnKW lexpr Nothing $ appSep $ docLit $ Text.pack "if"
            , docNodeAnnKW lexpr (Just AnnIf) $ docForceSingleline ifExprDoc
            ]
          )
          (docLines
            [ docAddBaseY BrIndentRegular
            $ docNodeAnnKW lexpr (Just AnnThen)
            $ docNonBottomSpacing
            $ docAlt
                [ docSeq
                  [ appSep $ docLit $ Text.pack "then"
                  , docForceParSpacing thenExprDoc
                  ]
                , docAddBaseY BrIndentRegular
                  $ docPar (docLit $ Text.pack "then") thenExprDoc
                ]
            , docAddBaseY BrIndentRegular $ docNonBottomSpacing $ docAlt
              [ docSeq
                [ appSep $ docLit $ Text.pack "else"
                , docForceParSpacing elseExprDoc
                ]
              , docAddBaseY BrIndentRegular
                $ docPar (docLit $ Text.pack "else") elseExprDoc
              ]
            ]
          )
        -- either
        --   if multi
        --      line
        --      condition
        --   then foo
        --     bar
        --   else foo
        --     bar
        -- or
        --   if multi
        --      line
        --      condition
        --   then
        --     stuff
        --   else
        --     stuff
        -- note that this does _not_ have par-spacing
        addAlternative $ docAddBaseY BrIndentRegular $ docPar
          (docAddBaseY maySpecialIndent $ docSeq
            [ docNodeAnnKW lexpr Nothing $ appSep $ docLit $ Text.pack "if"
            , docNodeAnnKW lexpr (Just AnnIf) $ ifExprDoc
            ]
          )
          (docLines
            [ docAddBaseY BrIndentRegular
            $ docNodeAnnKW lexpr (Just AnnThen)
            $ docAlt
                [ docSeq
                  [ appSep $ docLit $ Text.pack "then"
                  , docForceParSpacing thenExprDoc
                  ]
                , docAddBaseY BrIndentRegular
                  $ docPar (docLit $ Text.pack "then") thenExprDoc
                ]
            , docAddBaseY BrIndentRegular $ docAlt
              [ docSeq
                [ appSep $ docLit $ Text.pack "else"
                , docForceParSpacing elseExprDoc
                ]
              , docAddBaseY BrIndentRegular
                $ docPar (docLit $ Text.pack "else") elseExprDoc
              ]
            ]
          )
        addAlternative $ docSetBaseY $ docLines
          [ docAddBaseY maySpecialIndent $ docSeq
            [ docNodeAnnKW lexpr Nothing $ appSep $ docLit $ Text.pack "if"
            , docNodeAnnKW lexpr (Just AnnIf) $ ifExprDoc
            ]
          , docNodeAnnKW lexpr (Just AnnThen)
          $ docAddBaseY BrIndentRegular
          $ docPar (docLit $ Text.pack "then") thenExprDoc
          , docAddBaseY BrIndentRegular
            $ docPar (docLit $ Text.pack "else") elseExprDoc
          ]
    HsMultiIf _ cases -> do
      sourceComments <- sourceCommentsWithinNode lexpr
      clauseDocs <- cases `forM` layoutGrhs sourceComments
      binderDoc <- docLit $ Text.pack "->"
      hasComments <- hasAnyCommentsBelow lexpr
      prependConsumedComments (handledClauseComments $ NonEmpty.toList clauseDocs)
        $ docSetParSpacing
        $ docAddBaseY BrIndentRegular
        $ docPar (docLit $ Text.pack "if")
        $ layoutPatternBindFinal Nothing binderDoc Nothing Nothing
          (NonEmpty.toList clauseDocs) Nothing hasComments
    HsLet _ binds exp1 -> do
      expDoc1 <- docSharedWrapper layoutExpr' (toL exp1)
      -- We jump through some ugly hoops here to ensure proper sharing.
      hasComments <- hasAnyCommentsBelow lexpr
      let bindsSpan = localBindsSpan binds
      sourceComments <- sourceCommentsWithinNode lexpr
      mBindDocs <- fmap (fmap pure)
        <$> layoutLocalBindsWithComments sourceComments (L bindsSpan binds)
      let
        ifIndentFreeElse :: a -> a -> a
        ifIndentFreeElse x y = case indentPolicy of
          IndentPolicyLeft -> y
          IndentPolicyMultiple -> y
          IndentPolicyFree -> x
      -- this `docSetBaseAndIndent` might seem out of place (especially the
      -- Indent part; setBase is necessary due to the use of docLines below),
      -- but is here due to ghc-exactprint's DP handling of "let" in
      -- particular.
      -- Just pushing another indentation level is a straightforward approach
      -- to making brittany idempotent, even though the result is non-optimal
      -- if "let" is moved horizontally as part of the transformation, as the
      -- comments before the first let item are moved horizontally with it.
      docSetBaseAndIndent $ case mBindDocs of
        Just [bindDoc] -> runFilteredAlternative $ do
          addAlternativeCond (not hasComments) $ docSeq
            [ appSep $ docLit $ Text.pack "let"
            , docNodeAnnKW lexpr (Just AnnLet) $ appSep $ docForceSingleline
              bindDoc
            , appSep $ docLit $ Text.pack "in"
            , docForceSingleline expDoc1
            ]
          addAlternative $ docLines
            [ docNodeAnnKW lexpr (Just AnnLet) $ runFilteredAlternative $ do
                addAlternativeCond (not hasComments) $ docSeq
                  [ appSep $ docLit $ Text.pack "let"
                  , ifIndentFreeElse docSetBaseAndIndent docForceSingleline
                    $ bindDoc
                  ]
                addAlternative $ docAddBaseY BrIndentRegular $ docPar
                  (docLit $ Text.pack "let")
                  (docSetBaseAndIndent bindDoc)
            , docAlt
              [ docSeq
                [ appSep $ docLit $ Text.pack $ ifIndentFreeElse "in " "in"
                , ifIndentFreeElse
                  docSetBaseAndIndent
                  docForceSingleline
                  expDoc1
                ]
              , docAddBaseY BrIndentRegular
                $ docPar (docLit $ Text.pack "in") (docSetBaseY expDoc1)
              ]
            ]
        Just bindDocs@(_ : _) -> runFilteredAlternative $ do
          --either
          --  let
          --    a = b
          --    c = d
          --  in foo
          --    bar
          --    baz
          --or
          --  let
          --    a = b
          --    c = d
          --  in
          --    fooooooooooooooooooo
          let
            noHangingBinds =
              [ docNonBottomSpacing $ docAddBaseY BrIndentRegular $ docPar
                (docLit $ Text.pack "let")
                (docSetBaseAndIndent $ docLines bindDocs)
              , docSeq
                [ docLit $ Text.pack "in "
                , docAddBaseY BrIndentRegular $ docForceParSpacing expDoc1
                ]
              ]
          addAlternative $ case indentPolicy of
            IndentPolicyLeft -> docLines noHangingBinds
            IndentPolicyMultiple -> docLines noHangingBinds
            IndentPolicyFree -> docLines
              [ docNodeAnnKW lexpr (Just AnnLet) $ docSeq
                [ appSep $ docLit $ Text.pack "let"
                , docSetBaseAndIndent $ docLines bindDocs
                ]
              , docSeq [appSep $ docLit $ Text.pack "in ", docSetBaseY expDoc1]
              ]
          addAlternative $ docLines
            [ docNodeAnnKW lexpr (Just AnnLet)
            $ docAddBaseY BrIndentRegular
            $ docPar
                (docLit $ Text.pack "let")
                (docSetBaseAndIndent $ docLines $ bindDocs)
            , docAddBaseY BrIndentRegular
              $ docPar (docLit $ Text.pack "in") (docSetBaseY $ expDoc1)
            ]
        _ -> docSeq [appSep $ docLit $ Text.pack "let in", expDoc1]
      -- docSeq [appSep $ docLit "let in", expDoc1]
    HsDo _ stmtCtx (L _ stmts) -> case stmtCtx of
      DoExpr _ -> do
        stmtListDoc <- docSharedWrapper layoutStmtList stmts
        docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
          (docLit $ Text.pack "do")
          (docSetBaseAndIndent $ docNonBottomSpacing stmtListDoc)
      MDoExpr _ -> do
        stmtListDoc <- docSharedWrapper layoutStmtList stmts
        docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
          (docLit $ Text.pack "mdo")
          (docSetBaseAndIndent $ docNonBottomSpacing stmtListDoc)
      x
        | case x of
          ListComp -> True
          MonadComp -> True
          _ -> False
        -> do
          let resultStatement = toL $ List.last stmts
          resultComments <- filter
            (sourceCommentPrecedesNode resultStatement)
            <$> sourceCommentsWithinNode lexpr
          stmtDocs <- docSharedWrapper layoutStmt `mapM` (map toL stmts)
          hasComments <- hasAnyCommentsBelow lexpr
          let resultDoc = docNodeAnnKW lexpr (Just AnnOpenS)
                $ List.last stmtDocs
              presentationDocs = resultDoc : List.init stmtDocs
              presentationNodes = resultStatement : fmap toL (List.init stmts)
              separators = take (max 0 $ length presentationDocs - 1)
                $ Text.pack "|" : repeat (Text.pack ",")
              compact = docSeq
                [ docNodeAnnKW lexpr Nothing $ appSep $ docLit $ Text.pack "["
                , appSep $ docForceSingleline resultDoc
                , appSep $ docLit $ Text.pack "|"
                , docSeq
                $ List.intersperse docCommaSep
                $ docForceSingleline
                <$> List.init stmtDocs
                , docLit $ Text.pack " ]"
                ]
              start = case resultComments of
                [] -> docCols ColListComp
                  [ docNodeAnnKW lexpr Nothing $ appSep $ docLit $ Text.pack "["
                  , docSetBaseY resultDoc
                  ]
                firstComment : remainingComments -> docLines
                  $ [ docSeq
                      [ docNodeAnnKW lexpr Nothing
                        $ appSep
                        $ docLit
                        $ Text.pack "["
                      , layoutPatSynComment firstComment
                      ]
                    ]
                  ++ [ docEnsureIndent BrIndentRegular
                        $ layoutPatSynComment sourceComment
                     | sourceComment <- remainingComments
                     ]
                  ++ [docEnsureIndent BrIndentRegular resultDoc]
              (s1 : sM) = List.init stmtDocs
              line1 = docCols ColListComp
                [appSep $ docLit $ Text.pack "|", s1]
              lineM = sM <&> \d -> docCols ColListComp [docCommaSep, d]
              attached = docSetBaseY
                $ docLines $ [start, line1] ++ lineM ++ [docLit $ Text.pack "]"]
              alternatives =
                [(DelimiterCompact, compact) | not hasComments]
                  ++ [(DelimiterAttached, attached)]
          prependConsumedComments resultComments $ docDelimitedAlternatives
            SquareBracketsDelimiter
            (Text.pack "[")
            (Text.pack "]")
            (Just $ ExactPrintCompat.mkAnnKey lexpr)
            (Just . ExactPrintCompat.mkAnnKey <$> presentationNodes)
            separators
            alternatives
      _ -> do
        -- TODO
        unknownNodeError "HsDo{} unknown stmtCtx" lexpr
    ExplicitList _ elems@(_ : _) -> do
      elemDocs <- elems `forM` (docSharedWrapper layoutExpr' . toL)
      hasComments <- hasAnyCommentsBelow lexpr
      let children = Just . ExactPrintCompat.mkAnnKey . toL <$> elems
          separators = replicate (max 0 $ length elems - 1) $ Text.pack ","
          delimited = docDelimitedAlternatives
            SquareBracketsDelimiter
            (Text.pack "[")
            (Text.pack "]")
            (Just $ ExactPrintCompat.mkAnnKey lexpr)
            children
            separators
      case splitFirstLast elemDocs of
        FirstLastEmpty -> docSeq
          [ docLit $ Text.pack "["
          , docNodeAnnKW lexpr (Just AnnOpenS) $ docLit $ Text.pack "]"
          ]
        FirstLastSingleton e -> delimited
          [ ( DelimiterCompact
            , docSeq
            [ docLit $ Text.pack "["
            , docNodeAnnKW lexpr (Just AnnOpenS) $ docForceSingleline e
            , docLit $ Text.pack "]"
            ]
            )
          , ( DelimiterAttached
            , docSetBaseY $ docLines
            [ docSeq
              [ docLit $ Text.pack "["
              , docSeparator
              , docSetBaseY $ docNodeAnnKW lexpr (Just AnnOpenS) e
              ]
            , docLit $ Text.pack "]"
            ]
            )
          ]
        FirstLast e1 ems eN -> delimited
          $ [ ( DelimiterCompact
              , docSeq
              $ [docLit $ Text.pack "["]
              ++ List.intersperse
                   docCommaSep
                   (docForceSingleline
                   <$> (e1 : ems ++ [docNodeAnnKW lexpr (Just AnnOpenS) eN])
                   )
              ++ [docLit $ Text.pack "]"]
              )
            | not hasComments
            ]
          ++ [ ( DelimiterAttached
               , let
                   start = docCols ColList [appSep $ docLit $ Text.pack "[", e1]
                   linesM = ems <&> \d -> docCols ColList [docCommaSep, d]
                   lineN = docCols
                     ColList
                     [docCommaSep, docNodeAnnKW lexpr (Just AnnOpenS) eN]
                 in docSetBaseY
                   $ docLines $ [start] ++ linesM ++ [lineN, docLit $ Text.pack "]"]
               )
             ]
    ExplicitList _ [] -> docDelimitedAlternatives
      SquareBracketsDelimiter
      (Text.pack "[")
      (Text.pack "]")
      (Just $ ExactPrintCompat.mkAnnKey lexpr)
      []
      []
      [(DelimiterCompact, docLit $ Text.pack "[]")]
    RecordCon _ lname fields -> case fields of
      HsRecFields _ fs Nothing -> do
        let nameDoc = docWrapNode (toL lname) $ docLit $ lrdrNameToText (toL lname)
        rFs <-
          (map toL fs) `forM` \lfield@(L _ (HsFieldBind _ (L _ fieldOcc) rFExpr pun)) -> do
            let FieldOcc _ lnameF = fieldOcc
            rFExpDoc <- if pun
              then return Nothing
              else Just <$> docSharedWrapper layoutExpr' (toL rFExpr)
            return $ ( lfield
                     , lrdrNameToText (toL lnameF)
                     , (,) (toL rFExpr) <$> rFExpDoc
                     )
        recordExpression False indentPolicy lexpr nameDoc rFs
      HsRecFields _ [] (Just (L _ (RecFieldsDotDot 0))) -> do
        let t = lrdrNameToText (toL lname)
        docWrapNode (toL lname) $ docLit $ t <> Text.pack " { .. }"
      HsRecFields _ fs@(_ : _) (Just (L _ (RecFieldsDotDot dotdoti))) | dotdoti == length fs -> do
        let nameDoc = docWrapNode (toL lname) $ docLit $ lrdrNameToText (toL lname)
        fieldDocs <-
          (map toL fs) `forM` \fieldl@(L _ (HsFieldBind _ (L _ fieldOcc) fExpr pun)) -> do
            let FieldOcc _ lnameF = fieldOcc
            fExpDoc <- if pun
              then return Nothing
              else Just <$> docSharedWrapper layoutExpr' (toL fExpr)
            return
              ( fieldl
              , lrdrNameToText (toL lnameF)
              , (,) (toL fExpr) <$> fExpDoc
              )
        recordExpression True indentPolicy lexpr nameDoc fieldDocs
      _ -> unknownNodeError "RecordCon with puns" lexpr
    RecordUpd _ rExpr (RegularRecUpdFields _ recUpdFields) -> do
      rExprDoc <- docSharedWrapper layoutExpr' (toL rExpr)
      rFs <-
        (map toL recUpdFields) `forM` \lfield@(L _ (HsFieldBind _ (L _ ambName) rFExpr pun)) -> do
          rFExpDoc <- if pun
            then return Nothing
            else Just <$> docSharedWrapper layoutExpr' (toL rFExpr)
          ambNameRes <- case ambName of
            FieldOcc _ n -> return (lrdrNameToText (toL n))
            _ -> return (Text.pack "?")
          return (lfield, ambNameRes, (,) (toL rFExpr) <$> rFExpDoc)
      recordExpression False indentPolicy lexpr rExprDoc rFs
    RecordUpd _ _ (OverloadedRecUpdFields _ _) -> do
      briDocByExactInlineOnly ExpressionFallback lexpr
    ExprWithTySig _ exp1 sigWc -> case sigWc of
      HsWC _ body -> case unLoc body of
        HsSig _ _ typ1 -> do
          expDoc <- docSharedWrapper layoutExpr' (toL exp1)
          typDoc <- docSharedWrapper layoutType (toL typ1)
          docSeq [appSep expDoc, appSep $ docLit $ Text.pack "::", typDoc]
        _ -> briDocByExactInlineOnly ExpressionFallback lexpr
      _ -> briDocByExactInlineOnly ExpressionFallback lexpr
    ArithSeq _ Nothing info -> case info of
      From e1 -> do
        e1Doc <- docSharedWrapper layoutExpr' (toL e1)
        docDelimitedAlternatives
          SquareBracketsDelimiter (Text.pack "[") (Text.pack "]")
          (Just $ ExactPrintCompat.mkAnnKey lexpr)
          [Just $ ExactPrintCompat.mkAnnKey $ toL e1, Nothing]
          [Text.pack ".."]
          [(DelimiterCompact, docSeq
            [ docLit $ Text.pack "["
            , appSep $ docForceSingleline e1Doc
            , docLit $ Text.pack "..]"
            ])]
      FromThen e1 e2 -> do
        e1Doc <- docSharedWrapper layoutExpr' (toL e1)
        e2Doc <- docSharedWrapper layoutExpr' (toL e2)
        docDelimitedAlternatives
          SquareBracketsDelimiter (Text.pack "[") (Text.pack "]")
          (Just $ ExactPrintCompat.mkAnnKey lexpr)
          [ Just $ ExactPrintCompat.mkAnnKey $ toL e1
          , Just $ ExactPrintCompat.mkAnnKey $ toL e2
          , Nothing
          ]
          (Text.pack <$> [",", ".."])
          [(DelimiterCompact, docSeq
            [ docLit $ Text.pack "["
            , docForceSingleline e1Doc
            , appSep $ docLit $ Text.pack ","
            , appSep $ docForceSingleline e2Doc
            , docLit $ Text.pack "..]"
            ])]
      FromTo e1 eN -> do
        e1Doc <- docSharedWrapper layoutExpr' (toL e1)
        eNDoc <- docSharedWrapper layoutExpr' (toL eN)
        docDelimitedAlternatives
          SquareBracketsDelimiter (Text.pack "[") (Text.pack "]")
          (Just $ ExactPrintCompat.mkAnnKey lexpr)
          (Just . ExactPrintCompat.mkAnnKey . toL <$> [e1, eN])
          [Text.pack ".."]
          [(DelimiterCompact, docSeq
            [ docLit $ Text.pack "["
            , appSep $ docForceSingleline e1Doc
            , appSep $ docLit $ Text.pack ".."
            , docForceSingleline eNDoc
            , docLit $ Text.pack "]"
            ])]
      FromThenTo e1 e2 eN -> do
        e1Doc <- docSharedWrapper layoutExpr' (toL e1)
        e2Doc <- docSharedWrapper layoutExpr' (toL e2)
        eNDoc <- docSharedWrapper layoutExpr' (toL eN)
        docDelimitedAlternatives
          SquareBracketsDelimiter (Text.pack "[") (Text.pack "]")
          (Just $ ExactPrintCompat.mkAnnKey lexpr)
          (Just . ExactPrintCompat.mkAnnKey . toL <$> [e1, e2, eN])
          (Text.pack <$> [",", ".."])
          [(DelimiterCompact, docSeq
            [ docLit $ Text.pack "["
            , docForceSingleline e1Doc
            , appSep $ docLit $ Text.pack ","
            , appSep $ docForceSingleline e2Doc
            , appSep $ docLit $ Text.pack ".."
            , docForceSingleline eNDoc
            , docLit $ Text.pack "]"
            ])]
    ArithSeq{} -> briDocByExactInlineOnly ExpressionFallback lexpr
    HsTypedBracket{} -> layoutOpaqueExpression TemplateHaskellQuote lexpr
    HsUntypedBracket{} -> layoutOpaqueExpression TemplateHaskellQuote lexpr
    HsTypedSplice{} -> layoutOpaqueExpression TemplateHaskellSplice lexpr
    HsUntypedSplice _ splice ->
      layoutOpaqueExpression (untypedSpliceFamily splice) lexpr
    HsHole{} -> docLit $ Text.pack "_"
    HsProc{} -> briDocByExactInlineOnly ExpressionFallback lexpr
    HsStatic _ innerExpr -> do
      innerDoc <- docSharedWrapper layoutExpr' (toL innerExpr)
      docSeq
        [ appSep $ docLit $ Text.pack "static"
        , innerDoc
        ]
    HsGetField _ innerExpr (L _ (DotFieldOcc _ (L _ (FieldLabelString fieldFs)))) -> do
      innerDoc <- docSharedWrapper layoutExpr' (toL innerExpr)
      let fieldStr = FastString.unpackFS fieldFs
      docSeq
        [ innerDoc
        , docLit $ Text.pack ("." ++ fieldStr)
        ]
    HsProjection _ flds -> do
      let fieldStrs = NonEmpty.toList flds <&> \(DotFieldOcc _ (L _ (FieldLabelString fs))) ->
                        FastString.unpackFS fs
      docLit $ Text.pack ("(." ++ List.intercalate "." fieldStrs ++ ")")
    HsPragE{} -> briDocByExactInlineOnly ExpressionFallback lexpr
    HsEmbTy _ wc -> do
      typeDoc <- docSharedWrapper layoutType (toL $ hswc_body wc)
      docSeq
        [ appSep $ docLit $ Text.pack "type"
        , typeDoc
        ]
    XExpr _ -> briDocByExactInlineOnly ExpressionFallback lexpr
    _ -> briDocByExactInlineOnly ExpressionFallback lexpr

recordExpression
  :: (Data.Data.Data lExpr, Data.Data.Data name)
  => Bool
  -> IndentPolicy
  -> GenLocated SrcSpan lExpr
  -> ToBriDocM BriDocNumbered
  -> [ ( GenLocated SrcSpan name
       , Text
       , Maybe (Located (HsExpr GhcPs), ToBriDocM BriDocNumbered)
       )
     ]
  -> ToBriDocM BriDocNumbered
recordExpression dotdot indentPolicy lexpr nameDoc fields =
  docDelimitedAlternatives
    CurlyBracesDelimiter
    (Text.pack "{")
    (Text.pack "}")
    (Just $ ExactPrintCompat.mkAnnKey lexpr)
    children
    (replicate (max 0 $ length children - 1) $ Text.pack ",")
    [(DelimiterAttached, recordExpressionDocument
      dotdot indentPolicy lexpr nameDoc fields)]
 where
  fieldKeys = [Just $ ExactPrintCompat.mkAnnKey field | (field, _, _) <- fields]
  children = fieldKeys ++ [Nothing | dotdot]

recordExpressionDocument
  :: (Data.Data.Data lExpr, Data.Data.Data name)
  => Bool
  -> IndentPolicy
  -> GenLocated SrcSpan lExpr
  -> ToBriDocM BriDocNumbered
  -> [ ( GenLocated SrcSpan name
       , Text
       , Maybe (Located (HsExpr GhcPs), ToBriDocM BriDocNumbered)
       )
     ]
  -> ToBriDocM BriDocNumbered
recordExpressionDocument False _ lexpr nameDoc [] = docSeq
  [ docNodeAnnKW lexpr (Just AnnOpenC)
    $ docSeq [nameDoc, docLit $ Text.pack "{"]
  , docLit $ Text.pack "}"
  ]
recordExpressionDocument True _ lexpr nameDoc [] = docSeq -- this case might still be incomplete, and is probably not used
         -- atm anyway.
  [ docNodeAnnKW lexpr (Just AnnOpenC)
    $ docSeq [nameDoc, docLit $ Text.pack "{"]
  , docLit $ Text.pack " .. }"
  ]
recordExpressionDocument dotdot indentPolicy lexpr nameDoc rFs@(rF1 : rFr) = do
  layoutColumns <- mAsk <&> _conf_layout .> _lconfig_cols .> confUnpack
  sourceComments <- sourceCommentsWithinNode lexpr
  let (rF1f, rF1n, rF1e) = rF1
      useHangingLayout = length rFs < 4
      -- Leave enough width for nested values instead of spending it on alignment.
      hangingColumnLimit = layoutColumns * 3 `div` 5
      leadingRecordComments = filter
        (sourceCommentPrecedesNode rF1f)
        sourceComments
      fieldComments lfield fieldExpression = filter
        (\sourceComment ->
          sourceCommentWithinNodeSpan lfield sourceComment
            && sourceCommentPrecedesNode fieldExpression sourceComment
        )
        sourceComments
      handledComments = leadingRecordComments ++ List.concat
        [ maybe [] (fieldComments lfield . fst) fieldExpression
        | (lfield, _, fieldExpression) <- rFs
        ]
  prependConsumedComments handledComments $ runFilteredAlternative $ do
    -- container { fieldA = blub, fieldB = blub }
    addAlternativeCond (null handledComments) $ docSeq
      [ docNodeAnnKW lexpr Nothing $ appSep $ docForceSingleline nameDoc
      , appSep $ docLit $ Text.pack "{"
      , docSeq $ List.intersperse docCommaSep $ rFs <&> \case
        (lfield, fieldStr, Just (_, fieldDoc)) -> docWrapNode lfield $ docSeq
          [ appSep $ docLit fieldStr
          , appSep $ docLit $ Text.pack "="
          , docForceSingleline fieldDoc
          ]
        (lfield, fieldStr, Nothing) -> docWrapNode lfield $ docLit fieldStr
      , if dotdot
        then docSeq [docCommaSep, docLit $ Text.pack "..", docSeparator]
        else docSeparator
      , docLit $ Text.pack "}"
      ]
    -- hanging single-line fields
    -- container { fieldA = blub
    --           , fieldB = blub
    --           }
    addAlternativeCond
      ( indentPolicy == IndentPolicyFree
        && useHangingLayout
        && null handledComments
      )
      $ docColumnsLimit hangingColumnLimit
      $ docSeq
      [ docNodeAnnKW lexpr Nothing $ docForceSingleline $ appSep nameDoc
      , docSetBaseY
      $ docLines
      $ let
          line1 = docCols
            ColRec
            [ appSep $ docLit $ Text.pack "{"
            , docWrapNodePrior rF1f $ appSep $ docLit rF1n
            , case rF1e of
              Just (_, x) -> docWrapNodeRest rF1f $ docSeq
                [appSep $ docLit $ Text.pack "=", docForceSingleline x]
              Nothing -> docEmpty
            ]
          lineR = rFr <&> \(lfield, fText, fDoc) ->
            docWrapNode lfield $ docCols
              ColRec
              [ docCommaSep
              , appSep $ docLit fText
              , case fDoc of
                Just (_, x) ->
                  docSeq [appSep $ docLit $ Text.pack "=", docForceSingleline x]
                Nothing -> docEmpty
              ]
          dotdotLine = if dotdot
            then docCols
              ColRec
              [ docNodeAnnKW lexpr (Just AnnOpenC) docCommaSep
              , docNodeAnnKW lexpr (Just AnnDotdot) $ docLit $ Text.pack ".."
              ]
            else docNodeAnnKW lexpr (Just AnnOpenC) docEmpty
          lineN = docLit $ Text.pack "}"
        in [line1] ++ lineR ++ [dotdotLine, lineN]
      ]
    -- non-hanging with expressions placed to the right of the names
    -- container
    -- { fieldA = blub
    -- , fieldB = potentially
    --     multiline
    -- }
    addAlternative
      $ docForceParSpacing
      $ docAddBaseY BrIndentRegular
      $ docPar
      (docNodeAnnKW lexpr Nothing nameDoc)
      (docNonBottomSpacing
      $ docLines
      $ let
          line1 = docCols
            ColRec
            [ appSep $ docLit $ Text.pack "{"
            , docWrapNodePrior rF1f $ appSep $ docLit rF1n
            , docWrapNodeRest rF1f $ case rF1e of
              Just (fieldExpression, x) -> recordFieldRhs indentPolicy
                (fieldComments rF1f fieldExpression) fieldExpression x
              Nothing -> docEmpty
            ]
          lineR = rFr <&> \(lfield, fText, fDoc) ->
            docWrapNode lfield $ docCols
              ColRec
              [ docCommaSep
              , appSep $ docLit fText
              , case fDoc of
                Just (fieldExpression, x) -> recordFieldRhs indentPolicy
                  (fieldComments lfield fieldExpression) fieldExpression x
                Nothing -> docEmpty
              ]
          dotdotLine = if dotdot
            then docCols
              ColRec
              [ docNodeAnnKW lexpr (Just AnnOpenC) docCommaSep
              , docNodeAnnKW lexpr (Just AnnDotdot) $ docLit $ Text.pack ".."
              ]
            else docNodeAnnKW lexpr (Just AnnOpenC) docEmpty
          lineN = docLit $ Text.pack "}"
          firstLines = case leadingRecordComments of
            [] -> [line1]
            _ ->
              [ layoutPatSynComment sourceComment
              | sourceComment <- leadingRecordComments
              ]
              ++ [line1]
        in firstLines ++ lineR ++ [dotdotLine, lineN]
      )

recordFieldRhs
  :: IndentPolicy
  -> [SourceComment]
  -> Located (HsExpr GhcPs)
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
recordFieldRhs indentPolicy sourceComments fieldExpression fieldDoc = do
  compactApplication <- layoutCompactApplication fieldExpression
  let indentedFieldDoc = docEnsureIndent
        (BrIndentSpecial fieldRowPrefixWidth)
        $ docEnsureIndent BrIndentRegular
        $ docSetIndentLevel
        $ case compactApplication of
          Just compact -> docAlt [pure compact, fieldDoc]
          Nothing -> fieldDoc
  runFilteredAlternative $ do
    addAlternativeCond
      (indentPolicy == IndentPolicyFree && null sourceComments) $ docSeq
      [ appSep $ docLit $ Text.pack "="
      , docSetBaseY $ docForceSingleline fieldDoc
      ]
    addAlternativeCond (null sourceComments) $ docSeq
      [ appSep $ docLit $ Text.pack "="
      , docForceSingleline fieldDoc
      ]
    case compactApplication of
      Just compact -> addAlternativeCond (null sourceComments) $ docSeq
        [ appSep $ docLit $ Text.pack "="
        , pure compact
        ]
      Nothing -> pure ()
    addAlternative $ docPar
      (docLit $ Text.pack "=")
      (case sourceComments of
        [] -> indentedFieldDoc
        _ -> docLines
          $ [ docEnsureIndent (BrIndentSpecial commentRowPrefixWidth)
              $ docEnsureIndent BrIndentRegular
              $ layoutPatSynComment sourceComment
            | sourceComment <- sourceComments
            ]
          ++ [indentedFieldDoc]
      )
 where
 -- Structural field rows prefix the field name with either "{ " or ", ".
  fieldRowPrefixWidth = 2
  commentRowPrefixWidth = fieldRowPrefixWidth - 1

layoutCompactApplication
  :: Located (HsExpr GhcPs) -> ToBriDocM (Maybe BriDocNumbered)
layoutCompactApplication locatedExpression@(L _ expression) = case expression of
  HsApp _ left right -> do
    let (headExpression, arguments) = gather [right] left
    headDoc <- docSharedWrapper layoutExpr' (toL headExpression)
    argumentDocs <- mapM (docSharedWrapper layoutExpr' . toL) arguments
    Just <$> docWrapNode locatedExpression
      (docForceSingleline
        $ docSeq
        $ appSep (docForceSingleline headDoc)
        : spacifyDocs (docForceSingleline <$> argumentDocs)
      )
  _ -> pure Nothing
 where
  gather
    :: [LHsExpr GhcPs]
    -> LHsExpr GhcPs
    -> (LHsExpr GhcPs, [LHsExpr GhcPs])
  gather arguments = \case
    L _ (HsApp _ left right) -> gather (right : arguments) left
    headExpression -> (headExpression, arguments)

litBriDoc :: HsLit GhcPs -> BriDocFInt
litBriDoc = \case
  HsChar (SourceText t) _c -> BDFLit $ Text.pack (FastString.unpackFS t)
  HsCharPrim (SourceText t) _c -> BDFLit $ Text.pack (FastString.unpackFS t)
  HsString (SourceText t) _fastString -> BDFLit $ Text.pack (FastString.unpackFS t)
  HsStringPrim (SourceText t) _byteString -> BDFLit $ Text.pack (FastString.unpackFS t)
  HsInt _ il -> sourceTextLit (il_text il) (show (il_value il))
  HsIntPrim source i -> sourceTextLit source (show i ++ "#")
  HsWordPrim source i -> sourceTextLit source (show i ++ "##")
  HsInt8Prim source i -> sourceTextLit source (show i ++ "#Int8")
  HsInt16Prim source i -> sourceTextLit source (show i ++ "#Int16")
  HsInt32Prim source i -> sourceTextLit source (show i ++ "#Int32")
  HsInt64Prim source i -> sourceTextLit source (show i ++ "#Int64")
  HsWord8Prim source i -> sourceTextLit source (show i ++ "#Word8")
  HsWord16Prim source i -> sourceTextLit source (show i ++ "#Word16")
  HsWord32Prim source i -> sourceTextLit source (show i ++ "#Word32")
  HsWord64Prim source i -> sourceTextLit source (show i ++ "#Word64")
  HsFloatPrim _ fl -> sourceTextLitWithSuffix (fl_text fl) "#"
  HsDoublePrim _ fl -> sourceTextLitWithSuffix (fl_text fl) "##"
  HsMultilineString (SourceText t) _fs -> BDFLit $ Text.pack (FastString.unpackFS t)
  HsMultilineString NoSourceText fs -> BDFLit $ Text.pack (FastString.unpackFS fs)
  _ -> error "litBriDoc: literal with no SourceText"

sourceTextLit :: SourceText -> String -> BriDocFInt
sourceTextLit (SourceText fs) _ = BDFLit $ Text.pack (FastString.unpackFS fs)
sourceTextLit NoSourceText fallback = BDFLit $ Text.pack fallback

sourceTextLitWithSuffix :: SourceText -> String -> BriDocFInt
sourceTextLitWithSuffix (SourceText fs) suffix = BDFLit
  $ Text.pack (FastString.unpackFS fs ++ suffix)
sourceTextLitWithSuffix NoSourceText suffix = BDFLit $ Text.pack suffix

overLitValBriDoc :: OverLitVal -> BriDocFInt
overLitValBriDoc = \case
  HsIntegral il -> sourceTextLit (il_text il) (show (il_value il))
  HsFractional fl -> sourceTextLit (fl_text fl) ""
  HsIsString st _ -> sourceTextLit st ""
  _ -> error "overLitValBriDoc: literal with no SourceText"
