{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Expr where

import qualified Data.Data
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import GHC (GenLocated(L), RdrName(..), SrcSpan, unLoc)
import GHC.Types.Name.Reader (RdrName(Exact))
import GHC.Types.SrcLoc (noSrcSpan)
import GHC.Parser.Annotation (EpAnn(..), EpaLocation(..))
import Language.Haskell.Brittany.Internal.ExactPrintCompat (AnnKeywordId(..))
import qualified GHC.Data.FastString as FastString
import GHC.Hs
import GHC.Hs.Type (FieldOcc(..), HsSigType(HsSig), HsWildCardBndrs(HsWC))
import GHC.Types.SourceText (FractionalLit(..), IntegralLit(..), SourceText(..))
import Language.Haskell.Brittany.Internal.Layouters.IE (toL)
import qualified Data.List.NonEmpty as NonEmpty
import qualified GHC.OldList as List
import GHC.Types.Basic
import GHC.Types.Name
import GHC.Types.Name.Occurrence (occNameString)
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.Decl
import Language.Haskell.Brittany.Internal.Layouters.Pattern
import Language.Haskell.Brittany.Internal.Layouters.Stmt
import Language.Haskell.Brittany.Internal.Layouters.Type
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Utils
import qualified Language.Haskell.GHC.ExactPrint as ExactPrint



-- | Extract the SrcSpan from HsLocalBinds, using the EpAnn anchor if available.
localBindsSpan :: HsLocalBindsLR GhcPs GhcPs -> SrcSpan
localBindsSpan (HsValBinds (EpAnn anc _ _) _) = case anc of
  EpaSpan ss -> ss
  _ -> noSrcSpan
localBindsSpan _ = noSrcSpan

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

layoutExpr :: ToBriDoc HsExpr
layoutExpr lexpr = layoutExpr' (toL lexpr)

layoutExpr' :: ToBriDoc HsExpr
layoutExpr' lexpr@(L _ expr) = do
  indentPolicy <- mAsk <&> _conf_layout .> _lconfig_indentPolicy .> confUnpack
  let allowFreeIndent = indentPolicy == IndentPolicyFree
  docWrapNode lexpr $ case expr of
    HsVar _ vname -> do
      t <- lrdrNameToTextAnn (toL vname)
      -- GHC 9.14: apply paren adornment from NameAnn for operators in
      -- expression position (e.g., (:) → "(:)"). Skip for names that are
      -- already complete (like "()" or "[]"), and skip backticks since callers
      -- (SectionL, SectionR, OpApp) handle those.
      let t' = if t == Text.pack "()" || t == Text.pack "[]"
               then t
               else applyNameAdornmentParensOnly vname t
      docLit t'
    XExpr _ -> briDocByExactInlineOnly "XExpr" lexpr
    HsOverLabel _ name ->
      let label = FastString.unpackFS name in docLit . Text.pack $ '#' : label
    HsIPVar _ext (HsIPName name) ->
      let label = FastString.unpackFS name in docLit . Text.pack $ '?' : label
    HsOverLit _ olit -> do
      allocateNode $ overLitValBriDoc $ ol_val olit
    HsLit _ lit -> do
      allocateNode $ litBriDoc lit
    HsLam _ _ (MG _ lmatches)
      | [lmatch@(L _ match)] <- unLoc lmatches
      , pats <- unLoc (m_pats match)
      , GRHSs _ grhssNE llocals <- m_grhss match
      , [lgrhs] <- NonEmpty.toList grhssNE
      , EmptyLocalBinds{} <- llocals
      , GRHS _ [] body <- unLoc lgrhs
      -> do
        patDocs <- zip (True : repeat False) pats `forM` \(isFirst, p) ->
          fmap return $ do
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
            patDocSeq <- layoutPat p
            fixed <- case Seq.viewl patDocSeq of
              p1 Seq.:< pr | shouldPrefixSeparator -> do
                p1' <- docSeq [docSeparator, pure p1]
                pure (p1' Seq.<| pr)
              _ -> pure patDocSeq
            colsWrapPat fixed
        bodyDoc <-
          docAddBaseY BrIndentRegular <$> docSharedWrapper layoutExpr' (toL body)
        let
          funcPatternPartLine = docCols
            ColCasePattern
            (patDocs <&> (\p -> docSeq [docForceSingleline p, docSeparator]))
        docAlt
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
    HsLam _ _ _ -> unknownNodeError "HsLam too complex" lexpr
    HsLam _ LamCase (MG _ lmatches) | null (unLoc lmatches) -> do
      docSetParSpacing
        $ docAddBaseY BrIndentRegular
        $ (docLit $ Text.pack "\\case {}")
    HsLam _ LamCase (MG _ lmatches) -> do
      let matches = unLoc lmatches
      binderDoc <- docLit $ Text.pack "->"
      funcPatDocs <-
        docWrapNode (toL lmatches)
        $ layoutPatternBind Nothing binderDoc
        `mapM` matches
      docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
        (docLit $ Text.pack "\\case")
        (docSetBaseAndIndent
        $ docNonBottomSpacing
        $ docLines
        $ return
        <$> funcPatDocs
        )
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
      hasComments <- hasAnyCommentsConnected (toL exp2)
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
      let
        gather
          :: [(LHsExpr GhcPs, LHsExpr GhcPs)]
          -> LHsExpr GhcPs
          -> (LHsExpr GhcPs, [(LHsExpr GhcPs, LHsExpr GhcPs)])
        gather opExprList = \case
          (L _ (OpApp _ l1 op1 r1)) -> gather ((op1, r1) : opExprList) l1
          final -> (final, opExprList)
        (leftOperand, appList) = gather [] expLeft
      leftOperandDoc <- docSharedWrapper layoutExpr' (toL leftOperand)
      -- GHC 9.14: wrap alphanumeric infix ops with backticks
      appListDocs <- appList `forM` \(x, y) -> do
        isSymX <- isSymbolicSectionOp x
        xD <- docSharedWrapper layoutExpr' (toL x)
        yD <- docSharedWrapper layoutExpr' (toL y)
        let xD' = if isSymX then xD
              else docSeq [docLit $ Text.pack "`", xD, docLit $ Text.pack "`"]
        pure (xD', yD)
      isSymLast <- isSymbolicSectionOp expOp
      opLastDoc' <- docSharedWrapper layoutExpr' (toL expOp)
      let opLastDoc = if isSymLast then opLastDoc'
            else docSeq [docLit $ Text.pack "`", opLastDoc', docLit $ Text.pack "`"]
      expLastDoc <- docSharedWrapper layoutExpr' (toL expRight)
      allowSinglelinePar <- do
        hasComLeft <- hasAnyCommentsConnected (toL expLeft)
        hasComOp <- hasAnyCommentsConnected (toL expOp)
        pure $ not hasComLeft && not hasComOp
      let
        allowPar = case (expOp, expRight) of
          (L _ (HsVar _ (L _ (Unqual occname))), _)
            | occNameString occname == "$" -> True
          (_, L _ (HsApp _ _ (L _ HsVar{}))) -> False
          _ -> True
      runFilteredAlternative $ do
        -- > one + two + three
        -- or
        -- > one + two + case x of
        -- >   _ -> three
        addAlternativeCond allowSinglelinePar $ docSeq
          [ appSep $ docForceSingleline leftOperandDoc
          , docSeq $ appListDocs <&> \(od, ed) -> docSeq
            [appSep $ docForceSingleline od, appSep $ docForceSingleline ed]
          , appSep $ docForceSingleline opLastDoc
          , (if allowPar then docForceParSpacing else docForceSingleline)
            expLastDoc
          ]
        -- this case rather leads to some unfortunate layouting than to anything
        -- useful; disabling for now. (it interfers with cols stuff.)
        -- addAlternative
        --   $ docSetBaseY
        --   $ docPar
        --     leftOperandDoc
        --     ( docLines
        --      $ (appListDocs <&> \(od, ed) -> docCols ColOpPrefix [appSep od, docSetBaseY ed])
        --       ++ [docCols ColOpPrefix [appSep opLastDoc, docSetBaseY expLastDoc]]
        --     )
        -- > one
        -- >   + two
        -- >   + three
        addAlternative $ docPar
          leftOperandDoc
          (docLines
          $ (appListDocs <&> \(od, ed) ->
              docCols ColOpPrefix [appSep od, docSetBaseY ed]
            )
          ++ [docCols ColOpPrefix [appSep opLastDoc, docSetBaseY expLastDoc]]
          )
    OpApp _ expLeft expOp expRight -> do
      expDocLeft <- docSharedWrapper layoutExpr' (toL expLeft)
      expDocOp <- docSharedWrapper layoutExpr' (toL expOp)
      -- GHC 9.14: wrap alphanumeric infix ops with backticks
      isSymOp <- isSymbolicSectionOp expOp
      let expDocOp' = if isSymOp then expDocOp
            else docSeq [docLit $ Text.pack "`", expDocOp, docLit $ Text.pack "`"]
      expDocRight <- docSharedWrapper layoutExpr' (toL expRight)
      let
        allowPar = case (expOp, expRight) of
          (L _ (HsVar _ (L _ (Unqual occname))), _)
            | occNameString occname == "$" -> True
          (_, L _ (HsApp _ _ (L _ HsVar{}))) -> False
          _ -> True
      let
        leftIsDoBlock = case expLeft of
          L _ HsDo{} -> True
          _ -> False
      runFilteredAlternative $ do
        -- one-line
        addAlternative $ docSeq
          [ appSep $ docForceSingleline expDocLeft
          , appSep $ docForceSingleline expDocOp'
          , docForceSingleline expDocRight
          ]
        -- two-line
        addAlternative $ do
          let
            expDocOpAndRight = docForceSingleline $ docCols
              ColOpPrefix
              [appSep $ expDocOp', docSetBaseY expDocRight]
          if leftIsDoBlock
            then docLines [expDocLeft, expDocOpAndRight]
            else docAddBaseY BrIndentRegular
              $ docPar expDocLeft expDocOpAndRight
        -- one-line + par
        addAlternativeCond allowPar $ docSeq
          [ appSep $ docForceSingleline expDocLeft
          , appSep $ docForceSingleline expDocOp'
          , docForceParSpacing expDocRight
          ]
        -- more lines
        addAlternative $ do
          let
            expDocOpAndRight =
              docCols ColOpPrefix [appSep expDocOp', docSetBaseY expDocRight]
          if leftIsDoBlock
            then docLines [expDocLeft, expDocOpAndRight]
            else docAddBaseY BrIndentRegular
              $ docPar expDocLeft expDocOpAndRight
    NegApp _ op _ -> do
      opDoc <- docSharedWrapper layoutExpr' (toL op)
      docSeq [docLit $ Text.pack "-", opDoc]
    HsPar _ innerExp -> do
      innerExpDoc <- docSharedWrapper (docWrapNode lexpr . layoutExpr') (toL innerExp)
      docAlt
        [ docSeq
          [ docLit $ Text.pack "("
          , docForceSingleline innerExpDoc
          , docLit $ Text.pack ")"
          ]
        , docSetBaseY $ docLines
          [ docCols
            ColOpPrefix
            [ docLit $ Text.pack "("
            , docAddBaseY (BrIndentSpecial 2) innerExpDoc
            ]
          , docLit $ Text.pack ")"
          ]
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
      hasComments <-
        orM
          (hasCommentsBetween lexpr AnnOpenP AnnCloseP
          : map (hasAnyCommentsBelow . L noSrcSpan) args
          )
      let
        (openLit, closeLit) = case boxity of
          Boxed -> (docLit $ Text.pack "(", docLit $ Text.pack ")")
          Unboxed -> (docParenHashLSep, docParenHashRSep)
      case splitFirstLast argDocs of
        FirstLastEmpty ->
          docSeq [openLit, docNodeAnnKW lexpr (Just AnnOpenP) closeLit]
        FirstLastSingleton e -> docAlt
          [ docCols
            ColTuple
            [ openLit
            , docNodeAnnKW lexpr (Just AnnOpenP) $ docForceSingleline e
            , closeLit
            ]
          , docSetBaseY $ docLines
            [ docSeq
              [ openLit
              , docNodeAnnKW lexpr (Just AnnOpenP) $ docForceSingleline e
              ]
            , closeLit
            ]
          ]
        FirstLast e1 ems eN -> runFilteredAlternative $ do
          addAlternativeCond (not hasComments)
            $ docCols ColTuple
            $ [docSeq [openLit, docForceSingleline e1]]
            ++ (ems <&> \e -> docSeq [docCommaSep, docForceSingleline e])
            ++ [ docSeq
                   [ docCommaSep
                   , docNodeAnnKW lexpr (Just AnnOpenP) (docForceSingleline eN)
                   , closeLit
                   ]
               ]
          addAlternative
            $ let
                start = docCols ColTuples [appSep openLit, e1]
                linesM = ems <&> \d -> docCols ColTuples [docCommaSep, d]
                lineN = docCols
                  ColTuples
                  [docCommaSep, docNodeAnnKW lexpr (Just AnnOpenP) eN]
                end = closeLit
              in docSetBaseY $ docLines $ [start] ++ linesM ++ [lineN, end]
    HsCase _ cExp (MG _ (L _ [])) -> do
      cExpDoc <- docSharedWrapper layoutExpr' (toL cExp)
      docAlt
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
        docWrapNode (toL lmatches)
        $ layoutPatternBind Nothing binderDoc
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
      clauseDocs <- cases `forM` layoutGrhs
      binderDoc <- docLit $ Text.pack "->"
      hasComments <- hasAnyCommentsBelow lexpr
      docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
        (docLit $ Text.pack "if")
        (layoutPatternBindFinal
          Nothing
          binderDoc
          Nothing
          (NonEmpty.toList clauseDocs)
          Nothing
          hasComments
        )
    HsLet _ binds exp1 -> do
      expDoc1 <- docSharedWrapper layoutExpr' (toL exp1)
      -- We jump through some ugly hoops here to ensure proper sharing.
      hasComments <- hasAnyCommentsBelow lexpr
      let bindsSpan = localBindsSpan binds
      mBindDocs <- fmap (fmap pure) <$> layoutLocalBinds (L bindsSpan binds)
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
            [ docNodeAnnKW lexpr (Just AnnLet) $ docAlt
              [ docSeq
                [ appSep $ docLit $ Text.pack "let"
                , ifIndentFreeElse docSetBaseAndIndent docForceSingleline
                  $ bindDoc
                ]
              , docAddBaseY BrIndentRegular $ docPar
                (docLit $ Text.pack "let")
                (docSetBaseAndIndent bindDoc)
              ]
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
        stmtDocs <- docSharedWrapper layoutStmt `mapM` (map toL stmts)
        docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
          (docLit $ Text.pack "do")
          (docSetBaseAndIndent $ docNonBottomSpacing $ docLines stmtDocs)
      MDoExpr _ -> do
        stmtDocs <- docSharedWrapper layoutStmt `mapM` (map toL stmts)
        docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
          (docLit $ Text.pack "mdo")
          (docSetBaseAndIndent $ docNonBottomSpacing $ docLines stmtDocs)
      x
        | case x of
          ListComp -> True
          MonadComp -> True
          _ -> False
        -> do
          stmtDocs <- docSharedWrapper layoutStmt `mapM` (map toL stmts)
          hasComments <- hasAnyCommentsBelow lexpr
          runFilteredAlternative $ do
            addAlternativeCond (not hasComments) $ docSeq
              [ docNodeAnnKW lexpr Nothing $ appSep $ docLit $ Text.pack "["
              , docNodeAnnKW lexpr (Just AnnOpenS)
              $ appSep
              $ docForceSingleline
              $ List.last stmtDocs
              , appSep $ docLit $ Text.pack "|"
              , docSeq
              $ List.intersperse docCommaSep
              $ docForceSingleline
              <$> List.init stmtDocs
              , docLit $ Text.pack " ]"
              ]
            addAlternative
              $ let
                  start = docCols
                    ColListComp
                    [ docNodeAnnKW lexpr Nothing $ appSep $ docLit $ Text.pack
                      "["
                    , docSetBaseY
                    $ docNodeAnnKW lexpr (Just AnnOpenS)
                    $ List.last stmtDocs
                    ]
                  (s1 : sM) = List.init stmtDocs
                  line1 =
                    docCols ColListComp [appSep $ docLit $ Text.pack "|", s1]
                  lineM = sM <&> \d -> docCols ColListComp [docCommaSep, d]
                  end = docLit $ Text.pack "]"
                in docSetBaseY $ docLines $ [start, line1] ++ lineM ++ [end]
      _ -> do
        -- TODO
        unknownNodeError "HsDo{} unknown stmtCtx" lexpr
    ExplicitList _ elems@(_ : _) -> do
      elemDocs <- elems `forM` (docSharedWrapper layoutExpr' . toL)
      hasComments <- hasAnyCommentsBelow lexpr
      case splitFirstLast elemDocs of
        FirstLastEmpty -> docSeq
          [ docLit $ Text.pack "["
          , docNodeAnnKW lexpr (Just AnnOpenS) $ docLit $ Text.pack "]"
          ]
        FirstLastSingleton e -> docAlt
          [ docSeq
            [ docLit $ Text.pack "["
            , docNodeAnnKW lexpr (Just AnnOpenS) $ docForceSingleline e
            , docLit $ Text.pack "]"
            ]
          , docSetBaseY $ docLines
            [ docSeq
              [ docLit $ Text.pack "["
              , docSeparator
              , docSetBaseY $ docNodeAnnKW lexpr (Just AnnOpenS) e
              ]
            , docLit $ Text.pack "]"
            ]
          ]
        FirstLast e1 ems eN -> runFilteredAlternative $ do
          addAlternativeCond (not hasComments)
            $ docSeq
            $ [docLit $ Text.pack "["]
            ++ List.intersperse
                 docCommaSep
                 (docForceSingleline
                 <$> (e1 : ems ++ [docNodeAnnKW lexpr (Just AnnOpenS) eN])
                 )
            ++ [docLit $ Text.pack "]"]
          addAlternative
            $ let
                start = docCols ColList [appSep $ docLit $ Text.pack "[", e1]
                linesM = ems <&> \d -> docCols ColList [docCommaSep, d]
                lineN = docCols
                  ColList
                  [docCommaSep, docNodeAnnKW lexpr (Just AnnOpenS) eN]
                end = docLit $ Text.pack "]"
              in docSetBaseY $ docLines $ [start] ++ linesM ++ [lineN] ++ [end]
    ExplicitList _ [] -> docLit $ Text.pack "[]"
    RecordCon _ lname fields -> case fields of
      HsRecFields _ fs Nothing -> do
        let nameDoc = docWrapNode (toL lname) $ docLit $ lrdrNameToText (toL lname)
        rFs <-
          (map toL fs) `forM` \lfield@(L _ (HsFieldBind _ (L _ fieldOcc) rFExpr pun)) -> do
            let FieldOcc _ lnameF = fieldOcc
            rFExpDoc <- if pun
              then return Nothing
              else Just <$> docSharedWrapper layoutExpr' (toL rFExpr)
            return $ (lfield, lrdrNameToText (toL lnameF), rFExpDoc)
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
            return (fieldl, lrdrNameToText (toL lnameF), fExpDoc)
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
          return (lfield, ambNameRes, rFExpDoc)
      recordExpression False indentPolicy lexpr rExprDoc rFs
    RecordUpd _ _ (OverloadedRecUpdFields _ _) -> do
      briDocByExactInlineOnly "RecordUpd OverloadedRecUpdFields" lexpr
    ExprWithTySig _ exp1 sigWc -> case sigWc of
      HsWC _ body -> case unLoc body of
        HsSig _ _ typ1 -> do
          expDoc <- docSharedWrapper layoutExpr' (toL exp1)
          typDoc <- docSharedWrapper layoutType (toL typ1)
          docSeq [appSep expDoc, appSep $ docLit $ Text.pack "::", typDoc]
        _ -> briDocByExactInlineOnly "ExprWithTySig" lexpr
      _ -> briDocByExactInlineOnly "ExprWithTySig" lexpr
    ArithSeq _ Nothing info -> case info of
      From e1 -> do
        e1Doc <- docSharedWrapper layoutExpr' (toL e1)
        docSeq
          [ docLit $ Text.pack "["
          , appSep $ docForceSingleline e1Doc
          , docLit $ Text.pack "..]"
          ]
      FromThen e1 e2 -> do
        e1Doc <- docSharedWrapper layoutExpr' (toL e1)
        e2Doc <- docSharedWrapper layoutExpr' (toL e2)
        docSeq
          [ docLit $ Text.pack "["
          , docForceSingleline e1Doc
          , appSep $ docLit $ Text.pack ","
          , appSep $ docForceSingleline e2Doc
          , docLit $ Text.pack "..]"
          ]
      FromTo e1 eN -> do
        e1Doc <- docSharedWrapper layoutExpr' (toL e1)
        eNDoc <- docSharedWrapper layoutExpr' (toL eN)
        docSeq
          [ docLit $ Text.pack "["
          , appSep $ docForceSingleline e1Doc
          , appSep $ docLit $ Text.pack ".."
          , docForceSingleline eNDoc
          , docLit $ Text.pack "]"
          ]
      FromThenTo e1 e2 eN -> do
        e1Doc <- docSharedWrapper layoutExpr' (toL e1)
        e2Doc <- docSharedWrapper layoutExpr' (toL e2)
        eNDoc <- docSharedWrapper layoutExpr' (toL eN)
        docSeq
          [ docLit $ Text.pack "["
          , docForceSingleline e1Doc
          , appSep $ docLit $ Text.pack ","
          , appSep $ docForceSingleline e2Doc
          , appSep $ docLit $ Text.pack ".."
          , docForceSingleline eNDoc
          , docLit $ Text.pack "]"
          ]
    ArithSeq{} -> briDocByExactInlineOnly "ArithSeq" lexpr
    HsTypedBracket{} -> do
      -- TODO
      briDocByExactInlineOnly "HsTypedBracket{}" lexpr
    HsUntypedBracket{} -> do
      -- TODO
      briDocByExactInlineOnly "HsUntypedBracket{}" lexpr
    HsTypedSplice{} -> do
      -- TODO
      briDocByExactInlineOnly "HsTypedSplice{}" lexpr
    HsUntypedSplice _ (HsQuasiQuote _ quoter content) -> do
      allocateNode $ BDFPlain
        (Text.pack
        $ "["
        ++ showOutputable quoter
        ++ "|"
        ++ showOutputable (unLoc content)
        ++ "|]"
        )
    HsUntypedSplice{} -> do
      -- TODO
      briDocByExactInlineOnly "HsUntypedSplice{}" lexpr
    HsHole{} -> docLit $ Text.pack "_"
    HsProc{} -> briDocByExactInlineOnly "HsProc{}" lexpr
    HsStatic{} -> briDocByExactInlineOnly "HsStatic{}" lexpr
    HsGetField{} -> briDocByExactInlineOnly "HsGetField{}" lexpr
    HsProjection{} -> briDocByExactInlineOnly "HsProjection{}" lexpr
    HsPragE{} -> briDocByExactInlineOnly "HsPragE{}" lexpr
    HsEmbTy{} -> briDocByExactInlineOnly "HsEmbTy{}" lexpr
    XExpr _ -> briDocByExactInlineOnly "XExpr" lexpr
    _ -> briDocByExactInlineOnly "unknown HsExpr" lexpr

recordExpression
  :: (Data.Data.Data lExpr, Data.Data.Data name)
  => Bool
  -> IndentPolicy
  -> GenLocated SrcSpan lExpr
  -> ToBriDocM BriDocNumbered
  -> [ ( GenLocated SrcSpan name
       , Text
       , Maybe (ToBriDocM BriDocNumbered)
       )
     ]
  -> ToBriDocM BriDocNumbered
recordExpression False _ lexpr nameDoc [] = docSeq
  [ docNodeAnnKW lexpr (Just AnnOpenC)
    $ docSeq [nameDoc, docLit $ Text.pack "{"]
  , docLit $ Text.pack "}"
  ]
recordExpression True _ lexpr nameDoc [] = docSeq -- this case might still be incomplete, and is probably not used
         -- atm anyway.
  [ docNodeAnnKW lexpr (Just AnnOpenC)
    $ docSeq [nameDoc, docLit $ Text.pack "{"]
  , docLit $ Text.pack " .. }"
  ]
recordExpression dotdot indentPolicy lexpr nameDoc rFs@(rF1 : rFr) = do
  let (rF1f, rF1n, rF1e) = rF1
  runFilteredAlternative $ do
    -- container { fieldA = blub, fieldB = blub }
    addAlternative $ docSeq
      [ docNodeAnnKW lexpr Nothing $ appSep $ docForceSingleline nameDoc
      , appSep $ docLit $ Text.pack "{"
      , docSeq $ List.intersperse docCommaSep $ rFs <&> \case
        (lfield, fieldStr, Just fieldDoc) -> docWrapNode lfield $ docSeq
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
    addAlternativeCond (indentPolicy == IndentPolicyFree) $ docSeq
      [ docNodeAnnKW lexpr Nothing $ docForceSingleline $ appSep nameDoc
      , docSetBaseY
      $ docLines
      $ let
          line1 = docCols
            ColRec
            [ appSep $ docLit $ Text.pack "{"
            , docWrapNodePrior rF1f $ appSep $ docLit rF1n
            , case rF1e of
              Just x -> docWrapNodeRest rF1f $ docSeq
                [appSep $ docLit $ Text.pack "=", docForceSingleline x]
              Nothing -> docEmpty
            ]
          lineR = rFr <&> \(lfield, fText, fDoc) ->
            docWrapNode lfield $ docCols
              ColRec
              [ docCommaSep
              , appSep $ docLit fText
              , case fDoc of
                Just x ->
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
    addAlternative $ docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
      (docNodeAnnKW lexpr Nothing nameDoc)
      (docNonBottomSpacing
      $ docLines
      $ let
          line1 = docCols
            ColRec
            [ appSep $ docLit $ Text.pack "{"
            , docWrapNodePrior rF1f $ appSep $ docLit rF1n
            , docWrapNodeRest rF1f $ case rF1e of
              Just x -> runFilteredAlternative $ do
                addAlternativeCond (indentPolicy == IndentPolicyFree) $ do
                  docSeq [appSep $ docLit $ Text.pack "=", docSetBaseY x]
                addAlternative $ do
                  docSeq
                    [appSep $ docLit $ Text.pack "=", docForceParSpacing x]
                addAlternative $ do
                  docAddBaseY BrIndentRegular
                    $ docPar (docLit $ Text.pack "=") x
              Nothing -> docEmpty
            ]
          lineR = rFr <&> \(lfield, fText, fDoc) ->
            docWrapNode lfield $ docCols
              ColRec
              [ docCommaSep
              , appSep $ docLit fText
              , case fDoc of
                Just x -> runFilteredAlternative $ do
                  addAlternativeCond (indentPolicy == IndentPolicyFree) $ do
                    docSeq [appSep $ docLit $ Text.pack "=", docSetBaseY x]
                  addAlternative $ do
                    docSeq
                      [appSep $ docLit $ Text.pack "=", docForceParSpacing x]
                  addAlternative $ do
                    docAddBaseY BrIndentRegular
                      $ docPar (docLit $ Text.pack "=") x
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
      )

litBriDoc :: HsLit GhcPs -> BriDocFInt
litBriDoc = \case
  HsChar (SourceText t) _c -> BDFLit $ Text.pack (FastString.unpackFS t)
  HsCharPrim (SourceText t) _c -> BDFLit $ Text.pack (FastString.unpackFS t)
  HsString (SourceText t) _fastString -> BDFLit $ Text.pack (FastString.unpackFS t)
  HsStringPrim (SourceText t) _byteString -> BDFLit $ Text.pack (FastString.unpackFS t)
  HsInt _ il -> sourceTextLit (il_text il) (show (il_value il))
  HsIntPrim _ i -> BDFLit $ Text.pack (show i)
  HsWordPrim _ i -> BDFLit $ Text.pack (show i)
  HsInt64Prim _ i -> BDFLit $ Text.pack (show i)
  HsWord64Prim _ i -> BDFLit $ Text.pack (show i)
  HsFloatPrim _ fl -> sourceTextLit (fl_text fl) ""
  HsDoublePrim _ fl -> sourceTextLit (fl_text fl) ""
  _ -> error "litBriDoc: literal with no SourceText"

sourceTextLit :: SourceText -> String -> BriDocFInt
sourceTextLit (SourceText fs) _ = BDFLit $ Text.pack (FastString.unpackFS fs)
sourceTextLit NoSourceText fallback = BDFLit $ Text.pack fallback

overLitValBriDoc :: OverLitVal -> BriDocFInt
overLitValBriDoc = \case
  HsIntegral il -> sourceTextLit (il_text il) (show (il_value il))
  HsFractional fl -> sourceTextLit (fl_text fl) ""
  HsIsString st _ -> sourceTextLit st ""
  _ -> error "overLitValBriDoc: literal with no SourceText"
