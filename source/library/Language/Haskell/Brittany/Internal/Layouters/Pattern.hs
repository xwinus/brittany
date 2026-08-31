{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Pattern
  ( PatternLayout(..)
  , colsWrapPat
  , layoutPat
  , layoutPatMultiline
  , layoutPatNative
  , layoutPatStructural
  , layoutPattern
  , patternCompactDocument
  , patternDocument
  , wrapPatListy
  , wrapPatPrepend
  ) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import GHC (GenLocated(L), ol_val)
import GHC.Hs
import qualified GHC.OldList as List
import GHC.Types.Basic
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as ExactPrintCompat
import Language.Haskell.Brittany.Internal.Fallbacks
  ( FallbackId(..)
  , untypedSpliceFamily
  )
import Language.Haskell.Brittany.Internal.Delimiter.Types
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.IE (toL)
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Expr
import Language.Haskell.Brittany.Internal.Layouters.Pattern.Comments
import Language.Haskell.Brittany.Internal.Layouters.Pattern.Record
import Language.Haskell.Brittany.Internal.Layouters.Pattern.Types
import Language.Haskell.Brittany.Internal.Layouters.Type
import Language.Haskell.Brittany.Internal.PatternComments
  ( requiresExactSourcePattern )
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Types



layoutPattern :: LPat GhcPs -> ToBriDocM PatternLayout
layoutPattern pattern' = do
  compactColumns <- layoutPat pattern'
  structuralDocument <- layoutPatStructural pattern'
  pure PatternLayout
    { patternCompactColumns = compactColumns
    , patternStructuralDocument = structuralDocument
    }

-- | layouts patterns (inside function bindings, case alternatives, let
-- bindings or do notation). E.g. for input
--        > case computation of
--        >   (warnings, Success a b) -> ..
-- This part  ^^^^^^^^^^^^^^^^^^^^^^^ of the syntax tree is layouted by
-- 'layoutPat'. Similarly for
-- > func abc True 0 = []
--        ^^^^^^^^^^ this part
-- We will use `case .. of` as the imagined prefix to the examples used in
-- the different cases below.
layoutPat :: LPat GhcPs -> ToBriDocM (Seq BriDocNumbered)
layoutPat lpat@(L _ pat) =
  if requiresExactSourcePattern pat
    then Seq.singleton <$> briDocByExactInlineOnly PatternFallback (toL lpat)
    else layoutPatNative lpat

layoutPatNative :: LPat GhcPs -> ToBriDocM (Seq BriDocNumbered)
layoutPatNative lpat@(L _ pat) = docWrapNode (toL lpat) $ case pat of
  WildPat _ -> fmap Seq.singleton $ docLit $ Text.pack "_"
    -- _ -> expr
  VarPat _ n -> fmap Seq.singleton $ docLit $ lrdrNameToText (toL n)
    -- abc -> expr
  LitPat _ lit -> fmap Seq.singleton $ allocateNode $ litBriDoc lit
    -- 0 -> expr
  ParPat _ inner -> do
    innerDocs <- colsWrapPat =<< layoutPat inner
    document <- docDelimitedSequence
      ParenthesesDelimiter
      (Text.pack "(")
      (Text.pack ")")
      (Just $ ExactPrintCompat.mkAnnKey $ toL lpat)
      [ ( Just $ ExactPrintCompat.mkAnnKey $ toL inner
        , PresentDelimiterChild
        , pure innerDocs
        )
      ]
      []
      DelimiterIndentRegular
      BlockDelimiterChild
      [DelimiterCompact]
    pure $ Seq.singleton document
    -- return $ (left Seq.<| innerDocs) Seq.|> right
    -- case Seq.viewl innerDocs of
    --   Seq.EmptyL -> fmap return $ docLit $ Text.pack "()" -- this should never occur..
    --   x1 Seq.:< rest -> case Seq.viewr rest of
    --     Seq.EmptyR ->
    --       fmap return $ docSeq
    --       [ docLit $ Text.pack "("
    --       , return x1
    --       , docLit $ Text.pack ")"
    --       ]
    --     middle Seq.:> xN -> do
    --       x1' <- docSeq [docLit $ Text.pack "(", return x1]
    --       xN' <- docSeq [return xN, docLit $ Text.pack ")"]
    --       return $ (x1' Seq.<| middle) Seq.|> xN'
  ConPat _ lname (PrefixCon args) -> do
    -- Abc a b c -> expr
    nameDoc' <- applyNameAdornment lname <$> lrdrNameToTextAnn (toL lname)
    argDocs <- layoutPat `mapM` args
    if null argDocs
      then return <$> docLit nameDoc'
      else do
        x1 <- appSep (docLit nameDoc')
        xR <- fmap Seq.fromList $ sequence $ spacifyDocs $ fmap
          colsWrapPat
          argDocs
        return $ x1 Seq.<| xR
  ConPat _ lname (InfixCon left right) -> do
    -- a :< b -> expr
    nameDoc' <- applyNameAdornment lname <$> lrdrNameToTextAnn (toL lname)
    leftDoc <- appSep . colsWrapPat =<< layoutPat left
    rightDoc <- colsWrapPat =<< layoutPat right
    middle <- appSep $ docLit nameDoc'
    return $ Seq.empty Seq.|> leftDoc Seq.|> middle Seq.|> rightDoc
  ConPat _ lname (RecCon (HsRecFields _ [] Nothing)) -> do
    -- Abc{} -> expr
    let t = lrdrNameToText lname
    fmap Seq.singleton $ docLit $ t <> Text.pack "{}"
  ConPat _ lname (RecCon (HsRecFields _ fs@(_ : _) Nothing)) -> do
    -- Abc { a = locA, b = locB, c = locC } -> expr1
    -- Abc { a, b, c } -> expr2
    let t = lrdrNameToText lname
    fds <- fs `forM` \(L _ (HsFieldBind _ (L _ fieldOcc) fPat pun)) -> do
      let FieldOcc _ lnameF = fieldOcc
      fExpDoc <- if pun
        then return Nothing
        else Just <$> docSharedWrapper layoutPat fPat
      return (lrdrNameToText (toL lnameF), fExpDoc)
    Seq.singleton <$> docSeq
      [ appSep $ docLit t
      , appSep $ docLit $ Text.pack "{"
      , docSeq $ List.intersperse docCommaSep $ fds <&> \case
        (fieldName, Just fieldDoc) -> docSeq
          [ appSep $ docLit fieldName
          , appSep $ docLit $ Text.pack "="
          , fieldDoc >>= colsWrapPat
          ]
        (fieldName, Nothing) -> docLit fieldName
      , docSeparator
      , docLit $ Text.pack "}"
      ]
  ConPat _ lname (RecCon (HsRecFields _ [] (Just (L _ (RecFieldsDotDot 0))))) -> do
    -- Abc { .. } -> expr
    let t = lrdrNameToText lname
    Seq.singleton <$> docSeq [appSep $ docLit t, docLit $ Text.pack "{..}"]
  ConPat _ lname (RecCon (HsRecFields _ fs@(_ : _) (Just (L _ (RecFieldsDotDot dotdoti)))))
    | dotdoti == length fs -> do
    -- Abc { a = locA, .. }
      let t = lrdrNameToText lname
      fds <- fs `forM` \(L _ (HsFieldBind _ (L _ fieldOcc) fPat pun)) -> do
        let FieldOcc _ lnameF = fieldOcc
        fExpDoc <- if pun
          then return Nothing
          else Just <$> docSharedWrapper layoutPat fPat
        return (lrdrNameToText (toL lnameF), fExpDoc)
      Seq.singleton <$> docSeq
        [ appSep $ docLit t
        , appSep $ docLit $ Text.pack "{"
        , docSeq $ fds >>= \case
          (fieldName, Just fieldDoc) ->
            [ appSep $ docLit fieldName
            , appSep $ docLit $ Text.pack "="
            , fieldDoc >>= colsWrapPat
            , docCommaSep
            ]
          (fieldName, Nothing) -> [docLit fieldName, docCommaSep]
        , docLit $ Text.pack "..}"
        ]
  TuplePat _ args boxity -> do
    -- (nestedpat1, nestedpat2, nestedpat3) -> expr
    -- (#nestedpat1, nestedpat2, nestedpat3#) -> expr
    case boxity of
      Boxed -> wrapPatListy lpat
        ParenthesesDelimiter (Text.pack "(") (Text.pack ")") args
      Unboxed -> wrapPatListy lpat
        UnboxedParenthesesDelimiter (Text.pack "(#") (Text.pack "#)") args
  AsPat _ asName asPat -> do
    -- bind@nestedpat -> expr
    wrapPatPrepend asPat (docLit $ lrdrNameToText asName <> Text.pack "@")
  SigPat _ pat1 (HsPS _ ty1) -> do
    -- i :: Int -> expr
    patDocs <- layoutPat pat1
    tyDoc <- docSharedWrapper layoutType (toL ty1)
    case Seq.viewr patDocs of
      Seq.EmptyR -> error "cannot happen ljoiuxoasdcoviuasd"
      xR Seq.:> xN -> do
        xN' <- -- at the moment, we don't support splitting patterns into
               -- multiple lines. but we cannot enforce pasting everything
               -- into one line either, because the type signature will ignore
               -- this if we overflow sufficiently.
               -- In order to prevent syntactically invalid results in such
               -- cases, we need the AddBaseY here.
               -- This can all change when patterns get multiline support.
               docAddBaseY BrIndentRegular $ docSeq
          [ appSep $ return xN
          , appSep $ docLit $ Text.pack "::"
          , docForceSingleline tyDoc
          ]
        return $ xR Seq.|> xN'
  ListPat _ elems ->
    -- [] -> expr1
    -- [nestedpat1, nestedpat2, nestedpat3] -> expr2
    wrapPatListy lpat SquareBracketsDelimiter (Text.pack "[") (Text.pack "]") elems
  BangPat _ pat1 -> do
    -- !nestedpat -> expr
    wrapPatPrepend pat1 (docLit $ Text.pack "!")
  LazyPat _ pat1 -> do
    -- ~nestedpat -> expr
    wrapPatPrepend pat1 (docLit $ Text.pack "~")
  NPat _ llit@(L _ ol) mNegative _ -> do
    -- -13 -> expr
    litDoc <- docWrapNode (toL llit) $ allocateNode $ overLitValBriDoc $ GHC.ol_val ol
    negDoc <- docLit $ Text.pack "-"
    pure $ case mNegative of
      Just{} -> Seq.fromList [negDoc, litDoc]
      Nothing -> Seq.singleton litDoc
  ViewPat _ expr1 pat1 -> do
    -- (expr -> nestedpat) -> expr
    exprDoc <- docSharedWrapper layoutExpr (toL expr1)
    patDocs <- layoutPat pat1
    case Seq.viewl patDocs of
      Seq.EmptyL -> Seq.singleton <$> docForceSingleline exprDoc
      x1 Seq.:< xR -> do
        x1' <- docSeq
          [ appSep $ docForceSingleline exprDoc
          , appSep $ docLit $ Text.pack "->"
          , return x1
          ]
        return $ x1' Seq.<| xR

  EmbTyPat _ tyPat -> do
      typeDoc <- docSharedWrapper layoutType (toL $ hstp_body tyPat)
      singleDoc <- docSeq
        [ appSep $ docLit $ Text.pack "type"
        , typeDoc
        ]
      return $ Seq.singleton singleDoc

  InvisPat _ tyPat -> do
      typeDoc <- docSharedWrapper layoutType (toL $ hstp_body tyPat)
      patternDoc <- docSeq [docLit $ Text.pack "@", typeDoc]
      trailingComments <- ownedTrailingPatternComments (toL lpat)
      case trailingComments of
        [] -> return $ Seq.singleton patternDoc
        _ -> do
          commentedPattern <- docSeq
            $ return patternDoc
            : List.concat
              [ [docLit $ Text.pack " ", layoutPatternSourceComment sourceComment]
              | sourceComment <- trailingComments
              ]
          Seq.singleton <$> docLines
            [return commentedPattern, docBlankLine]

  OrPat _ pats -> do
      let patList = NonEmpty.toList pats
      patDocs <- forM patList $ \p -> do
        patDocSeq <- layoutPat p
        colsWrapPat patDocSeq
      singleDoc <- docSeq
        $ List.intersperse (docLit $ Text.pack " ; ") (map return patDocs)
      return $ Seq.singleton singleDoc

  SplicePat _ splice -> fmap Seq.singleton
    $ briDocByOpaqueNoComment
      (untypedSpliceFamily splice)
      PatternFallback
      (toL lpat)

  _ -> return <$> briDocByExactInlineOnly PatternFallback (toL lpat)

layoutPatStructural :: LPat GhcPs -> ToBriDocM (Maybe BriDocNumbered)
layoutPatStructural lpat@(L _ pat)
  | requiresExactSourcePattern pat = pure Nothing
  | otherwise = case pat of
  ConPat _ lname (PrefixCon args@(_ : _)) -> do
    nameDoc <- applyNameAdornment lname <$> lrdrNameToTextAnn (toL lname)
    argDocs <- args `forM` \arg -> do
      patternDocument =<< layoutPattern arg
    fmap Just $ docWrapNode (toL lpat)
      $ docAddBaseY BrIndentRegular
      $ docPar
          (docLit nameDoc)
          (docSetIndentLevel $ docLines $ return <$> argDocs)
  ConPat _ lname (InfixCon left right) -> do
    nameDoc <- applyNameAdornment lname <$> lrdrNameToTextAnn (toL lname)
    leftDoc <- patternDocument =<< layoutPattern left
    rightDoc <- patternDocument =<< layoutPattern right
    fmap Just $ docWrapNode (toL lpat)
      $ docAddBaseY BrIndentRegular
      $ docLines
        [ pure leftDoc
        , docEnsureIndent BrIndentRegular $ docSeq
          [ appSep $ docLit nameDoc
          , pure rightDoc
          ]
        ]
  ConPat _ lname (RecCon fields) ->
    Just <$> layoutRecordPattern layoutPattern lpat lname fields
  ParPat _ inner -> do
    innerDoc <- patternDocument =<< layoutPattern inner
    fmap Just $ docWrapNode (toL lpat)
      $ docDelimitedSequence
        ParenthesesDelimiter
        (Text.pack "(")
        (Text.pack ")")
        (Just $ ExactPrintCompat.mkAnnKey $ toL lpat)
        [ ( Just $ ExactPrintCompat.mkAnnKey $ toL inner
          , PresentDelimiterChild
          , pure innerDoc
          )
        ]
        []
        DelimiterIndentRegular
        PatternBlockDelimiterChild
        [DelimiterCompact, DelimiterAttached]
  TuplePat _ elements boxity -> case boxity of
    Boxed -> layoutDelimitedPattern
      lpat ParenthesesDelimiter (Text.pack "(") (Text.pack ")")
      docParenL docParenR elements
    Unboxed -> layoutDelimitedPattern
      lpat UnboxedParenthesesDelimiter (Text.pack "(#") (Text.pack "#)")
      (docLit $ Text.pack "(#") (docLit $ Text.pack "#)") elements
  ListPat _ elements -> layoutDelimitedPattern
    lpat SquareBracketsDelimiter (Text.pack "[") (Text.pack "]")
    docBracketL docBracketR elements
  AsPat _ name inner -> layoutPrefixedPattern
    lpat (docLit $ lrdrNameToText name <> Text.pack "@") inner
  BangPat _ inner -> layoutPrefixedPattern lpat (docLit $ Text.pack "!") inner
  LazyPat _ inner -> layoutPrefixedPattern lpat (docLit $ Text.pack "~") inner
  SigPat _ inner (HsPS _ signatureType) -> do
    innerDoc <- patternDocument =<< layoutPattern inner
    typeDoc <- docSharedWrapper layoutType $ toL signatureType
    fmap Just $ docWrapNode (toL lpat)
      $ docAddBaseY BrIndentRegular
      $ docPar
        (pure innerDoc)
        (docSeq
          [ appSep $ docLit $ Text.pack "::"
          , typeDoc
          ]
        )
  ViewPat _ expression inner -> do
    expressionDoc <- docSharedWrapper layoutExpr $ toL expression
    innerDoc <- patternDocument =<< layoutPattern inner
    fmap Just $ docWrapNode (toL lpat)
      $ docAddBaseY BrIndentRegular
      $ docPar
        (docSeq
          [ appSep expressionDoc
          , docLit $ Text.pack "->"
          ]
        )
        (pure innerDoc)
  OrPat _ patterns -> do
    patternDocs <- mapM (layoutPattern >=> patternDocument)
      $ NonEmpty.toList patterns
    fmap Just $ docWrapNode (toL lpat)
      $ docSetBaseY
      $ docLines
      $ case patternDocs of
          [] -> []
          firstPattern : remainingPatterns -> pure firstPattern
            : [ docSeq
                [ appSep $ docLit $ Text.pack ";"
                , pure patternDoc
                ]
              | patternDoc <- remainingPatterns
              ]
  _ -> return Nothing

layoutPrefixedPattern
  :: LPat GhcPs
  -> ToBriDocM BriDocNumbered
  -> LPat GhcPs
  -> ToBriDocM (Maybe BriDocNumbered)
layoutPrefixedPattern outer prefix inner = do
  innerLayout <- layoutPattern inner
  case patternStructuralDocument innerLayout of
    Nothing -> pure Nothing
    Just structuralDocument -> fmap Just $ docWrapNode (toL outer)
      $ docSeq [prefix, pure structuralDocument]

layoutDelimitedPattern
  :: LPat GhcPs
  -> DelimiterKind
  -> Text
  -> Text
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
  -> [LPat GhcPs]
  -> ToBriDocM (Maybe BriDocNumbered)
layoutDelimitedPattern _ _ _ _ _ _ [] = pure Nothing
layoutDelimitedPattern outer kind openToken closeToken _ _ elements = do
  elementDocs <- mapM (layoutPattern >=> patternDocument) elements
  fmap Just $ docWrapNode (toL outer)
    $ docDelimitedSequence
      kind
      openToken
      closeToken
      (Just $ ExactPrintCompat.mkAnnKey $ toL outer)
      (zipWith
        (\element document ->
          ( Just $ ExactPrintCompat.mkAnnKey $ toL element
          , PresentDelimiterChild
          , pure document
          )
        )
        elements
        elementDocs)
      (replicate (max 0 $ length elements - 1)
        (RepeatedDelimiterSeparator, Text.pack ",", AttachSeparatorLeft))
      DelimiterIndentRegular
      TrailingDelimiterSeparators
      (if length elements == 1
        then [DelimiterCompact]
        else [DelimiterAttached])

layoutPatMultiline :: LPat GhcPs -> ToBriDocM (Maybe BriDocNumbered)
layoutPatMultiline = layoutPatStructural

wrapPatPrepend
  :: LPat GhcPs -> ToBriDocM BriDocNumbered -> ToBriDocM (Seq BriDocNumbered)
wrapPatPrepend pat prepElem = do
  patDocs <- layoutPat pat
  case Seq.viewl patDocs of
    Seq.EmptyL -> return Seq.empty
    x1 Seq.:< xR -> do
      x1' <- docSeq [prepElem, return x1]
      return $ x1' Seq.<| xR

wrapPatListy
  :: LPat GhcPs
  -> DelimiterKind
  -> Text
  -> Text
  -> [LPat GhcPs]
  -> ToBriDocM (Seq BriDocNumbered)
wrapPatListy outer kind open close elems = do
  elemDocs <- elems `forM` (layoutPat >=> colsWrapPat)
  document <- docDelimitedSequence
    kind
    open
    close
    (Just $ ExactPrintCompat.mkAnnKey $ toL outer)
    (zipWith
      (\element childDocument ->
        ( Just $ ExactPrintCompat.mkAnnKey $ toL element
        , PresentDelimiterChild
        , pure childDocument
        )
      )
      elems
      elemDocs)
    (replicate (max 0 $ length elems - 1)
      (RepeatedDelimiterSeparator, Text.pack ",", AttachSeparatorRight))
    DelimiterIndentRegular
    PatternInlineDelimiter
    [DelimiterCompact]
  pure $ Seq.singleton document
