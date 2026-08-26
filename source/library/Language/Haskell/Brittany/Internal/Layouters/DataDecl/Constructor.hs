{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.DataDecl.Constructor
  ( createAnnotatedDetailsDoc
  , createDetailsDoc
  , createGadtDetailsDoc
  , createMultipleDetailsDoc
  ) where

import qualified Data.Data
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as Text
import GHC (GenLocated(L), Located)
import GHC.Hs
import qualified GHC.OldList as List
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.IE (toL)
import Language.Haskell.Brittany.Internal.Layouters.Type
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Types

createDetailsDoc
  :: Text -> HsConDeclH98Details GhcPs -> ToBriDocM BriDocNumbered
createDetailsDoc = createDetailsDocWith False

createAnnotatedDetailsDoc
  :: Text -> HsConDeclH98Details GhcPs -> ToBriDocM BriDocNumbered
createAnnotatedDetailsDoc consNameStr details = case details of
  PrefixCon arguments ->
    createAnnotatedPrefixDoc consNameStr arguments
  RecCon (L _ []) ->
    docSeq [docLit consNameStr, docSeparator, docLit $ Text.pack "{}"]
  RecCon lRec@(L _ fields@(_ : _)) -> do
    let ((fName1, fType1) : fDocR) = mkAnnotatedFieldDocs fields
        allowSingleline = False
    docAddBaseY BrIndentRegular $ runFilteredAlternative $ do
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
              , docForceSingleline fType1
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
    [ createAnnotatedFieldTypeDoc arg1
    , docSeparator
    , docLit consNameStr
    , docSeparator
    , createAnnotatedFieldTypeDoc arg2
    ]

createAnnotatedPrefixDoc
  :: Text
  -> [HsConDeclField GhcPs]
  -> ToBriDocM BriDocNumbered
createAnnotatedPrefixDoc consNameStr arguments = do
  indentPolicy <- mAsk <&> _conf_layout .> _lconfig_indentPolicy .> confUnpack
  let argumentDocs = createAnnotatedFieldTypeDoc <$> arguments
      singleLine = docSeq
        [ docLit consNameStr
        , docSeparator
        , docForceSingleline
        $ docSeq
        $ List.intersperse docSeparator argumentDocs
        ]
      leftIndented = docSetParSpacing
        . docAddBaseY BrIndentRegular
        . docPar (docLit consNameStr)
        . docLines
        $ argumentDocs
      multiAppended = docSeq
        [ docLit consNameStr
        , docSeparator
        , docSetBaseY $ docLines argumentDocs
        ]
      multiIndented = docSetBaseY $ docAddBaseY BrIndentRegular $ docPar
        (docLit consNameStr)
        (docLines argumentDocs)
  case indentPolicy of
    IndentPolicyLeft -> docAlt [singleLine, leftIndented]
    IndentPolicyMultiple -> docAlt [singleLine, multiAppended, leftIndented]
    IndentPolicyFree ->
      docAlt [singleLine, multiAppended, multiIndented, leftIndented]

mkAnnotatedFieldDocs
  :: [LHsConDeclRecField GhcPs]
  -> [(ToBriDocM BriDocNumbered, ToBriDocM BriDocNumbered)]
mkAnnotatedFieldDocs = fmap $ \lField -> case lField of
  L _ (HsConDeclRecField { cdrf_spec = field, cdrf_names = names }) ->
    createAnnotatedNamesAndTypeDoc (toL lField) names field

createAnnotatedNamesAndTypeDoc
  :: Data.Data.Data ast
  => Located ast
  -> [GenLocated t (FieldOcc GhcPs)]
  -> HsConDeclField GhcPs
  -> (ToBriDocM BriDocNumbered, ToBriDocM BriDocNumbered)
createAnnotatedNamesAndTypeDoc lField names field =
  ( docNodeAnnKW lField Nothing $ docWrapNodePrior lField $ docSeq
    [ docSeq $ List.intersperse docCommaSep $ names <&> \case
        L _ (FieldOcc _ lname) -> docLit =<< lrdrNameToTextAnn (toL lname)
    ]
  , docWrapNodeRest lField $ createAnnotatedFieldTypeDoc field
  )

createAnnotatedFieldTypeDoc
  :: HsConDeclField GhcPs -> ToBriDocM BriDocNumbered
createAnnotatedFieldTypeDoc field = docSeq
  [ case cdf_unpack field of
      SrcUnpack -> docSeq [docLitS "{-# UNPACK #-}", docSeparator]
      SrcNoUnpack -> docSeq [docLitS "{-# NOUNPACK #-}", docSeparator]
      NoSrcUnpack -> docEmpty
  , case cdf_bang field of
      SrcLazy -> docLitS "~"
      SrcStrict -> docLitS "!"
      NoSrcStrict -> docEmpty
  , layoutType $ toL $ cdf_type field
  ]

createMultipleDetailsDoc
  :: Text -> HsConDeclH98Details GhcPs -> ToBriDocM BriDocNumbered
createMultipleDetailsDoc = createDetailsDocWith True

createDetailsDocWith
  :: Bool
  -> Text
  -> HsConDeclH98Details GhcPs
  -> ToBriDocM BriDocNumbered
createDetailsDocWith preferVertical consNameStr details = case details of
  PrefixCon args ->
    createPrefixDoc preferVertical consNameStr (cdf_type <$> args)
  RecCon (L _ []) ->
    docSeq [docLit consNameStr, docSeparator, docLit $ Text.pack "{}"]
  RecCon lRec@(L _ fields@(_ : _)) -> do
    let ((fName1, fType1) : fDocR) = mkFieldDocs fields
        allowSingleline = False
    docAddBaseY BrIndentRegular $ runFilteredAlternative $ do
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
              , docForceSingleline fType1
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
      createNamesAndTypeDoc (toL lField) nameList (toL $ cdf_type cdf)

createPrefixDoc
  :: Bool -> Text -> [LHsType GhcPs] -> ToBriDocM BriDocNumbered
createPrefixDoc preferVertical consNameStr argTypes = do
  indentPolicy <- mAsk <&> _conf_layout .> _lconfig_indentPolicy .> confUnpack
  let singleLine = docSeq
        [ docLit consNameStr
        , docSeparator
        , docForceSingleline
        $ docSeq
        $ List.intersperse docSeparator
        $ layoutType . toL <$> argTypes
        ]
      vertical = docLines
        [ docLit consNameStr
        , docEnsureIndent BrIndentRegular
          $ docLines
          $ layoutType . toL <$> argTypes
        ]
      leftIndented = docSetParSpacing
        . docAddBaseY BrIndentRegular
        . docPar (docLit consNameStr)
        . docLines
        $ layoutType . toL <$> argTypes
      multiAppended = docSeq
        [ docLit consNameStr
        , docSeparator
        , docSetBaseY $ docLines $ layoutType . toL <$> argTypes
        ]
      multiIndented = docSetBaseY $ docAddBaseY BrIndentRegular $ docPar
        (docLit consNameStr)
        (docLines $ layoutType . toL <$> argTypes)
  if preferVertical
    then case indentPolicy of
      IndentPolicyLeft -> docAlt [singleLine, vertical, leftIndented]
      IndentPolicyMultiple ->
        docAlt [singleLine, vertical, multiAppended, leftIndented]
      IndentPolicyFree ->
        docAlt [singleLine, vertical, multiAppended, multiIndented, leftIndented]
    else case indentPolicy of
      IndentPolicyLeft -> docAlt [singleLine, leftIndented]
      IndentPolicyMultiple -> docAlt [singleLine, multiAppended, leftIndented]
      IndentPolicyFree ->
        docAlt [singleLine, multiAppended, multiIndented, leftIndented]

createGadtDetailsDoc
  :: Text
  -> [HsConDeclField GhcPs]
  -> LHsType GhcPs
  -> ToBriDocM BriDocNumbered
createGadtDetailsDoc consNameStr arguments resultType = do
  let argumentDocs = layoutType . toL . cdf_type <$> arguments
      resultDoc = layoutType $ toL resultType
      singleLine = docSeq
        [ docLit consNameStr
        , docSeparator
        , docLitS "::"
        , docSeparator
        , docForceSingleline
        $ docSeq
        $ List.intersperse (docSeq [docSeparator, docLitS "->", docSeparator])
        $ argumentDocs ++ [resultDoc]
        ]
      signatureLines = case argumentDocs of
        [] -> [docSeq [docLitS "::", docSeparator, resultDoc]]
        firstArg : restArgs ->
          docSeq [docLitS "::", docSeparator, firstArg]
            : (restArgs ++ [resultDoc] <&> \argumentDoc ->
                docSeq [docLitS "->", docSeparator, argumentDoc])
      multiline = docAddBaseY BrIndentRegular
        $ docPar (docLit consNameStr) (docLines signatureLines)
  docAlt [singleLine, multiline]

createNamesAndTypeDoc
  :: Data.Data.Data ast
  => Located ast
  -> [GenLocated t (FieldOcc GhcPs)]
  -> Located (HsType GhcPs)
  -> (ToBriDocM BriDocNumbered, ToBriDocM BriDocNumbered)
createNamesAndTypeDoc lField names typ =
  ( docNodeAnnKW lField Nothing $ docWrapNodePrior lField $ docSeq
    [ docSeq $ List.intersperse docCommaSep $ names <&> \case
        L _ (FieldOcc _ lname) -> docLit =<< lrdrNameToTextAnn (toL lname)
    ]
  , docWrapNodeRest lField $ layoutType typ
  )
