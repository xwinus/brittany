{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.Layouters.DataDecl.Constructor
  ( createAnnotatedDetailsDoc
  , createDetailsDoc
  , createGadtDetailsDoc
  , createMultipleDetailsDoc
  , createVerticalDetailsDoc
  ) where

import qualified Data.Data
import Data.Kind (Type)
import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as Text
import GHC (GenLocated(L), Located, getLoc)
import GHC.Hs
import qualified GHC.OldList as List
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.ExactPrintCompat
  ( realSpanToSrcSpan
  , srcSpanToRealSpan
  )
import Language.Haskell.Brittany.Internal.ExactSource (sourceCommentFragment)
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.IE (toL)
import Language.Haskell.Brittany.Internal.Layouters.Type
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Types

createDetailsDoc
  :: Text -> HsConDeclH98Details GhcPs -> ToBriDocM BriDocNumbered
createDetailsDoc = createDetailsDocWith PreferHanging

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
    [ createFieldTypeDoc arg1
    , docSeparator
    , docLit consNameStr
    , docSeparator
    , createFieldTypeDoc arg2
    ]

createAnnotatedPrefixDoc
  :: Text
  -> [HsConDeclField GhcPs]
  -> ToBriDocM BriDocNumbered
createAnnotatedPrefixDoc = createPrefixDoc PreferHanging

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
  , docWrapNodeRest lField $ createFieldTypeDoc field
  )

createFieldTypeDoc
  :: HsConDeclField GhcPs -> ToBriDocM BriDocNumbered
createFieldTypeDoc field = docSeq
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
createMultipleDetailsDoc = createDetailsDocWith PreferVertical

createVerticalDetailsDoc
  :: Text -> HsConDeclH98Details GhcPs -> ToBriDocM BriDocNumbered
createVerticalDetailsDoc = createDetailsDocWith ForceVertical

type PrefixLayoutPreference :: Type
data PrefixLayoutPreference
  = PreferHanging
  | PreferVertical
  | ForceVertical

createDetailsDocWith
  :: PrefixLayoutPreference
  -> Text
  -> HsConDeclH98Details GhcPs
  -> ToBriDocM BriDocNumbered
createDetailsDocWith preference consNameStr details = case details of
  PrefixCon args ->
    createPrefixDoc preference consNameStr args
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
    [ createFieldTypeDoc arg1
    , docSeparator
    , docLit consNameStr
    , docSeparator
    , createFieldTypeDoc arg2
    ]
 where
  mkFieldDocs
    :: [LHsConDeclRecField GhcPs]
    -> [(ToBriDocM BriDocNumbered, ToBriDocM BriDocNumbered)]
  mkFieldDocs = fmap $ \lField -> case lField of
    L _ (HsConDeclRecField { cdrf_spec = cdf, cdrf_names = nameList }) ->
      createNamesAndTypeDoc (toL lField) nameList cdf

createPrefixDoc
  :: PrefixLayoutPreference
  -> Text
  -> [HsConDeclField GhcPs]
  -> ToBriDocM BriDocNumbered
createPrefixDoc preference consNameStr arguments = do
  indentPolicy <- mAsk <&> _conf_layout .> _lconfig_indentPolicy .> confUnpack
  layoutColumns <- mAsk <&> _conf_layout .> _lconfig_cols .> confUnpack
  let argumentDocs = createFieldTypeDoc <$> arguments
      singleLine = docSeq
        [ docLit consNameStr
        , docSeparator
        , docForceSingleline
        $ docSeq
        $ List.intersperse docSeparator
        $ argumentDocs
        ]
      vertical = docLines
        [ docLit consNameStr
        , docEnsureIndent BrIndentRegular
          $ docLines
          $ argumentDocs
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
      boundedHanging = docColumnsLimit
        (layoutColumns * 3 `div` 5)
        multiAppended
      multiIndented = docSetBaseY $ docAddBaseY BrIndentRegular $ docPar
        (docLit consNameStr)
        (docLines argumentDocs)
      compactAlternatives = case preference of
        ForceVertical -> []
        _ -> [singleLine]
      structuralAlternatives = case preference of
        PreferHanging -> [boundedHanging, vertical]
        PreferVertical -> [vertical, boundedHanging]
        ForceVertical -> [vertical, boundedHanging]
  case indentPolicy of
    IndentPolicyLeft -> docAlt $ compactAlternatives ++ case preference of
      PreferHanging -> [leftIndented, vertical]
      _ -> [vertical, leftIndented]
    IndentPolicyMultiple -> docAlt
      $ compactAlternatives ++ structuralAlternatives ++ [leftIndented]
    IndentPolicyFree -> docAlt
      $ compactAlternatives
      ++ structuralAlternatives
      ++ [multiIndented, leftIndented]

createGadtDetailsDoc
  :: Located (ConDecl GhcPs)
  -> Text
  -> [HsConDeclField GhcPs]
  -> LHsType GhcPs
  -> ToBriDocM BriDocNumbered
createGadtDetailsDoc constructor consNameStr arguments resultType = do
  signatureComments <- gadtSignatureComments
    constructor arguments resultType
  let argumentDocs = createFieldTypeDoc <$> arguments
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
      typeLines = case argumentDocs of
        [] -> [docSeq [docLitS "::", docSeparator, resultDoc]]
        firstArg : restArgs ->
          docSeq [docLitS "::", docSeparator, firstArg]
            : (restArgs ++ [resultDoc] <&> \argumentDoc ->
                docSeq [docLitS "->", docSeparator, argumentDoc])
      signatureLines = case signatureComments of
        [] -> typeLines
        _ -> docLitS "::"
          : (layoutGadtSourceComment <$> signatureComments)
          ++ case argumentDocs of
            [] -> [resultDoc]
            firstArg : restArgs -> firstArg
              : (restArgs ++ [resultDoc] <&> \argumentDoc ->
                  docSeq [docLitS "->", docSeparator, argumentDoc])
      multiline = docAddBaseY BrIndentRegular
        $ docPar (docLit consNameStr) (docLines signatureLines)
  if null signatureComments
    then docAlt [singleLine, multiline]
    else multiline

gadtSignatureComments
  :: Located (ConDecl GhcPs)
  -> [HsConDeclField GhcPs]
  -> LHsType GhcPs
  -> ToBriDocM [SourceComment]
gadtSignatureComments constructor arguments resultType = do
  commentPlan <- mAsk
  let constructorStart = fmap sourceSpanStart
        $ srcSpanToRealSpan $ getLoc constructor
      firstType = case arguments of
        [] -> resultType
        firstArgument : _ -> cdf_type firstArgument
      firstTypeEnd = fmap sourceSpanEnd
        $ srcSpanToRealSpan $ getLoc $ toL firstType
  pure $ case (constructorStart, firstTypeEnd) of
    (Just start, Just end) -> List.sortOn gadtSourceCommentStart
      [ sourceComment
      | sourceComment <- Map.elems $ commentPlanSources commentPlan
      , fst (gadtSourceCommentStart sourceComment) >= fst start
      , fst (gadtSourceCommentEnd sourceComment) <= fst end
      ]
    _ -> []

layoutGadtSourceComment :: SourceComment -> ToBriDocM BriDocNumbered
layoutGadtSourceComment sourceComment = briDocBySourceFragmentNoComment
  (L (realSpanToSrcSpan $ sourceCommentSpan sourceComment) sourceComment)
  (sourceCommentFragment sourceComment)

gadtSourceCommentStart :: SourceComment -> (Int, Int)
gadtSourceCommentStart sourceComment =
  ( SrcLoc.srcSpanStartLine $ sourceCommentSpan sourceComment
  , SrcLoc.srcSpanStartCol $ sourceCommentSpan sourceComment
  )

gadtSourceCommentEnd :: SourceComment -> (Int, Int)
gadtSourceCommentEnd sourceComment = sourceSpanEnd
  $ sourceCommentSpan sourceComment

sourceSpanEnd :: SrcLoc.RealSrcSpan -> (Int, Int)
sourceSpanEnd span' =
  (SrcLoc.srcSpanEndLine span', SrcLoc.srcSpanEndCol span')

sourceSpanStart :: SrcLoc.RealSrcSpan -> (Int, Int)
sourceSpanStart span' =
  (SrcLoc.srcSpanStartLine span', SrcLoc.srcSpanStartCol span')

createNamesAndTypeDoc
  :: Data.Data.Data ast
  => Located ast
  -> [GenLocated t (FieldOcc GhcPs)]
  -> HsConDeclField GhcPs
  -> (ToBriDocM BriDocNumbered, ToBriDocM BriDocNumbered)
createNamesAndTypeDoc lField names field =
  ( docNodeAnnKW lField Nothing $ docWrapNodePrior lField $ docSeq
    [ docSeq $ List.intersperse docCommaSep $ names <&> \case
        L _ (FieldOcc _ lname) -> docLit =<< lrdrNameToTextAnn (toL lname)
    ]
  , docWrapNodeRest lField $ createFieldTypeDoc field
  )
