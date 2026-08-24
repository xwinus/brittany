{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Import where

import qualified Data.Semigroup as Semigroup
import qualified Data.Set as Set
import qualified Data.Text as Text
import GHC (GenLocated(L), Located, moduleNameString, unLoc)
import GHC.Hs
import GHC.Types.Basic
import qualified GHC.Data.FastString as FastString
import GHC.Types.PkgQual (RawPkgQual(..))
import GHC.Types.SourceText (SourceText(..), StringLiteral(..))
import GHC.Unit.Types (IsBootInterface(..))
import Language.Haskell.Syntax.ImpExp (ImportListInterpretation(EverythingBut))
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.ExactPrintCompat (AnnKey)
import Language.Haskell.Brittany.Internal.Fallbacks (FallbackId(..))
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.IE (layoutLLIEs, layoutAnnAndSepLLIEs, toL, SortItemsFlag(ShouldSortItems))
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Types



prepPkg :: SourceText -> String
prepPkg rawN = case rawN of
  SourceText n -> FastString.unpackFS n
  -- This would be odd to encounter and the
  -- result will most certainly be wrong
  NoSourceText -> ""
prepModName :: Located e -> e
prepModName = unLoc

pkgToSourceText :: RawPkgQual -> Maybe SourceText
pkgToSourceText (RawPkgQual (StringLiteral st _ _)) = Just st
pkgToSourceText NoRawPkgQual = Nothing

layoutImport :: ToBriDoc ImportDecl
layoutImport = layoutImportWithExactText Nothing

layoutImportWithExactText
  :: Maybe (Text.Text, Set.Set AnnKey)
  -> Located (ImportDecl GhcPs)
  -> ToBriDocM BriDocNumbered
layoutImportWithExactText exactText importNode@(L _ importD) = case exactText of
  Just (source, annotationKeys) ->
    briDocByExactTextWithAnnsNoComment
      ImportFallback
      importNode
      annotationKeys
      source
  Nothing -> layoutImportNormally importD

layoutImportNormally :: ImportDecl GhcPs -> ToBriDocM BriDocNumbered
layoutImportNormally importD = case importD of
  ImportDecl
    { ideclName = lmodName
    , ideclPkgQual = pkg
    , ideclSource = src
    , ideclSafe = safe
    , ideclQualified = q
    , ideclAs = mas
    , ideclImportList = mllies
    } -> do
    importCol <- mAsk <&> _conf_layout .> _lconfig_importColumn .> confUnpack
    importAsCol <-
      mAsk <&> _conf_layout .> _lconfig_importAsColumn .> confUnpack
    indentPolicy <- mAsk <&> _conf_layout .> _lconfig_indentPolicy .> confUnpack
    let
      modName = unLoc lmodName
      compact = indentPolicy /= IndentPolicyFree
      modNameT = Text.pack $ moduleNameString modName
      pkgNameT = Text.pack . prepPkg <$> pkgToSourceText pkg
      masT = Text.pack . moduleNameString . unLoc <$> mas
      hiding = maybe False (== EverythingBut) (fst <$> mllies)
      minQLength = length "import qualified "
      qLengthReal =
        let
          qualifiedPart = if q /= NotQualified then length "qualified " else 0
          safePart = if safe then length "safe " else 0
          pkgPart = maybe 0 ((+ 1) . Text.length) pkgNameT
          srcPart = case src of
            IsBoot -> length "{-# SOURCE #-} "
            NotBoot -> 0
        in length "import " + srcPart + safePart + qualifiedPart + pkgPart
      qLength = max minQLength qLengthReal
      -- Cost in columns of importColumn
      asCost = length "as "
      hidingParenCost = if hiding then length "hiding ( " else length "( "
      nameCost = Text.length modNameT + qLength
      importQualifiers = docSeq
        [ appSep $ docLit $ Text.pack "import"
        , case src of
          IsBoot -> appSep $ docLit $ Text.pack "{-# SOURCE #-}"
          NotBoot -> docEmpty
        , if safe then appSep $ docLit $ Text.pack "safe" else docEmpty
        , if q /= NotQualified
          then appSep $ docLit $ Text.pack "qualified"
          else docEmpty
        , maybe docEmpty (appSep . docLit) pkgNameT
        ]
      indentName =
        if compact then id else docEnsureIndent (BrIndentSpecial qLength)
      modNameD = indentName $ appSep $ docLit modNameT
      hidDocCol = if hiding then importCol - hidingParenCost else importCol - 2
      hidDocColDiff = importCol - 2 - hidDocCol
      hidDoc =
        if hiding then appSep $ docLit $ Text.pack "hiding" else docEmpty
      importHead = docSeq [importQualifiers, modNameD]
      bindingsD = case mllies of
        Nothing -> docEmpty
        Just (_, llies) -> do
          let llies' = toL llies
          hasComments <- hasAnyCommentsBelow llies'
          if compact
            then docAlt
              [ docSeq
                [ hidDoc
                , docForceSingleline $ layoutLLIEs True ShouldSortItems llies'
                ]
              , let
                  makeParIfHiding = if hiding
                    then docAddBaseY BrIndentRegular . docPar hidDoc
                    else id
                in makeParIfHiding (layoutLLIEs True ShouldSortItems llies')
              ]
            else do
              ieDs <- layoutAnnAndSepLLIEs ShouldSortItems llies'
              docWrapNodeRest llies'
                $ docEnsureIndent (BrIndentSpecial hidDocCol)
                $ case ieDs of
                  -- ..[hiding].( )
                    [] -> if hasComments
                      then docPar
                        (docSeq
                          [hidDoc, docParenLSep, docWrapNode llies' docEmpty]
                        )
                        (docEnsureIndent
                          (BrIndentSpecial hidDocColDiff)
                          docParenR
                        )
                      else docSeq
                        [hidDoc, docParenLSep, docSeparator, docParenR]
                    -- ..[hiding].( b )
                    [ieD] -> runFilteredAlternative $ do
                      addAlternativeCond (not hasComments)
                        $ docSeq
                            [ hidDoc
                            , docParenLSep
                            , docForceSingleline ieD
                            , docSeparator
                            , docParenR
                            ]
                      addAlternative $ docPar
                        (docSeq [hidDoc, docParenLSep, docNonBottomSpacing ieD])
                        (docEnsureIndent
                          (BrIndentSpecial hidDocColDiff)
                          docParenR
                        )
                    -- ..[hiding].( b
                    --            , b'
                    --            )
                    (ieD : ieDs') -> docPar
                      (docSeq [hidDoc, docSetBaseY $ docSeq [docParenLSep, ieD]]
                      )
                      (docEnsureIndent (BrIndentSpecial hidDocColDiff)
                      $ docLines
                      $ ieDs'
                      ++ [docParenR]
                      )
      makeAsDoc asT =
        docSeq [appSep $ docLit $ Text.pack "as", appSep $ docLit asT]
    if compact
      then
        let asDoc = maybe docEmpty makeAsDoc masT
        in
          docAlt
            [ docForceSingleline $ docSeq [importHead, asDoc, bindingsD]
            , docAddBaseY BrIndentRegular
              $ docPar (docSeq [importHead, asDoc]) bindingsD
            ]
      else case masT of
        Just n -> if enoughRoom
          then docLines [docSeq [importHead, asDoc], bindingsD]
          else docLines [importHead, asDoc, bindingsD]
         where
          enoughRoom = nameCost < importAsCol - asCost
          asDoc = docEnsureIndent (BrIndentSpecial (importAsCol - asCost))
            $ makeAsDoc n
        Nothing -> if enoughRoom
          then docSeq [importHead, bindingsD]
          else docLines [importHead, bindingsD]
          where enoughRoom = nameCost < importCol - hidingParenCost
  _ -> docEmpty
