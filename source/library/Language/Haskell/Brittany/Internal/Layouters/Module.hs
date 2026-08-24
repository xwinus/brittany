{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Module where

import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as Text
import GHC (GenLocated(L), Located, getLoc, unLoc)
import GHC.Hs hiding (DeltaPos)
import qualified GHC.OldList as List
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.ExactPrintCompat (AnnKeywordId(..), Annotation(..), Comment(commentContents), DeltaPos(..), deltaRow, srcSpanToRealSpan)
import Language.Haskell.Brittany.Internal.ExactSource (nodeSourceSlice)
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.IE (layoutLLIEs, SortItemsFlag(KeepItemsUnsorted), toL)
import Language.Haskell.Brittany.Internal.Layouters.Import
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.TopLevelSpacing
import Language.Haskell.Brittany.Internal.Types



layoutModule :: ToBriDoc' (HsModule GhcPs)
layoutModule = layoutModuleWithExactText Text.empty

preambleRequiresExactSource :: Text.Text -> HsModule GhcPs -> Bool
preambleRequiresExactSource source HsModule{hsmodImports = imports} =
  containsWarningPragma || any importRequiresExactSource imports
 where
  containsWarningPragma = any (`Text.isInfixOf` source)
    [ Text.pack "{-# WARNING"
    , Text.pack "{-# DEPRECATED"
    ]
  importRequiresExactSource (L _ importDeclaration) = case importDeclaration of
    ImportDecl
      { ideclLevelSpec = levelStyle
      , ideclQualified = qualifiedStyle
      } -> levelStyle /= NotLevelled || qualifiedStyle == QualifiedPost
    XImportDecl{} -> True

layoutModuleWithExactText :: Text.Text -> ToBriDoc' (HsModule GhcPs)
layoutModuleWithExactText exactText lmod@(L _ mod') = case mod' of
    -- Implicit module Main
  HsModule{ hsmodName = Nothing, hsmodImports = imports } -> do
    commentedImports <- transformToCommentedImport imports
    hasModuleComments <- hasAnyCommentsPrior lmod
    -- groupify commentedImports `forM_` tellDebugMessShow
    docLines
      $ (if hasModuleComments then [docNodeAnnKW lmod Nothing docEmpty] else [])
      ++ (commentedImportsToDoc exactText <$> sortCommentedImports commentedImports)
    -- sortedImports <- sortImports imports
    -- docLines $ [layoutImport y i | (y, i) <- sortedImports]
  HsModule{ hsmodName = Just n, hsmodExports = les, hsmodImports = imports } -> do
    commentedImports <- transformToCommentedImport imports
    spacedImports <- preserveInitialImportSpacing mod' imports commentedImports
    -- groupify commentedImports `forM_` tellDebugMessShow
    -- sortedImports <- sortImports imports
    let tn = Text.pack $ moduleNameString $ unLoc n
    allowSingleLineExportList <-
      mAsk <&> _conf_layout .> _lconfig_allowSingleLineExportList .> confUnpack
    -- the config should not prevent single-line layout when there is no
    -- export list
    let allowSingleLine = allowSingleLineExportList || Data.Maybe.isNothing les
    docLines
      $ docSeq
          [ docNodeAnnKW lmod Nothing docEmpty
             -- A pseudo node that serves merely to force documentation
             -- before the node
          , docNodeMoveToKWDP lmod AnnModule True $ runFilteredAlternative $ do
            addAlternativeCond allowSingleLine $ docForceSingleline $ docSeq
              [ appSep $ docLit $ Text.pack "module"
              , appSep $ docLit tn
              , appSep $ case les of
                Nothing -> docEmpty
                Just x -> docWrapNode lmod $ layoutLLIEs True KeepItemsUnsorted (toL x)
              , docSeparator
              , docLit $ Text.pack "where"
              ]
            addAlternative $ docLines
              [ docAddBaseY BrIndentRegular $ docPar
                  (docSeq [appSep $ docLit $ Text.pack "module", docLit tn])
                  (docSeq
                    [ case les of
                      Nothing -> docEmpty
                      Just x -> docWrapNode lmod $ layoutLLIEs False KeepItemsUnsorted (toL x)
                    , docSeparator
                    , docLit $ Text.pack "where"
                    ]
                  )
              ]
          ]
      : (commentedImportsToDoc exactText <$> sortCommentedImports spacedImports) -- [layoutImport y i | (y, i) <- sortedImports]

preserveInitialImportSpacing
  :: HsModule GhcPs
  -> [LImportDecl GhcPs]
  -> [CommentedImport]
  -> ToBriDocM [CommentedImport]
preserveInitialImportSpacing mod' imports commentedImports = case imports of
  [] -> pure commentedImports
  firstImport : _ -> do
    let importNode = toL firstImport
    importAnn <- astAnn importNode
    let headerUnit = (`topLevelUnit` Nothing) <$> moduleWhereSpan mod'
        importUnit = do
          importSpan <- srcSpanToRealSpan $ getLoc importNode
          pure $ topLevelUnit importSpan importAnn
        blankLines = case (headerUnit, importUnit) of
          (Just previous, Just next) ->
            topLevelSeparatorLines previous next - 1
          _ -> 0
    pure $ replicate blankLines EmptyLine ++ commentedImports

data CommentedImport
  = EmptyLine
  | IndependentComment (Comment, DeltaPos)
  | ImportStatement ImportStatementRecord

instance Show CommentedImport where
  show = \case
    EmptyLine -> "EmptyLine"
    IndependentComment _ -> "IndependentComment"
    ImportStatement r ->
      "ImportStatement " ++ show (length $ commentsBefore r) ++ " " ++ show
        (length $ commentsAfter r)

data ImportStatementRecord = ImportStatementRecord
  { commentsBefore :: [(Comment, DeltaPos)]
  , commentsAfter :: [(Comment, DeltaPos)]
  , importStatement :: Located (ImportDecl GhcPs)
  }

instance Show ImportStatementRecord where
  show r =
    "ImportStatement " ++ show (length $ commentsBefore r) ++ " " ++ show
      (length $ commentsAfter r)

transformToCommentedImport
  :: [LImportDecl GhcPs] -> ToBriDocM [CommentedImport]
transformToCommentedImport is = do
  let isLoc = map toL is
  nodeWithAnnotations <- isLoc `forM` \i -> do
    annotionMay <- astAnn i
    pure (annotionMay, i)
  let
    convertComment (c, DP (y, x)) =
      replicate (y - 1) EmptyLine ++ [IndependentComment (c, DP (1, x))]
    accumF
      :: [(Comment, DeltaPos)]
      -> (Maybe Annotation, Located (ImportDecl GhcPs))
      -> ([(Comment, DeltaPos)], [CommentedImport])
    accumF accConnectedComm (annMay, declaration) = case annMay of
      Nothing ->
        ( []
        , [ ImportStatement ImportStatementRecord
              { commentsBefore = []
              , commentsAfter = []
              , importStatement = declaration
              }
          ]
        )
      Just ann ->
        let
          rawBlanksBeforeImportDecl = deltaRow (annEntryDelta ann) - 1
          (newAccumulator, priorComments') =
            List.span ((== 0) . deltaRow . snd) (annPriorComments ann)
          -- Adjust blanks: prior comments consume some of the delta rows
          -- between the previous sibling and this import
          priorCommentRows = sum $ map (max 1 . deltaRow . snd) priorComments'
          blanksBeforeImportDecl = max 0 (rawBlanksBeforeImportDecl - priorCommentRows)
          go
            :: [(Comment, DeltaPos)]
            -> [(Comment, DeltaPos)]
            -> ([CommentedImport], [(Comment, DeltaPos)], Int)
          go acc [] = ([], acc, 0)
          go acc [c1@(_, DP (y, _))] = ([], c1 : acc, y - 1)
          go acc (c1@(_, DP (1, _)) : xs) = go (c1 : acc) xs
          go acc ((c1, DP (y, x)) : xs) =
            ( (convertComment =<< xs) ++ replicate (y - 1) EmptyLine
            , (c1, DP (1, x)) : acc
            , 0
            )
          (convertedIndependentComments, beforeComments, initialBlanks) =
            if blanksBeforeImportDecl /= 0
              then (convertComment =<< priorComments', [], 0)
              else go [] (reverse priorComments')
        in
          ( newAccumulator
          , convertedIndependentComments
          ++ replicate (blanksBeforeImportDecl + initialBlanks) EmptyLine
          ++ [ ImportStatement ImportStatementRecord
                 { commentsBefore = beforeComments
                 , commentsAfter = accConnectedComm ++ annFollowingComments ann
                 , importStatement = declaration
                 }
             ]
          )
  let (finalAcc, finalList) = mapAccumR accumF [] nodeWithAnnotations
  pure $ join $ (convertComment =<< finalAcc) : finalList

sortCommentedImports :: [CommentedImport] -> [CommentedImport]
sortCommentedImports =
  unpackImports . mergeGroups . map (fmap (sortGroups)) . groupify
 where
  unpackImports :: [CommentedImport] -> [CommentedImport]
  unpackImports xs = xs >>= \case
    l@EmptyLine -> [l]
    l@IndependentComment{} -> [l]
    ImportStatement r ->
      map IndependentComment (commentsBefore r) ++ [ImportStatement r]
  mergeGroups
    :: [Either CommentedImport [ImportStatementRecord]] -> [CommentedImport]
  mergeGroups xs = xs >>= \case
    Left x -> [x]
    Right y -> ImportStatement <$> y
  sortGroups :: [ImportStatementRecord] -> [ImportStatementRecord]
  sortGroups =
    List.sortOn
      (moduleNameString . unLoc . ideclName . unLoc . importStatement)
  groupify
    :: [CommentedImport] -> [Either CommentedImport [ImportStatementRecord]]
  groupify cs = go [] cs
   where
    go [] = \case
      (l@EmptyLine : rest) -> Left l : go [] rest
      (l@IndependentComment{} : rest) -> Left l : go [] rest
      (ImportStatement r : rest) -> go [r] rest
      [] -> []
    go acc = \case
      (l@EmptyLine : rest) -> Right (reverse acc) : Left l : go [] rest
      (l@IndependentComment{} : rest) ->
        Left l : Right (reverse acc) : go [] rest
      (ImportStatement r : rest) -> go (r : acc) rest
      [] -> [Right (reverse acc)]

commentedImportsToDoc :: Text.Text -> CommentedImport -> ToBriDocM BriDocNumbered
commentedImportsToDoc exactText = \case
  EmptyLine -> docLitS ""
  IndependentComment c -> commentToDoc c
  ImportStatement r -> do
    let importNode = importStatement r
    exactImportText <- case (Text.null exactText, ideclImportList $ unLoc importNode) of
      (False, Just (_, llies)) -> do
        let listNode = toL llies
        hasComments <- hasAnyRegularCommentsConnected listNode
        if hasComments
          then do
            listAnns <- filterAnns listNode <$> mAsk
            pure $ case nodeSourceSlice exactText importNode listAnns of
              Just source -> Just (source, Map.keysSet listAnns)
              Nothing -> Nothing
          else pure Nothing
      _ -> pure Nothing
    docSeq
      ( layoutImportWithExactText exactImportText importNode
      : map commentToDoc (commentsAfter r)
      )
 where
  commentToDoc (c, DP (_y, x)) = docLitS (replicate x ' ' ++ commentContents c)
