{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Brittany.Internal.Layouters.Stmt where

import qualified Data.Semigroup as Semigroup
import qualified Data.Text as Text
import GHC (GenLocated(L), unLoc)
import GHC.Hs
import Language.Haskell.Brittany.Internal.Config.Types
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as ExactPrintCompat
import Language.Haskell.Brittany.Internal.Fallbacks (FallbackId(..))
import Language.Haskell.Brittany.Internal.Layouters.IE (toL)
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.Decl
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Expr
import Language.Haskell.Brittany.Internal.Layouters.Pattern
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.TopLevelSpacing
import Language.Haskell.Brittany.Internal.Types



layoutStmtList
  :: [ExprLStmt GhcPs]
  -> ToBriDocM BriDocNumbered
layoutStmtList stmts = do
  stmtDocs <- docSharedWrapper layoutStmt `mapM` (map toL stmts)
  stmtUnits <- forM stmts $ \stmt -> do
    annotation <- astAnn $ toL stmt
    pure $ (`topLevelUnit` annotation)
      <$> ExactPrintCompat.srcSpanToRealSpan (getLocA stmt)
  docLines $ addSourceSeparators stmtDocs stmtUnits
 where
  addSourceSeparators [] [] = []
  addSourceSeparators (doc : docs) (unit : units) =
    doc : go unit docs units
  addSourceSeparators docs _ = docs

  go _ [] [] = []
  go previous (doc : docs) (current : units) =
    replicate (separatorLines previous current - 1) docBlankLine
      ++ (doc : go current docs units)
  go _ docs _ = docs

  separatorLines (Just previous) (Just current)
    = topLevelSeparatorLines previous current
  separatorLines _ _ = 1

layoutStmt :: ToBriDoc' (StmtLR GhcPs GhcPs (LHsExpr GhcPs))
layoutStmt lstmt@(L _ stmt) = do
  indentPolicy <- mAsk <&> _conf_layout .> _lconfig_indentPolicy .> confUnpack
  indentAmount :: Int <-
    mAsk <&> _conf_layout .> _lconfig_indentAmount .> confUnpack
  docWrapNode lstmt $ case stmt of
    LastStmt _ body Nothing _ -> do
      layoutExpr (toL body)
    BindStmt _ lPat expr -> do
      patDoc <- fmap pure $ patternDocument =<< layoutPattern lPat
      expDoc <- docSharedWrapper layoutExpr (toL expr)
      docAlt
        [ docCols
          ColBindStmt
          [ appSep patDoc
          , docSeq
            [ appSep $ docLit $ Text.pack "<-"
            , docAddBaseY BrIndentRegular $ docForceParSpacing expDoc
            ]
          ]
        , docCols
          ColBindStmt
          [ appSep patDoc
          , docAddBaseY BrIndentRegular
            $ docPar (docLit $ Text.pack "<-") (expDoc)
          ]
        ]
    LetStmt _ binds -> do
      let isFree = indentPolicy == IndentPolicyFree
      let indentFourPlus = indentAmount >= 4
      let locatedBinds = L (localBindsSpan binds) binds
      letComments <- filter (sourceCommentPrecedesNode locatedBinds)
        <$> sourceCommentsWithinNode lstmt
      let letDoc = appendSourceComments
            (docLit $ Text.pack "let")
            letComments
          commentedBindDocs bindDocs = prependConsumedComments letComments
            $ docAddBaseY BrIndentRegular
            $ docPar letDoc
            $ docSetBaseAndIndent
            $ docLines
            $ return <$> bindDocs
      layoutLocalBinds locatedBinds >>= \case
        Nothing -> prependConsumedComments letComments letDoc
          -- i just tested the above, and it is indeed allowed. heh.
        Just [] -> prependConsumedComments letComments letDoc
        -- let bind = expr
        Just [bindDoc] | not (null letComments) ->
          commentedBindDocs [bindDoc]
        Just [bindDoc] -> docAlt
          [ docCols
            ColDoLet
            [ appSep $ docLit $ Text.pack "let"
            , let
                f = case indentPolicy of
                  IndentPolicyFree -> docSetBaseAndIndent
                  IndentPolicyLeft -> docForceSingleline
                  IndentPolicyMultiple
                    | indentFourPlus -> docSetBaseAndIndent
                    | otherwise -> docForceSingleline
              in f $ return bindDoc
            ]
          , -- let
              --   bind = expr
            docAddBaseY BrIndentRegular $ docPar
            (docLit $ Text.pack "let")
            (docSetBaseAndIndent $ return bindDoc)
          ]
        Just bindDocs | not (null letComments) ->
          commentedBindDocs bindDocs
        Just bindDocs -> runFilteredAlternative $ do
          -- let aaa = expra
          --     bbb = exprb
          --     ccc = exprc
          addAlternativeCond (isFree || indentFourPlus) $ docSeq
            [ appSep $ docLit $ Text.pack "let"
            , let
                f = if indentFourPlus
                  then docEnsureIndent BrIndentRegular
                  else docSetBaseAndIndent
              in f $ docLines $ return <$> bindDocs
            ]
          -- let
          --   aaa = expra
          --   bbb = exprb
          --   ccc = exprc
          addAlternativeCond (not indentFourPlus)
            $ docAddBaseY BrIndentRegular
            $ docPar
                (docLit $ Text.pack "let")
                (docSetBaseAndIndent $ docLines $ return <$> bindDocs)
    RecStmt _ stmts _ _ _ _ _ -> do
      stmtListDoc <- docSharedWrapper layoutStmtList $ unLoc stmts
      runFilteredAlternative $ do
        -- rec stmt1
        --     stmt2
        --     stmt3
        addAlternativeCond (indentPolicy == IndentPolicyFree) $ docSeq
          [ docLit (Text.pack "rec")
          , docSeparator
          , docSetBaseAndIndent stmtListDoc
          ]
        -- rec
        --   stmt1
        --   stmt2
        --   stmt3
        addAlternative $ docAddBaseY BrIndentRegular $ docPar
          (docLit (Text.pack "rec"))
          stmtListDoc
    BodyStmt _ expr _ _ -> do
      expDoc <- docSharedWrapper layoutExpr (toL expr)
      docAddBaseY BrIndentRegular $ expDoc
    _ -> briDocByExactInlineOnly StatementFallback lstmt
