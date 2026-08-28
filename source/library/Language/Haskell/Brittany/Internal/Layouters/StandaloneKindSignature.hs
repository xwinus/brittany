{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.StandaloneKindSignature
  ( layoutStandaloneKindSignature
  ) where

import qualified Data.Map                                as Map
import           GHC                                      ( GenLocated(L) )
import           GHC.Hs
import           GHC.Types.SrcLoc                         ( getLoc
                                                          , srcSpanEndCol
                                                          , srcSpanEndLine
                                                          , srcSpanStartCol
                                                          , srcSpanStartLine
                                                          )
import           Language.Haskell.Brittany.Internal.ExactPrintCompat
                                                          ( realSpanToSrcSpan
                                                          , srcSpanToRealSpan
                                                          )
import           Language.Haskell.Brittany.Internal.ExactSource
                                                          ( sourceCommentFragment
                                                          )
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Layouters.IE
                                                          ( toL )
import           Language.Haskell.Brittany.Internal.Layouters.Type
import           Language.Haskell.Brittany.Internal.Prelude
import           Language.Haskell.Brittany.Internal.SourceComment.Types
import           Language.Haskell.Brittany.Internal.Types

layoutStandaloneKindSignature :: ToBriDoc StandaloneKindSig
layoutStandaloneKindSignature signature@(L _ (StandaloneKindSig _ name kindSignature))
  = docWrapNode signature $ do
    nameText <- applyNameAdornment name <$> lrdrNameToTextAnn (toL name)
    let lhs =
          docSeq
            [appSep $ docLitS "type", docWrapNode (toL name) $ docLit nameText]
    sharedLhs   <- docSharedWrapper id lhs
    kindDoc     <- docSharedWrapper layoutKindSignature kindSignature
    hasComments <- hasAnyCommentsBelow $ toL kindSignature
    layoutLhsAndKind hasComments sharedLhs kindDoc

layoutKindSignature :: LHsSigType GhcPs -> ToBriDocM BriDocNumbered
layoutKindSignature signature@(L _ (HsSig _ binders body)) =
  docWrapNode (toL signature) $ do
    bodyDoc <- docSharedWrapper layoutType $ toL body
    case binders of
      HsOuterImplicit{}           -> bodyDoc
      HsOuterExplicit _ variables -> do
        variableDocs   <- layoutTyVarBndrs variables
        binderComments <- commentsBetweenBindersAndBody variables body
        let
          binderCommentDocs = layoutSourceComment <$> binderComments
          forallPrefix =
            docSeq $ docLitS "forall" : processTyVarBndrsSingleline variableDocs
          multilineBody = case body of
            L _ HsFunTy{} -> docForceMultiline bodyDoc
            _             -> bodyDoc
          compact =
            docSeq [forallPrefix, docLitS ". ", docForceSingleline bodyDoc]
          multilineWithoutComments =
            docLines [docSeq [forallPrefix, docLitS "."], multilineBody]
          multilineWithComments =
            docLines
              $  [forallPrefix]
              ++ (docEnsureIndent BrIndentRegular <$> binderCommentDocs)
              ++ [ docCols
                     ColTyOpPrefix
                     [ docLitS " . "
                     , docAddBaseY (BrIndentSpecial 3) multilineBody
                     ]
                 ]
        if null binderComments
          then docAlt [compact, multilineWithoutComments]
          else multilineWithComments

commentsBetweenBindersAndBody
  :: [LHsTyVarBndr flag GhcPs] -> LHsType GhcPs -> ToBriDocM [SourceComment]
commentsBetweenBindersAndBody variables body = do
  commentPlan <- mAsk
  pure $ case reverse variables of
    [] -> []
    lastVariable : _ ->
      case
          ( srcSpanToRealSpan $ getLoc $ toL lastVariable
          , srcSpanToRealSpan $ getLoc $ toL body
          )
        of
          (Just variableSpan, Just bodySpan) ->
            filter (isBetween variableSpan bodySpan)
              $ Map.elems
              $ commentPlanSources commentPlan
          _ -> []
 where
  isBetween variableSpan bodySpan sourceComment =
    (srcSpanEndLine variableSpan, srcSpanEndCol variableSpan)
      <= ( srcSpanStartLine $ sourceCommentSpan sourceComment
         , srcSpanStartCol $ sourceCommentSpan sourceComment
         )
      && ( srcSpanEndLine $ sourceCommentSpan sourceComment
         , srcSpanEndCol $ sourceCommentSpan sourceComment
         )
      <= (srcSpanStartLine bodySpan, srcSpanStartCol bodySpan)

layoutSourceComment :: SourceComment -> ToBriDocM BriDocNumbered
layoutSourceComment sourceComment = briDocBySourceFragmentNoComment
  (L (realSpanToSrcSpan $ sourceCommentSpan sourceComment) sourceComment)
  (sourceCommentFragment sourceComment)

layoutLhsAndKind
  :: Bool
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
layoutLhsAndKind hasComments lhs kindDoc = runFilteredAlternative $ do
  addAlternativeCond (not hasComments) $ docSeq
    [lhs, docSeparator, docLitS "::", docSeparator, docForceSingleline kindDoc]
  addAlternative $ docAddBaseY BrIndentRegular $ docPar lhs $ docCols
    ColTyOpPrefix
    [appSep $ docLitS "::", docAddBaseY (BrIndentSpecial 3) kindDoc]
