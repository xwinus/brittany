{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Expr.OperatorChainComments
  ( gatherOperatorChain, operatorEndsLine ) where

import GHC (GenLocated(L))
import GHC.Types.SrcLoc (getLoc, realSrcSpanStart, realSrcSpanEnd, srcSpanStartLine, srcSpanEndLine)
import qualified Data.List as List
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as EP
import GHC.Hs
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.IE (toL)
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types

-- Flatten only the parser's left OpApp spine. Each removed prefix keeps its
-- prior comments before the first operand and its rest comments after its last
-- operand. Parentheses and operand scopes remain separate layout documents.
-- Comment-free chains retain the original documents without empty wrappers.
gatherOperatorChain
  :: Bool
  -> (LHsExpr GhcPs -> ToBriDocM BriDocNumbered)
  -> (LHsExpr GhcPs -> ToBriDocM BriDocNumbered)
  -> LHsExpr GhcPs
  -> ToBriDocM
       ( ToBriDocM BriDocNumbered
       , [(LHsExpr GhcPs, LHsExpr GhcPs, ToBriDocM BriDocNumbered)]
       )
gatherOperatorChain preserveAnnotations layoutFirst layoutOperand expression = do
  (first, parts) <- gather expression
  pure (first, parts [])
 where
  gather expression@(L _ (OpApp _ left operator right)) = do
    (first, parts) <- gather left
    first' <- if preserveAnnotations
      then docSharedWrapper (docWrapNodePrior $ toL expression) first
      else pure first
    right' <- docSharedWrapper
      ((if preserveAnnotations then docWrapNodeRest (toL expression) else id)
        . layoutOperand) right
    pure (first', parts . ((operator, right, right') :))
  gather operand = do
    first <- docSharedWrapper layoutFirst operand
    pure (first, id)

-- Preserve a physical boundary after an operator comment, including a block
-- comment followed by a source newline. Moving its operand onto the comment's
-- row can change comment ownership on the next formatting pass.
operatorEndsLine :: LHsExpr GhcPs -> LHsExpr GhcPs -> ToBriDocM Bool
operatorEndsLine operator operand = do
  comments <- astConnectedComments $ toL operator
  pure $ case (sourceSpan operator, sourceSpan operand) of
    (Just operatorSpan, Just operandSpan) ->
      any (follows operatorSpan operandSpan . fst) comments
    _ -> False
 where
  sourceSpan = EP.srcSpanToRealSpan . getLoc . toL
  follows operatorSpan operandSpan comment = case
      EP.srcSpanToRealSpan $ EP.commentIdentifier comment of
    Nothing -> False
    Just commentSpan -> realSrcSpanStart commentSpan >= realSrcSpanEnd operatorSpan
      && realSrcSpanEnd commentSpan <= realSrcSpanStart operandSpan
      && ("--" `List.isPrefixOf` EP.commentContents comment
        || srcSpanEndLine commentSpan < srcSpanStartLine operandSpan)
