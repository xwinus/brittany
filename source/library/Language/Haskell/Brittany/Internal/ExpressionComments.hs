{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.ExpressionComments
  ( commentSensitiveExpressions
  , exactSourceExpressions
  , isCommentSensitiveExpression
  , requiresExactSourceExpression
  ) where

import Data.Data (Data)
import qualified Data.Generics as SYB
import GHC (GenLocated(L), Located, unLoc)
import GHC.Hs
  ( HsExpr(..)
  , HsDoFlavour(..)
  , LHsExpr
  , LHsRecUpdFields(..)
  )
import GHC.Parser.Annotation (getLocA)
import Language.Haskell.Brittany.Internal.Prelude

-- | Find expression shapes whose GHC 9.14 comment annotations cannot be laid
-- out idempotently. Their containing declaration must retain its exact source.
commentSensitiveExpressions
  :: Data ast => ast -> [Located (HsExpr GhcPs)]
commentSensitiveExpressions = collectExpressions isCommentSensitiveExpression

-- | Find expression shapes that currently require exact-source rendering even
-- without comments.
exactSourceExpressions
  :: Data ast => ast -> [Located (HsExpr GhcPs)]
exactSourceExpressions = collectExpressions requiresExactSourceExpression

collectExpressions
  :: Data ast
  => (HsExpr GhcPs -> Bool)
  -> ast
  -> [Located (HsExpr GhcPs)]
collectExpressions predicate = SYB.everything (++) expressionQuery
 where
  expressionQuery :: SYB.GenericQ [Located (HsExpr GhcPs)]
  expressionQuery = const [] `SYB.extQ` collectExpression

  collectExpression :: LHsExpr GhcPs -> [Located (HsExpr GhcPs)]
  collectExpression expression
    | predicate $ unLoc expression =
        [L (getLocA expression) (unLoc expression)]
    | otherwise = []

isCommentSensitiveExpression :: HsExpr GhcPs -> Bool
isCommentSensitiveExpression = \case
  HsMultiIf{} -> True
  HsLet{} -> True
  HsPar _ expression -> containsOperatorApplication expression
  RecordCon{} -> True
  RecordUpd{} -> True
  HsGetField{} -> True
  HsProjection{} -> True
  _ -> False

requiresExactSourceExpression :: HsExpr GhcPs -> Bool
requiresExactSourceExpression = \case
  ExplicitSum{} -> True
  HsForAll{} -> True
  HsFunArr{} -> True
  HsPragE{} -> True
  HsQual{} -> True
  HsDo _ (DoExpr (Just _)) _ -> True
  HsDo _ (MDoExpr (Just _)) _ -> True
  RecordUpd _ _ (OverloadedRecUpdFields _ _) -> True
  _ -> False

containsOperatorApplication :: LHsExpr GhcPs -> Bool
containsOperatorApplication = SYB.everything (||) operatorQuery
 where
  operatorQuery :: SYB.GenericQ Bool
  operatorQuery = const False `SYB.extQ` isOperatorApplication

  isOperatorApplication :: LHsExpr GhcPs -> Bool
  isOperatorApplication expression = case unLoc expression of
    OpApp{} -> True
    _ -> False
