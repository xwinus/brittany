{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.ExpressionComments
  ( commentSensitiveExpressions
  , exactSourceExpressions
  ) where

import Data.Data (Data)
import qualified Data.Generics as SYB
import GHC (GenLocated(L), Located, unLoc)
import GHC.Hs (HsExpr(..), LHsExpr, LHsRecUpdFields(..))
import GHC.Parser.Annotation (getLocA)
import Language.Haskell.Brittany.Internal.Prelude

-- | Find expression shapes whose GHC 9.14 comment annotations cannot be laid
-- out idempotently. Their containing declaration must retain its exact source.
commentSensitiveExpressions
  :: Data ast => ast -> [Located (HsExpr GhcPs)]
commentSensitiveExpressions = collectExpressions isCommentSensitive

-- | Find expression shapes that currently require exact-source rendering even
-- without comments.
exactSourceExpressions
  :: Data ast => ast -> [Located (HsExpr GhcPs)]
exactSourceExpressions = collectExpressions requiresExactSource

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

isCommentSensitive :: HsExpr GhcPs -> Bool
isCommentSensitive = \case
  HsMultiIf{} -> True
  HsLet{} -> True
  HsPar _ expression -> containsOperatorApplication expression
  RecordCon{} -> True
  RecordUpd{} -> True
  HsGetField{} -> True
  HsProjection{} -> True
  _ -> False

requiresExactSource :: HsExpr GhcPs -> Bool
requiresExactSource = \case
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
