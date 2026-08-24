{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecursiveDo #-}
module DoSyntaxExpected where
import qualified Prelude                                 as P
consume action = action
block = consume do
  value <- Just 1
  pure value
qualified = P.do
  value <- Just 1
  P.return value
qualifiedRecursive = P.mdo
  values <- Just (1 : values)
  P.return values
recursive = mdo
  values <- Just (1 : values)
  pure values
recursiveStatement = do
  rec first  <- Just second
      second <- Just first
  pure (first, second)
