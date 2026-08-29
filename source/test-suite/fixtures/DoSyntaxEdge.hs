{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecursiveDo #-}
module DoSyntaxEdge where
import qualified Prelude                                 as P
consume action = action
block =
  consume do
    -- Keep the block argument comment.
    value <- Just 1
    pure value
qualified =
  P.do
  -- Keep the qualified do comment.
  value <- Just 1
  P.return value
qualifiedRecursive =
  P.mdo
  -- Keep the qualified recursive do comment.
  values <- Just (1 : values)
  P.return values
recursive =
  mdo
    -- Keep the recursive do comment.
    values <- Just (1 : values)
    pure values
recursiveStatement =
  do
    rec
        -- Keep the recursive statement comment.
        first  <- Just second
        second <- Just first
    pure (first, second)
