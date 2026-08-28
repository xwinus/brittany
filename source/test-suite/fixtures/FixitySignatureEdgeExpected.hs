module FixitySignatureEdge where

-- | Keep the declaration documentation.
infixl 0 <+>, `append`
infixr 9 <.>,
  -- Keep the comment before the backticked name.
  `compose`
infix 1 <#>,
  {- Keep the block comment. -}
  `hash`
infix 5 `compareLike` -- Keep the trailing comment.
