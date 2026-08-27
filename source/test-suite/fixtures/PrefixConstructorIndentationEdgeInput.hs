{-# LANGUAGE BangPatterns #-}
module PrefixConstructorIndentationEdge where

data One = One Int deriving Show

data Two = Two Int Bool deriving Show

data Many = ConstructorWithANameLongEnoughToRequireStructuralLayout Int Bool Char String Double
  deriving Show

-- | Declaration documentation.
data Documented =
  -- | Constructor documentation.
  DocumentedConstructorWithANameLongEnoughToRequireStructuralLayout
    ~Int
    !Bool
    {-# UNPACK #-} !Int
    {-# NOUNPACK #-} !Int
  deriving Show

data Commented
  = CommentedConstructorWithANameLongEnoughToRequireStructuralLayout
      Int
      -- Keep this comment with the strict field.
      !Bool
      {-# UNPACK #-} !Int
  deriving Show
