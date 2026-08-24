{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE ViewPatterns #-}
module StrictPatternSyntaxEdge where
strictIdentity
  ( -- Keep the bang-pattern boundary comment.
    !value
  ) = value
lazyIdentity
  ( -- Keep the lazy-pattern boundary comment.
    ~value
  ) = value
isEmpty
  ( length
    -- Keep the view-pattern arrow comment.
    -> 0
  ) = True
isEmpty _ = False
