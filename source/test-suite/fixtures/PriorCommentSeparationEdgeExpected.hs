module PriorCommentSeparationEdge where

-- Adjacent comment.
data Adjacent = Adjacent

-- One blank line.

data OneGap = OneGap


-- Two blank lines before this comment.


data TwoGaps = TwoGaps

blockAdjacent = let { {- Block comment may share its source line. -} value = 1 } in value

-- First consecutive comment.
-- Second consecutive comment.

newtype Consecutive = Consecutive Int

nested =
  let
    -- First nested comment.
    -- Second nested comment.

    value = 1
  in value
