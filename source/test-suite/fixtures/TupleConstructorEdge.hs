{-# LANGUAGE UnboxedTuples #-}
triple = (,,) <$> Just 1 <*> Just 2 <*> Just 3
pairMapper = map (,)
-- preserve explicit parentheses without growing them
redundantPair = ((,))
symbolicControl = (+)
unboxedPair = (#,#)
