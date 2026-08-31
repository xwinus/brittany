{-# LANGUAGE LambdaCase #-}
multiple = \case
  Nothing -> 0
  Just value | value > 0 -> value
             | otherwise -> 0
commented = \case
  Nothing -> 0
  -- preserve the alternative comment
  Just value -> value
nested = \case
  Nothing -> \case
    Nothing    -> 0
    Just value -> value
  Just value -> \case
    Nothing          -> value
    Just nestedValue -> nestedValue
empty = \case {}
