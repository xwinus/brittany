{-# LANGUAGE MultiWayIf #-}
choose value = if
  | value > 0 -- positive guard
              -> value -- positive result
  | otherwise -> 0 -- fallback
