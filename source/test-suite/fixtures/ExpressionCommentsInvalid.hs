{-# LANGUAGE MultiWayIf #-}

broken value = if
  | value > 0 -- missing branch body
