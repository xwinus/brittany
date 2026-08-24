{-# LANGUAGE TypeApplications #-}
module SpecialisePragmasEdge where
convert flag value = if flag then 'y' else 'n'
{-# SPECIALISE
  convert
    -- Keep the specialised expression comment.
    @Int False
  :: Int -> Char
  #-}
profiled value =
  {-# SCC "profiled" #-}
  -- Keep the expression pragma comment.
  value
