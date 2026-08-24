{-# LANGUAGE TypeApplications #-}
module SpecialisePragmasExpected where
identity value = value
convert flag value = if flag then 'y' else 'n'
{-# INLINE identity #-}
{-# OPAQUE convert #-}
{-# SPECIALISE identity @Int :: Int -> Int #-}
{-# SPECIALISE convert @Int False :: Int -> Char #-}
profiled value = {-# SCC "profiled" #-} value
