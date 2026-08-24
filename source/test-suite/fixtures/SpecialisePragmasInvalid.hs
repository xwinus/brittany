{-# LANGUAGE TypeApplications #-}
module SpecialisePragmasInvalid where

identity value = value
{-# SPECIALISE identity @Int :: #-}
