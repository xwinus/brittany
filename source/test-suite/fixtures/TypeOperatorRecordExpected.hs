{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module TypeOperatorRecord where

data Phase
  = Before
  | After

type phase ::: value = value

data PostProcessConfig (phase :: Phase) config = PostProcessConfig
  { ppcEnabled :: phase ::: Bool
  , ppcConfig  :: config phase
  }
