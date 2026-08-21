{-# LANGUAGE RequiredTypeArguments #-}
module Test546 where
f :: forall a -> a -> a
f (type t) x = x :: t
