{-# LANGUAGE StrictData #-}
module Main where

data Env = Env
  { lazyField :: ~Int
  }

main :: IO ()
main = case Env undefined of
  Env{} -> putStrLn "constructed"
