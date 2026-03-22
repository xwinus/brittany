{-# LANGUAGE OrPatterns #-}
module Test543 where
f (Left x ; Right x) = x
g (Nothing ; Just Nothing) = True
g _                        = False
h (0 ; 1 ; 2) = True
h _           = False
