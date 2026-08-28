{-# LANGUAGE StandaloneDeriving #-}
module TupleSemanticNameInvalid where

data BriDocF f = BriDocF
deriving instance Eq (BriDocF ((,) Int)
