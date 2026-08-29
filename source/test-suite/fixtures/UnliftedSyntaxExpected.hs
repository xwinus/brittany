{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE UnliftedNewtypes #-}
module UnliftedSyntaxExpected where
import           Data.Kind                                ( Type )
import           GHC.Exts                                 ( Int#
                                                          , UnliftedType
                                                          )
newtype RawInt = RawInt Int#
data UList a :: UnliftedType where
  UNil :: UList a
  UCons :: a -> UList a -> UList a
type UPair :: Type -> UnliftedType
data UPair a = UPair a a
swap :: (# Int#, Int# #) -> (# Int#, Int# #)
swap (# left, right #) = (# right, left #)
select :: (# Int# | Bool #) -> Bool
select (# | value #) = value
select (# _ | #)     = False
