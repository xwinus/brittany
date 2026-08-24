{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE UnliftedNewtypes #-}
module UnliftedSyntaxEdge where
import           Data.Kind                                ( Type )
import           GHC.Exts                                 ( Int#
                                                          , UnliftedType
                                                          )
newtype RawInt =
  RawInt
    -- Keep the unlifted newtype field comment.
    Int#
data UList a :: UnliftedType where
  UNil
    :: -- Keep the unlifted constructor signature comment.
       UList a
  UCons :: a -> UList a -> UList a
type UPair :: Type -> UnliftedType
data UPair a =
  UPair
    -- Keep the unlifted data field comment.
    a
    a
swap
  :: (# Int#
      -- Keep the tuple type comment.
      , Int# #)
  -> (# Int#, Int# #)
swap
  (# left
   -- Keep the tuple pattern comment.
   , right #) =
  (# right
   -- Keep the tuple expression comment.
   , left #)
select
  :: (# Int#
      -- Keep the sum type comment.
      | Bool #)
  -> Bool
select
  (# |
     -- Keep the sum pattern comment.
     value #) = value
select (# _ | #) = False
