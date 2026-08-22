{-# LANGUAGE TypeFamilies #-}
import           Data.Kind                                ( Type )
type family Element container
  -- result kind comment
  :: Type
data family Payload container
  -- data family result kind comment
  :: Type
type instance Element [value]
  -- equation comment
  = value
data instance Payload [value]
  -- constructor comment
  = Payload value
