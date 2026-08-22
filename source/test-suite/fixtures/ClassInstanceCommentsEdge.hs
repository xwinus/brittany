{-# LANGUAGE TypeFamilies #-}
class Container value where
  -- associated type comment
  type Item value

  -- default method comment
  item :: value -> Item value
  item = undefined
instance Container [value] where
  -- associated type instance comment
  type Item [value] = value

  -- implementation comment
  item = head
