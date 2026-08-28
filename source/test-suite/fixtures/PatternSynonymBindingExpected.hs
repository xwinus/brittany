{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PatternSynonymBindingExpected where

pattern LineModeValid :: forall t . t -> LineModeValidity t
pattern LineModeValid value =
  LineModeValidity (StrictJust value) :: LineModeValidity t
pattern LineModeInvalid :: forall t . LineModeValidity t
pattern LineModeInvalid = LineModeValidity StrictNothing :: LineModeValidity t
