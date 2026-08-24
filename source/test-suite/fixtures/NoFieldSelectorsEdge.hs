{-# LANGUAGE NoFieldSelectors #-}
module NoFieldSelectorsEdge where
data Label = Label
  { label :: String
  }
label = "fallback"
construct value =
  Label
    { -- Keep the field comment without generating a selector.
      label = value
    }
