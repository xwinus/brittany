{-# LANGUAGE NoFieldSelectors #-}
module NoFieldSelectorsExpected where
data Label = Label
  { label :: String
  }
label = "fallback"
construct value = Label { label = value }
readLabel Label { label = value } = value
