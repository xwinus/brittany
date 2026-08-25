{-# LANGUAGE DataKinds #-}
instanceHeadAnchor = ()
instance
  ( FromJSON (c 'Partial)
  , Monoid (c 'Partial)
  )
  => FromJSON (PtPostProcessConfig c)
  where
    parseJSON = parser
