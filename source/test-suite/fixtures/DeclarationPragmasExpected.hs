{-# LANGUAGE TemplateHaskell #-}
module DeclarationPragmasExpected where

{-# RULES "identity/value" forall value. identity value = value #-}
{-# ANN module ("module annotation" :: String) #-}
{-# ANN value ("value annotation" :: String) #-}
{-# WARNING oldValue "Use value" #-}
{-# DEPRECATED olderValue "Use value" #-}
identity value = value
value = ()
oldValue = value
olderValue = value
