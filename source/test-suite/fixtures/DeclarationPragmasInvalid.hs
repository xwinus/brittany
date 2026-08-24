{-# LANGUAGE TemplateHaskell #-}
module DeclarationPragmasInvalid where

{-# RULES "broken" forall value. identity value #-}
