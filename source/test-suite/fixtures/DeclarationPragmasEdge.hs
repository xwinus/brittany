{-# LANGUAGE TemplateHaskell #-}
module DeclarationPragmasEdge where

{-# RULES
  "identity/value"
    -- Keep the rule binder comment.
    forall value. identity value = value
  #-}
{-# ANN
  -- Keep the annotation target comment.
  value
  ("value annotation" :: String)
  #-}
{-# WARNING
  oldValue
  -- Keep the warning pragma comment.
  "Use value"
  #-}
identity value = value
value = ()
oldValue = value
