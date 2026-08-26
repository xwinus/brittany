{-# LANGUAGE DataKinds #-}
module SignaturePostDocUnsupported where

scoped
    :: '(Int, Bool)
    -- ^ promoted tuple result
scoped = undefined

ambiguous
    :: Int
    -- Keep this ordinary comment at exact source.
    -> Bool
ambiguous = undefined
