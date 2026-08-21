{-# LANGUAGE OverloadedRecordDot #-}
module Test541 where
f x = x.field
g x = x.field.subfield
h x = (foo x).bar.baz
