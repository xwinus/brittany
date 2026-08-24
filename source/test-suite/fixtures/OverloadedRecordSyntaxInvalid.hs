{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
module OverloadedRecordSyntaxInvalid where

broken record = record { owner. = "new" }
