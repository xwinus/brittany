{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
module OverloadedRecordSyntaxExpected where
ownerName record = record.owner.name
projectOwnerName = (.owner.name)
renameOwner record = record { owner.name = "new" }
