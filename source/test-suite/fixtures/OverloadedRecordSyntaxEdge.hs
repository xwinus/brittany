{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
module OverloadedRecordSyntaxEdge where
ownerName record = record.owner.name
renameOwner record =
  record
    { -- Keep the nested update field comment.
      owner.name = "new"
    }
useOwnerName record name = record { owner.name }
