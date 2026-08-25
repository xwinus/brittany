module VerticalRecordExpected where

envFileSystem =
  FileSystem
    { fsCreateDirectory = undefined
    , fsDoesFileExist   = undefined
    , fsFindFiles       = undefined
    , fsGetPermissions  = undefined
    , fsWriteFile       = undefined
    }
