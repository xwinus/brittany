{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.TransactionalWrite
  ( FileOperations(..)
  , PlannedWrite(..)
  , TransactionError(..)
  , defaultFileOperations
  , transactionalWrite
  , transactionalWriteWith
  ) where

import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import Data.Kind (Type)
import Language.Haskell.Brittany.Internal.Prelude
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import System.IO (FilePath)
import qualified System.IO.Error as IOError

type PlannedWrite :: Type
data PlannedWrite = PlannedWrite
  { plannedTarget :: FilePath
  , plannedOriginal :: ByteString.ByteString
  , plannedReplacement :: ByteString.ByteString
  , plannedPermissions :: Directory.Permissions
  }

type TransactionError :: Type
data TransactionError
  = StagingFailure FilePath String
  | ConcurrentModification FilePath
  | CommitFailure FilePath String
  | RollbackFailure FilePath String String
  deriving (Eq, Show)

type FileOperations :: Type
data FileOperations = FileOperations
  { operationCreateTemp :: FilePath -> String -> IO FilePath
  , operationWriteFile :: FilePath -> ByteString.ByteString -> IO ()
  , operationSetPermissions
      :: FilePath -> Directory.Permissions -> IO ()
  , operationReadFile :: FilePath -> IO ByteString.ByteString
  , operationRenameFile :: FilePath -> FilePath -> IO ()
  , operationRemoveFile :: FilePath -> IO ()
  , operationBeforeCommit :: IO ()
  }

type StagedWrite :: Type
data StagedWrite = StagedWrite
  { stagedPlan :: PlannedWrite
  , stagedReplacementPath :: FilePath
  , stagedBackupPath :: FilePath
  }

defaultFileOperations :: FileOperations
defaultFileOperations = FileOperations
  { operationCreateTemp = createSiblingTemp
  , operationWriteFile = ByteString.writeFile
  , operationSetPermissions = Directory.setPermissions
  , operationReadFile = ByteString.readFile
  , operationRenameFile = Directory.renameFile
  , operationRemoveFile = Directory.removeFile
  , operationBeforeCommit = pure ()
  }

transactionalWrite :: [PlannedWrite] -> IO (Either TransactionError ())
transactionalWrite = transactionalWriteWith defaultFileOperations

transactionalWriteWith
  :: FileOperations
  -> [PlannedWrite]
  -> IO (Either TransactionError ())
transactionalWriteWith operations plans = Exception.mask $ \restore -> do
  stagedResult <- tryIO $ restore $ stageAll operations plans
  case stagedResult of
    Left ioError -> pure $ Left $ StagingFailure
      (fromMaybe "unknown target" $ stagingFailureTarget plans ioError)
      (IOError.ioeGetErrorString ioError)
    Right staged -> do
      beforeCommitResult <- tryIO
        $ restore (operationBeforeCommit operations)
          `Exception.onException` cleanupAll operations staged
      case beforeCommitResult of
        Left ioError -> do
          cleanupAll operations staged
          pure $ Left $ StagingFailure
            "before commit" (IOError.ioeGetErrorString ioError)
        Right () -> commitAll operations staged

stageAll :: FileOperations -> [PlannedWrite] -> IO [StagedWrite]
stageAll _ [] = pure []
stageAll operations (plan : remainingPlans) = do
  staged <- stageOne operations plan
  remaining <- stageAll operations remainingPlans
    `Exception.onException` cleanupAll operations [staged]
  pure $ staged : remaining

stageOne :: FileOperations -> PlannedWrite -> IO StagedWrite
stageOne operations plan = do
  replacementPath <- operationCreateTemp operations target "stage"
  let cleanupReplacement = cleanupPath operations replacementPath
  (do
      operationWriteFile operations replacementPath $ plannedReplacement plan
      operationSetPermissions operations replacementPath
        $ plannedPermissions plan
    ) `Exception.onException` cleanupReplacement
  backupPath <- operationCreateTemp operations target "backup"
    `Exception.onException` cleanupReplacement
  let cleanupBoth = do
        cleanupPath operations replacementPath
        cleanupPath operations backupPath
  (do
      operationWriteFile operations backupPath $ plannedOriginal plan
      operationSetPermissions operations backupPath $ plannedPermissions plan
    ) `Exception.onException` cleanupBoth
  pure StagedWrite
    { stagedPlan = plan
    , stagedReplacementPath = replacementPath
    , stagedBackupPath = backupPath
    }
 where
  target = plannedTarget plan

commitAll
  :: FileOperations
  -> [StagedWrite]
  -> IO (Either TransactionError ())
commitAll operations staged = go [] staged
 where
  go committed = \case
    [] -> do
      cleanupAll operations staged
      pure $ Right ()
    current : remaining -> do
      let plan = stagedPlan current
          target = plannedTarget plan
      currentContents <- tryAny $ operationReadFile operations target
      case currentContents of
        Left exception -> failCommit committed target exception
        Right contents
          | contents /= plannedOriginal plan -> do
              rollbackResult <- rollback operations committed
              cleanupAll operations staged
              pure $ case rollbackResult of
                Nothing -> Left $ ConcurrentModification target
                Just (rollbackTarget, rollbackError) -> Left $ RollbackFailure
                  rollbackTarget rollbackError
                  ("concurrent modification at " ++ target)
          | otherwise -> do
              renameResult <- tryAny $ operationRenameFile operations
                (stagedReplacementPath current) target
              case renameResult of
                Left exception -> failCommit committed target exception
                Right () -> go (current : committed) remaining

  failCommit committed target exception = do
    rollbackResult <- rollback operations committed
    cleanupAll operations staged
    case Exception.fromException exception of
      Just asyncException -> Exception.throwIO
        (asyncException :: Exception.AsyncException)
      Nothing -> pure $ case rollbackResult of
        Nothing -> Left $ CommitFailure target
          $ Exception.displayException exception
        Just (rollbackTarget, rollbackError) -> Left $ RollbackFailure
          rollbackTarget rollbackError $ Exception.displayException exception

rollback
  :: FileOperations
  -> [StagedWrite]
  -> IO (Maybe (FilePath, String))
rollback operations = go
 where
  go [] = pure Nothing
  go (committed : remaining) = do
    let target = plannedTarget $ stagedPlan committed
    renameResult <- tryIO $ operationRenameFile operations
      (stagedBackupPath committed) target
    case renameResult of
      Left ioError -> pure $ Just (target, IOError.ioeGetErrorString ioError)
      Right () -> go remaining

cleanupAll :: FileOperations -> [StagedWrite] -> IO ()
cleanupAll operations = mapM_ $ \staged -> do
  cleanupPath operations $ stagedReplacementPath staged
  cleanupPath operations $ stagedBackupPath staged

cleanupPath :: FileOperations -> FilePath -> IO ()
cleanupPath operations path = void $ tryIO $ operationRemoveFile operations path

createSiblingTemp :: FilePath -> String -> IO FilePath
createSiblingTemp target purpose = Exception.bracketOnError
  (IO.openBinaryTempFile directory template)
  (\(path, handle) -> IO.hClose handle >> cleanup path)
  (\(path, handle) -> IO.hClose handle >> pure path)
 where
  directory = FilePath.takeDirectory target
  template = FilePath.takeFileName target ++ ".brittany-" ++ purpose
  cleanup path = void $ tryIO $ Directory.removeFile path

tryIO :: IO value -> IO (Either IOError.IOError value)
tryIO = IOError.tryIOError

tryAny :: IO value -> IO (Either Exception.SomeException value)
tryAny = Exception.try

stagingFailureTarget
  :: [PlannedWrite]
  -> IOError.IOError
  -> Maybe FilePath
stagingFailureTarget plans ioError =
  IOError.ioeGetFileName ioError <|> plannedTarget <$> listToMaybe plans
