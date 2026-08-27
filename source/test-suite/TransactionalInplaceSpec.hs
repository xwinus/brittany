{-# LANGUAGE LambdaCase #-}

module TransactionalInplaceSpec (spec) where

import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.IORef as IORef
import qualified Data.List as List
import Data.Word (Word8)
import qualified Language.Haskell.Brittany.Internal.TransactionalWrite as Transaction
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "transactional multi-file inplace formatting" $ do
  Hspec.it "commits every valid change, preserves permissions, and is idempotent" $
    withTestDirectory "success" $ \directory -> do
      first <- writeModule directory "First.hs" "module First where\nvalue=1\n"
      second <- writeModule directory "Second.hs" "module Second where\nvalue=2\n"
      third <- writeModule directory "Third.hs" "module Third where\nvalue=3\n"
      firstPermissions <- Directory.getPermissions first
      Directory.setPermissions first firstPermissions { Directory.executable = True }
      runFormatter [first, second, third]
      firstPass <- traverse ByteString.readFile [first, second, third]
      firstPass `Hspec.shouldNotBe` map ByteString.pack
        [ascii "module First where\nvalue=1\n"
        ,ascii "module Second where\nvalue=2\n"
        ,ascii "module Third where\nvalue=3\n"]
      Directory.executable <$> Directory.getPermissions first
        `Hspec.shouldReturn` True
      runFormatter [first, second, third]
      traverse ByteString.readFile [first, second, third]
        `Hspec.shouldReturn` firstPass

  Hspec.it "leaves every target unchanged for failures anywhere in the batch" $
    withTestDirectory "validation" $ \directory -> do
      candidatesBefore <- candidateArtifacts
      mapM_ (assertValidationAtomic directory) [0, 1, 2]
      candidateArtifacts `Hspec.shouldReturn` candidatesBefore

  Hspec.it "leaves valid peers unchanged after a strict fallback failure" $
    withTestDirectory "fallback" $ \directory -> do
      valid <- writeModule directory "Valid.hs" "module Valid where\nvalue=1\n"
      let fallback = directory FilePath.</> "Fallback.hs"
      Directory.copyFile
        (projectRoot FilePath.</> "source/test-suite/fixtures/SpecialisePragmasEdge.hs")
        fallback
      originals <- traverse ByteString.readFile [valid, fallback]
      Brittany.mainWith "brittany"
        [ "--config-file"
        , projectRoot FilePath.</> "data/brittany.yaml"
        , "--no-user-config"
        , "--fail-on-fallback"
        , "--output-on-errors"
        , "--write-mode"
        , "inplace"
        , valid
        , fallback
        ] `Hspec.shouldThrow` (== Exit.ExitFailure 1)
      traverse ByteString.readFile [valid, fallback] `Hspec.shouldReturn` originals

  Hspec.it "handles duplicate and non-ASCII paths without duplicate writes" $
    withTestDirectory "paths" $ \directory -> do
      target <- writeModule directory "space č.hs"
        "module UnicodePath where\nvalue=1\n"
      runFormatter [target, target]
      firstPass <- ByteString.readFile target
      runFormatter [target, target]
      ByteString.readFile target `Hspec.shouldReturn` firstPass

  Hspec.it "turns an unexpected file exception into a clean batch failure" $
    withTestDirectory "unexpected" $ \directory -> do
      valid <- writeModule directory "Valid.hs" "module Valid where\nvalue=1\n"
      original <- ByteString.readFile valid
      candidatesBefore <- candidateArtifacts
      Brittany.mainWith "brittany"
        [ "--no-user-config"
        , "--write-mode"
        , "inplace"
        , valid
        , directory FilePath.</> "Missing.hs"
        ] `Hspec.shouldThrow` (== Exit.ExitFailure 74)
      ByteString.readFile valid `Hspec.shouldReturn` original
      candidateArtifacts `Hspec.shouldReturn` candidatesBefore

  Hspec.it "keeps check mode non-mutating" $
    withTestDirectory "check" $ \directory -> do
      target <- writeModule directory "Check.hs" "module Check where\nvalue=1\n"
      original <- ByteString.readFile target
      Brittany.mainWith "brittany"
        ["--no-user-config", "--check-mode", target]
        `Hspec.shouldThrow` (== Exit.ExitFailure 1)
      ByteString.readFile target `Hspec.shouldReturn` original

  Hspec.it "keeps multi-file display mode non-mutating" $
    withTestDirectory "display" $ \directory -> do
      first <- writeModule directory "DisplayOne.hs"
        "module DisplayOne where\nvalue=1\n"
      second <- writeModule directory "DisplayTwo.hs"
        "module DisplayTwo where\nvalue=2\n"
      originals <- traverse ByteString.readFile [first, second]
      Brittany.mainWith "brittany" ["--no-user-config", first, second]
      traverse ByteString.readFile [first, second] `Hspec.shouldReturn` originals

  Hspec.it "does not stage an unchanged read-only file" $
    withTestDirectory "readonly" $ \directory -> do
      unchanged <- writeModule directory "Unchanged.hs"
        "module Unchanged where\n\nvalue = 1\n"
      runFormatter [unchanged]
      original <- ByteString.readFile unchanged
      permissions <- Directory.getPermissions unchanged
      Directory.setPermissions unchanged permissions { Directory.writable = False }
      changed <- writeModule directory "Changed.hs"
        "module Changed where\nvalue=2\n"
      runFormatter [unchanged, changed]
      ByteString.readFile unchanged `Hspec.shouldReturn` original
      Directory.writable <$> Directory.getPermissions unchanged
        `Hspec.shouldReturn` False

  Hspec.it "detects an external edit before commit and preserves it" $
    withTestDirectory "concurrent" $ \directory -> do
      target <- writeBytes directory "Concurrent.hs" "old"
      plan <- plannedWrite target "old" "formatted"
      let external = bytes "external"
          operations = Transaction.defaultFileOperations
            { Transaction.operationBeforeCommit = ByteString.writeFile target external }
      Transaction.transactionalWriteWith operations [plan]
        `Hspec.shouldReturn` Left (Transaction.ConcurrentModification target)
      ByteString.readFile target `Hspec.shouldReturn` external
      assertNoArtifacts directory

  Hspec.it "rolls back earlier renames when a later rename fails" $
    withTestDirectory "rollback" $ \directory -> do
      first <- writeBytes directory "First.hs" "first-old"
      second <- writeBytes directory "Second.hs" "second-old"
      plans <- sequence
        [ plannedWrite first "first-old" "first-new"
        , plannedWrite second "second-old" "second-new"
        ]
      renameCount <- IORef.newIORef (0 :: Int)
      let defaults = Transaction.defaultFileOperations
          operations = defaults
            { Transaction.operationRenameFile = \source target -> do
                count <- IORef.atomicModifyIORef' renameCount $ \value ->
                  let next = value + 1 in (next, next)
                if count == 2
                  then ioError $ userError "injected rename failure"
                  else Transaction.operationRenameFile defaults source target
            }
      result <- Transaction.transactionalWriteWith operations plans
      result `Hspec.shouldSatisfy` \case
        Left Transaction.CommitFailure{} -> True
        _ -> False
      ByteString.readFile first `Hspec.shouldReturn` bytes "first-old"
      ByteString.readFile second `Hspec.shouldReturn` bytes "second-old"
      assertNoArtifacts directory

  mapM_ stagingFailureExample
    [ ("temporary-file creation", failCreate)
    , ("temporary-file write", failWrite)
    , ("temporary-file permissions", failPermissions)
    ]

  Hspec.it "cleans staging files when interrupted before commit" $
    withTestDirectory "interrupt" $ \directory -> do
      target <- writeBytes directory "Interrupt.hs" "old"
      plan <- plannedWrite target "old" "new"
      let operations = Transaction.defaultFileOperations
            { Transaction.operationBeforeCommit =
                Exception.throwIO Exception.UserInterrupt
            }
      Transaction.transactionalWriteWith operations [plan]
        `Hspec.shouldThrow` (== Exception.UserInterrupt)
      ByteString.readFile target `Hspec.shouldReturn` bytes "old"
      assertNoArtifacts directory

  Hspec.it "rolls back committed files when interrupted during commit" $
    withTestDirectory "commit-interrupt" $ \directory -> do
      first <- writeBytes directory "First.hs" "first-old"
      second <- writeBytes directory "Second.hs" "second-old"
      plans <- sequence
        [ plannedWrite first "first-old" "first-new"
        , plannedWrite second "second-old" "second-new"
        ]
      renameCount <- IORef.newIORef (0 :: Int)
      let defaults = Transaction.defaultFileOperations
          operations = defaults
            { Transaction.operationRenameFile = \source target -> do
                count <- IORef.atomicModifyIORef' renameCount $ \value ->
                  let next = value + 1 in (next, next)
                if count == 2
                  then Exception.throwIO Exception.UserInterrupt
                  else Transaction.operationRenameFile defaults source target
            }
      Transaction.transactionalWriteWith operations plans
        `Hspec.shouldThrow` (== Exception.UserInterrupt)
      ByteString.readFile first `Hspec.shouldReturn` bytes "first-old"
      ByteString.readFile second `Hspec.shouldReturn` bytes "second-old"
      assertNoArtifacts directory

stagingFailureExample
  :: (String, Transaction.FileOperations -> Transaction.FileOperations)
  -> Hspec.SpecWith ()
stagingFailureExample (description, injectFailure) =
  Hspec.it ("cleans up after " ++ description ++ " failure") $
    withTestDirectory "staging" $ \directory -> do
      target <- writeBytes directory "Staging.hs" "old"
      plan <- plannedWrite target "old" "new"
      result <- Transaction.transactionalWriteWith
        (injectFailure Transaction.defaultFileOperations) [plan]
      result `Hspec.shouldSatisfy` \case
        Left Transaction.StagingFailure{} -> True
        _ -> False
      ByteString.readFile target `Hspec.shouldReturn` bytes "old"
      assertNoArtifacts directory

failCreate :: Transaction.FileOperations -> Transaction.FileOperations
failCreate operations = operations
  { Transaction.operationCreateTemp = \_ _ ->
      ioError $ userError "injected create failure"
  }

failWrite :: Transaction.FileOperations -> Transaction.FileOperations
failWrite operations = operations
  { Transaction.operationWriteFile = \_ _ ->
      ioError $ userError "injected write failure"
  }

failPermissions :: Transaction.FileOperations -> Transaction.FileOperations
failPermissions operations = operations
  { Transaction.operationSetPermissions = \_ _ ->
      ioError $ userError "injected permission failure"
  }

assertValidationAtomic :: FilePath -> Int -> IO ()
assertValidationAtomic directory failureIndex = do
  let caseDirectory = directory FilePath.</> show failureIndex
  Directory.createDirectory caseDirectory
  validA <- writeModule caseDirectory "ValidA.hs"
    "module ValidA where\nvalue=1\n"
  invalid <- writeModule caseDirectory "Invalid.hs"
    "module Invalid where\nvalue =\n"
  validB <- writeModule caseDirectory "ValidB.hs"
    "module ValidB where\nvalue=2\n"
  let originals = [validA, invalid, validB]
  originalContents <- traverse ByteString.readFile originals
  let before = take failureIndex [validA, validB]
      after = drop failureIndex [validA, validB]
      ordered = before ++ [invalid] ++ after
  Brittany.mainWith "brittany"
    (["--no-user-config", "--write-mode", "inplace"] ++ ordered)
    `Hspec.shouldThrow` (== Exit.ExitFailure 1)
  traverse ByteString.readFile originals `Hspec.shouldReturn` originalContents
  assertNoArtifacts caseDirectory

runFormatter :: [FilePath] -> IO ()
runFormatter paths = Brittany.mainWith "brittany"
  $ ["--no-user-config", "--write-mode", "inplace"] ++ paths

plannedWrite :: FilePath -> String -> String -> IO Transaction.PlannedWrite
plannedWrite target original replacement = do
  permissions <- Directory.getPermissions target
  pure Transaction.PlannedWrite
    { Transaction.plannedTarget = target
    , Transaction.plannedOriginal = bytes original
    , Transaction.plannedReplacement = bytes replacement
    , Transaction.plannedPermissions = permissions
    }

writeModule :: FilePath -> FilePath -> String -> IO FilePath
writeModule = writeBytes

writeBytes :: FilePath -> FilePath -> String -> IO FilePath
writeBytes directory name contents = do
  let path = directory FilePath.</> name
  ByteString.writeFile path $ bytes contents
  pure path

bytes :: String -> ByteString.ByteString
bytes = ByteString.pack . ascii

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum

withTestDirectory :: String -> (FilePath -> IO value) -> IO value
withTestDirectory label = Exception.bracket create Directory.removeDirectoryRecursive
 where
  create = do
    temporaryDirectory <- Directory.getTemporaryDirectory
    (path, handle) <- IO.openTempFile temporaryDirectory
      $ "brittany-transaction-" ++ label
    IO.hClose handle
    Directory.removeFile path
    Directory.createDirectory path
    pure path

assertNoArtifacts :: FilePath -> Hspec.Expectation
assertNoArtifacts directory = do
  entries <- Directory.listDirectory directory
  filter (List.isInfixOf ".brittany-") entries `Hspec.shouldBe` []

candidateArtifacts :: IO [FilePath]
candidateArtifacts = do
  temporaryDirectory <- Directory.getTemporaryDirectory
  entries <- Directory.listDirectory temporaryDirectory
  pure $ List.sort $ filter (List.isPrefixOf "brittany-plan") entries
