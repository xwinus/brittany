{-# LANGUAGE StandaloneKindSignatures #-}

module CommandLineSpec (spec) where

import qualified Control.Exception as Exception
import qualified Data.ByteString.Char8 as ByteString
import Data.Kind (Type)
import qualified GHC.IO.Handle as Handle
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.IO.Error as IOError
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "command-line parsing" $ do
  Hspec.it "rejects an unknown option through stderr with a usage exit code" $ do
    (result, stdoutText, stderrText) <- captureOutput
      $ Brittany.mainWith "brittany" ["--badflag", "Example.hs"]
    result `Hspec.shouldBe` Left (Exit.ExitFailure 64)
    stdoutText `Hspec.shouldBe` ""
    stderrText `Hspec.shouldContain` "brittany: error parsing arguments:"
    stderrText `Hspec.shouldContain` "--badflag"
    stderrText `Hspec.shouldContain` "usage:"

  Hspec.it "rejects an invalid option value without attempting formatting" $ do
    (result, stdoutText, stderrText) <- captureOutput
      $ Brittany.mainWith "brittany" ["--write-mode", "invalid"]
    result `Hspec.shouldBe` Left (Exit.ExitFailure 64)
    stdoutText `Hspec.shouldBe` ""
    stderrText `Hspec.shouldContain` "error parsing arguments:"
    stderrText `Hspec.shouldContain` "--write-mode"

  Hspec.it "rejects an option whose required argument is missing" $ do
    (result, stdoutText, stderrText) <- captureOutput
      $ Brittany.mainWith "brittany" ["--config-file"]
    result `Hspec.shouldBe` Left (Exit.ExitFailure 64)
    stdoutText `Hspec.shouldBe` ""
    stderrText `Hspec.shouldContain` "error parsing arguments:"
    stderrText `Hspec.shouldContain` "--config-file"

  Hspec.it "keeps the help command successful and writes it to stdout" $ do
    (result, stdoutText, stderrText) <- captureOutput
      $ Brittany.mainWith "brittany" ["--help"]
    result `Hspec.shouldBe` Left Exit.ExitSuccess
    stdoutText `Hspec.shouldContain` "USAGE"
    stderrText `Hspec.shouldBe` ""

captureOutput
  :: IO () -> IO (Either Exit.ExitCode (), String, String)
captureOutput action = do
  temporaryDirectory <- Directory.getTemporaryDirectory
  Exception.bracket
    (openCaptureFiles temporaryDirectory)
    closeCaptureFiles
    $ \captureFiles -> do
      originalStdout <- Handle.hDuplicate IO.stdout
      originalStderr <- Handle.hDuplicate IO.stderr
      result <- Exception.try
        (do
          Handle.hDuplicateTo (captureStdoutHandle captureFiles) IO.stdout
          Handle.hDuplicateTo (captureStderrHandle captureFiles) IO.stderr
          action
        ) `Exception.finally` do
          IO.hFlush IO.stdout
          IO.hFlush IO.stderr
          Handle.hDuplicateTo originalStdout IO.stdout
          Handle.hDuplicateTo originalStderr IO.stderr
          IO.hClose originalStdout
          IO.hClose originalStderr
      IO.hClose $ captureStdoutHandle captureFiles
      IO.hClose $ captureStderrHandle captureFiles
      stdoutText <- ByteString.unpack
        <$> ByteString.readFile (captureStdoutPath captureFiles)
      stderrText <- ByteString.unpack
        <$> ByteString.readFile (captureStderrPath captureFiles)
      pure (result, stdoutText, stderrText)

type CaptureFiles :: Type
data CaptureFiles = CaptureFiles
  { captureStdoutPath :: FilePath
  , captureStdoutHandle :: IO.Handle
  , captureStderrPath :: FilePath
  , captureStderrHandle :: IO.Handle
  }

openCaptureFiles :: FilePath -> IO CaptureFiles
openCaptureFiles temporaryDirectory = do
  (stdoutPath, stdoutHandle) <- IO.openTempFile temporaryDirectory
    "brittany-command-line-stdout"
  (stderrPath, stderrHandle) <- IO.openTempFile temporaryDirectory
    "brittany-command-line-stderr"
  pure CaptureFiles
    { captureStdoutPath = stdoutPath
    , captureStdoutHandle = stdoutHandle
    , captureStderrPath = stderrPath
    , captureStderrHandle = stderrHandle
    }

closeCaptureFiles :: CaptureFiles -> IO ()
closeCaptureFiles captureFiles = do
  closeIfOpen $ captureStdoutHandle captureFiles
  closeIfOpen $ captureStderrHandle captureFiles
  removeIfExists $ captureStdoutPath captureFiles
  removeIfExists $ captureStderrPath captureFiles
 where
  closeIfOpen handle = IOError.tryIOError (IO.hClose handle) >> pure ()
  removeIfExists path = IOError.tryIOError (Directory.removeFile path) >> pure ()
