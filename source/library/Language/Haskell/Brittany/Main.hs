{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Main
  ( ChangeStatus(..)
  , WriteMode(..)
  , coreIO
  , helpDoc
  , licenseDoc
  , main
  , mainCmdParser
  , mainWith
  , shouldEmitOutput
  ) where

import qualified Control.Exception as Exception
import Control.Monad (zipWithM)
import qualified Data.ByteString as ByteString
import qualified Data.Either
import Data.Kind (Type)
import qualified Data.List.Extra
import qualified Data.Monoid
import qualified Data.Semigroup as Semigroup
import qualified GHC.OldList as List
import Language.Haskell.Brittany.Internal.Config
import Language.Haskell.Brittany.Internal.Config.Types
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.TransactionalWrite
  ( PlannedWrite(..)
  , TransactionError
  , transactionalWrite
  )
import Language.Haskell.Brittany.Internal.Utils
import Language.Haskell.Brittany.Main.Transform
  ( ChangeStatus(..)
  , coreIO
  , coreIOInSession
  , shouldEmitOutput
  )
import Paths_brittany
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit
import qualified System.FilePath.Posix as FilePath
import qualified System.IO
import qualified System.IO.Error as IOError
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.PrettyPrint as PP
import Text.Read (Read(..))
import UI.Butcher.Monadic

type WriteMode :: Type
data WriteMode = Display | Inplace

instance Read WriteMode where
  readPrec = value "display" Display <|> value "inplace" Inplace
   where
    value identifier writeMode =
      ReadPrec.lift $ ReadP.string identifier >> pure writeMode

instance Show WriteMode where
  show Display = "display"
  show Inplace = "inplace"

main :: IO ()
main = do
  programName <- Environment.getProgName
  arguments <- Environment.getArgs
  mainWith programName arguments

mainWith :: String -> [String] -> IO ()
mainWith programName arguments =
  Environment.withProgName programName
    . Environment.withArgs arguments
    $ case runCmdParserWithHelpDesc
        (Just programName) (InputArgs arguments) mainCmdParser of
      (description, Left parsingError) -> do
        putStrErrLn $ programName ++ ": " ++ parsingErrorString parsingError
        putStrErrLn "usage:"
        System.IO.hPutStrLn System.IO.stderr $ show $ ppUsage description
        System.Exit.exitWith $ System.Exit.ExitFailure 64
      (description, Right command) -> case _cmd_out command of
        Nothing -> do
          putStrErrLn "usage:"
          System.IO.hPutStrLn System.IO.stderr $ show $ ppUsage description
          System.Exit.exitWith $ System.Exit.ExitFailure 64
        Just action -> action

helpDoc :: PP.Doc
helpDoc = PP.vcat $ List.intersperse (PP.text "")
  [ parDocW
    [ "Reformats one or more haskell modules."
    , "Currently affects only the module head (imports/exports), type"
    , "signatures and function bindings;"
    , "everything else is left unmodified."
    , "Based on ghc-exactprint, thus (theoretically) supporting all"
    , "that ghc does."
    ]
  , parDoc "Example invocations:"
  , PP.hang (PP.text "") 2 $ PP.vcat
    [ PP.text "brittany"
    , PP.nest 2 $ PP.text "read from stdin, output to stdout"
    ]
  , PP.hang (PP.text "") 2 $ PP.vcat
    [ PP.text "brittany --indent=4 --write-mode=inplace *.hs"
    , PP.nest 2 $ PP.vcat
      [ PP.text "transactionally update all modules in the current directory"
      , PP.text "4 spaces indentation"
      ]
    ]
  , parDocW
    [ "This program is written carefully and contains safeguards to ensure"
    , "the output is syntactically valid, no comments are removed, and the"
    , "normalized syntax-affecting parsed AST is preserved."
    , "Nonetheless, compiler plugins and type-directed behavior remain beyond"
    , "what a parsed-syntax comparison can prove."
    , "Please do check the output and keep version-controlled backups."
    ]
  , parDoc "There is NO WARRANTY, to the extent permitted by law."
  , parDocW
    [ "This program is free software released under the AGPLv3."
    , "For details use the --license flag."
    ]
  , parDoc "See https://github.com/lspitzner/brittany"
  , parDoc
    "Please report bugs at https://github.com/lspitzner/brittany/issues"
  ]

licenseDoc :: PP.Doc
licenseDoc = PP.vcat $ List.intersperse (PP.text "")
  [ parDoc "Copyright (C) 2016-2019 Lennart Spitzner"
  , parDoc "Copyright (C) 2019 PRODA LTD"
  , parDocW
    [ "This program is free software: you can redistribute it and/or modify"
    , "it under the terms of the GNU Affero General Public License,"
    , "version 3, as published by the Free Software Foundation."
    ]
  , parDocW
    [ "This program is distributed in the hope that it will be useful,"
    , "but WITHOUT ANY WARRANTY; without even the implied warranty of"
    , "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
    , "GNU Affero General Public License for more details."
    ]
  , parDocW
    [ "You should have received a copy of the GNU Affero General Public"
    , "License along with this program.  If not, see"
    , "<http://www.gnu.org/licenses/>."
    ]
  ]

mainCmdParser :: CommandDesc () -> CmdParser Identity (IO ()) ()
mainCmdParser helpDescription = do
  addCmdSynopsis "haskell source pretty printer"
  addCmdHelp helpDoc
  addHelpCommand helpDescription
  addCmd "license" $ addCmdImpl $ print licenseDoc
  reorderStart
  printHelp <- addSimpleBoolFlag "h" ["help"] mempty
  printVersion <- addSimpleBoolFlag "" ["version"] mempty
  printLicense <- addSimpleBoolFlag "" ["license"] mempty
  noUserConfig <- addSimpleBoolFlag "" ["no-user-config"] mempty
  configPaths <- addFlagStringParams
    "" ["config-file"] "PATH" $ flagHelpStr "path to config file"
  commandLineConfig <- cmdlineConfigParser
  suppressOutput <- addSimpleBoolFlag
    "" ["suppress-output"] $ flagHelp $ parDoc
      "suppress the regular output, i.e. the transformed haskell source"
  _verbosity <- addSimpleCountFlag
    "v" ["verbose"] $ flagHelp $ parDoc "[currently without effect; TODO]"
  checkMode <- addSimpleBoolFlag
    "c" ["check-mode"] $ flagHelp $ PP.vcat
      [ PP.text "check for changes but do not write them out"
      , PP.text "exits with code 0 if no changes necessary, 1 otherwise"
      , PP.text "and print file path(s) of files that have changes to stdout"
      ]
  writeMode <- addFlagReadParam
    "" ["write-mode"] "(display|inplace)"
    ( flagHelp (PP.vcat
        [ PP.text "display: output for any input(s) goes to stdout"
        , PP.text "inplace: transactionally update respective input files"
        ])
      Data.Monoid.<> flagDefault Display
    )
  inputParameters <- addParamNoFlagStrings
    "PATH" $ paramHelpStr "paths to input/inout haskell source files"
  reorderStop
  addCmdImpl $ void $ do
    when printLicense $ print licenseDoc >> System.Exit.exitSuccess
    when printVersion $ do
      putStrLn $ "brittany version " ++ showVersion version
      putStrLn "Copyright (C) 2016-2019 Lennart Spitzner"
      putStrLn "Copyright (C) 2019 PRODA LTD"
      putStrLn "There is NO WARRANTY, to the extent permitted by law."
      System.Exit.exitSuccess
    when printHelp $ do
      liftIO $ putStrLn
        $ PP.renderStyle PP.style { PP.ribbonsPerLine = 1.0 }
        $ ppHelpShallow helpDescription
      System.Exit.exitSuccess

    let
      inputPaths = if null inputParameters
        then [Nothing]
        else Just <$> inputParameters
      outputPaths = case writeMode of
        Display -> repeat Nothing
        Inplace -> inputPaths
    configsToLoad <- liftIO $ if null configPaths
      then maybeToList
        <$> (Directory.getCurrentDirectory >>= findLocalConfigPath)
      else pure configPaths
    config <- runMaybeT
      (if noUserConfig
        then readConfigs commandLineConfig configsToLoad
        else readConfigsWithUserConfig commandLineConfig configsToLoad
      ) >>= \case
        Nothing -> System.Exit.exitWith $ System.Exit.ExitFailure 53
        Just loadedConfig -> pure loadedConfig
    when (config & _conf_debug & _dconf_dump_config & confUnpack)
      $ trace (showConfigYaml config) $ pure ()

    results <- ParseModule.withParserSession $ \parserSession ->
      case (writeMode, checkMode, sequence inputPaths) of
        (Inplace, False, Just paths) ->
          runTransactionalInplace parserSession putStrErrLn config
            suppressOutput paths
        _ -> zipWithM
          (coreIOInSession parserSession putStrErrLn config suppressOutput
            checkMode
          )
          inputPaths outputPaths
    finishWithResults checkMode results

finishWithResults :: Bool -> [Either Int ChangeStatus] -> IO ()
finishWithResults checkMode results =
  if checkMode
    then case Data.Either.lefts results of
      [] -> when (Changes `elem` Data.Either.rights results)
        $ System.Exit.exitWith $ System.Exit.ExitFailure 1
      [exitCode] -> System.Exit.exitWith $ System.Exit.ExitFailure exitCode
      _ -> System.Exit.exitWith $ System.Exit.ExitFailure 1
    else case results of
      successful | all Data.Either.isRight successful -> pure ()
      [Left exitCode] -> System.Exit.exitWith $ System.Exit.ExitFailure exitCode
      _ -> System.Exit.exitWith $ System.Exit.ExitFailure 1

type InplacePlan :: Type
data InplacePlan = InplacePlan
  { inplacePlanPath :: FilePath.FilePath
  , inplacePlanOriginal :: ByteString.ByteString
  , inplacePlanPermissions :: Directory.Permissions
  , inplacePlanCandidate :: FilePath.FilePath
  , inplacePlanResult :: Either Int ChangeStatus
  }

runTransactionalInplace
  :: ParseModule.ParserSession
  -> (String -> IO ())
  -> Config
  -> Bool
  -> [FilePath.FilePath]
  -> IO [Either Int ChangeStatus]
runTransactionalInplace parserSession putErrorLine config suppressOutput paths =
  handlePlanningFailure $ do
    plans <- planAll $ Data.List.Extra.nubOrd paths
    Exception.finally (complete plans) $ cleanupPlans plans
 where
  complete plans = do
    let results = inplacePlanResult <$> plans
    if any Data.Either.isLeft results || suppressOutput
      then pure results
      else do
        writesResult <- traverse toPlannedWrite
          [plan | plan <- plans, inplacePlanResult plan == Right Changes]
        case sequence writesResult of
          Left ioError -> transactionFailed $ show ioError
          Right writes -> transactionalWrite writes >>= \case
            Left transactionError ->
              transactionFailed $ showTransactionError transactionError
            Right () -> pure results

  planAll [] = pure []
  planAll (path : remaining) = do
    plan <- planOne path
    rest <- planAll remaining
      `Exception.onException` cleanupPlans [plan]
    pure $ plan : rest

  planOne path = do
    original <- ByteString.readFile path
    permissions <- Directory.getPermissions path
    candidate <- createCandidatePath
    let
      cleanupCandidate = cleanupPath candidate
      putFileError message = putErrorLine $ path ++ ": " ++ message
    result <- coreIOInSession parserSession putFileError config suppressOutput False
      (Just path) (Just candidate)
      `Exception.onException` cleanupCandidate
    pure InplacePlan
      { inplacePlanPath = path
      , inplacePlanOriginal = original
      , inplacePlanPermissions = permissions
      , inplacePlanCandidate = candidate
      , inplacePlanResult = result
      }

  toPlannedWrite plan = do
    replacementResult <- IOError.tryIOError
      $ ByteString.readFile $ inplacePlanCandidate plan
    pure $ replacementResult <&> \replacement -> PlannedWrite
      { plannedTarget = inplacePlanPath plan
      , plannedOriginal = inplacePlanOriginal plan
      , plannedReplacement = replacement
      , plannedPermissions = inplacePlanPermissions plan
      }

  transactionFailed message = do
    putErrorLine $ "transactional inplace update failed: " ++ message
    pure [Left 74]

  handlePlanningFailure action = Exception.catch action handleException

  handleException :: Exception.SomeException -> IO [Either Int ChangeStatus]
  handleException exception =
    case Exception.fromException exception of
      Just asyncException -> Exception.throwIO
        (asyncException :: Exception.AsyncException)
      Nothing -> do
        putErrorLine $ "transactional inplace planning failed: "
          ++ Exception.displayException exception
        pure [Left 74]

createCandidatePath :: IO FilePath.FilePath
createCandidatePath = Exception.bracketOnError
  (Directory.getTemporaryDirectory >>= \directory ->
    System.IO.openBinaryTempFile directory "brittany-plan")
  (\(path, handle) -> System.IO.hClose handle >> cleanupPath path)
  (\(path, handle) -> System.IO.hClose handle >> pure path)

cleanupPlans :: [InplacePlan] -> IO ()
cleanupPlans = mapM_ $ cleanupPath . inplacePlanCandidate

cleanupPath :: FilePath.FilePath -> IO ()
cleanupPath path = void $ IOError.tryIOError $ Directory.removeFile path

showTransactionError :: TransactionError -> String
showTransactionError = show
