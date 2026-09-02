{-# OPTIONS_GHC -Wno-implicit-prelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Brittany.Internal.ParseModule where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Except as Except
import qualified Data.Map as Map
import qualified GHC
import qualified GHC.Driver.Session
import qualified GHC.Paths as GHCPaths
import qualified GHC.Types.SrcLoc
import Language.Haskell.Brittany.Internal.ExactPrintCompat (Anns)
import Language.Haskell.Brittany.Internal.ExtractAnns (extractAnnsFromModule, recoverMissingComments)
import Language.Haskell.Brittany.Internal.Performance
  ( PerformanceCollector
  , PerformancePhase(..)
  , measurePhase
  , measurePhaseM
  )
import qualified Language.Haskell.GHC.ExactPrint.Parsers as ExactPrint

-- | Parses a Haskell module. Although this nominally requires IO, it is
-- morally pure. It should have no observable effects.
--
-- For GHC 9.14+, uses GHC's runGhc and ghc-exactprint's initDynFlagsPure
-- instead of maintaining a manual DynFlags/Settings structure.
parseModule
  :: IO.MonadIO io
  => [String]
  -> FilePath
  -> (GHC.Driver.Session.DynFlags -> IO (Either String a))
  -> String
  -> io (Either String (Anns, GHC.ParsedSource, a))
parseModule arguments1 filePath checkDynFlags string =
  parseModuleWithMetrics Nothing arguments1 filePath checkDynFlags string

parseModuleWithMetrics
  :: IO.MonadIO io
  => Maybe PerformanceCollector
  -> [String]
  -> FilePath
  -> (GHC.Driver.Session.DynFlags -> IO (Either String a))
  -> String
  -> io (Either String (Anns, GHC.ParsedSource, a))
parseModuleWithMetrics metrics arguments1 filePath checkDynFlags string =
  Except.runExceptT $ Except.ExceptT $ IO.liftIO $ do
    result <- measurePhase metrics GhcSession
      $ Exception.try
        $ GHC.runGhc (Just GHCPaths.libdir)
        $ do
          (dflags0, logger) <- measurePhaseM metrics GhcSessionSetup $ do
            dflags <- ExactPrint.initDynFlagsPure filePath string
            currentLogger <- GHC.getLogger
            pure (dflags, currentLogger)
          (dflags1, leftovers1, _) <- measurePhaseM metrics DynamicFlagParsing
            $ GHC.parseDynamicFlags logger dflags0
              $ fmap GHC.Types.SrcLoc.noLoc arguments1
          Monad.unless (null leftovers1)
            $ IO.liftIO
            $ Exception.throwIO
            $ Exception.ErrorCall
            $ "leftovers: " <> show (fmap GHC.Types.SrcLoc.unLoc leftovers1)
          checkResult <- IO.liftIO $ checkDynFlags dflags1
          case checkResult of
            Left e -> IO.liftIO $ Exception.throwIO $ Exception.ErrorCall e
            Right dynFlagsResult -> do
              parseResult <- IO.liftIO $ measurePhase metrics SourceParsing
                $ Exception.evaluate
                $ ExactPrint.parseModuleFromStringInternal dflags1 filePath string
              case parseResult of
                Left _parseErr ->
                  IO.liftIO
                    $ Exception.throwIO
                    $ Exception.ErrorCall
                    $ "parse error"
                Right parsedSource -> do
                  parsedSource' <- IO.liftIO $ measurePhase metrics CommentRecovery
                    $ Exception.evaluate
                    $ recoverMissingComments string filePath parsedSource
                  anns <- IO.liftIO $ measurePhase metrics AnnotationExtraction $ do
                    let extracted = extractAnnsFromModule parsedSource'
                    _ <- Exception.evaluate $ Map.size extracted
                    pure extracted
                  pure $ Right (anns, parsedSource', dynFlagsResult)
    case result of
      Left (e :: Exception.SomeException) -> pure $ Left (show e)
      Right r -> pure r
