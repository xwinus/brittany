{-# OPTIONS_GHC -Wno-implicit-prelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.ParseModule
  ( ParserContext
  , ParserSession
  , parseModule
  , parseModuleInContextWithMetrics
  , parseModuleInSessionWithMetrics
  , parseModuleWithMetrics
  , parseModuleWithMetricsAndContext
  , withParserSession
  , withParserSessionWithMetrics
  ) where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Data.Map as Map
import Data.Kind (Type)
import qualified GHC
import qualified GHC.Driver.Env as DriverEnv
import qualified GHC.Driver.Monad as DriverMonad
import qualified GHC.Driver.Session
import qualified GHC.Paths as GHCPaths
import qualified GHC.Types.SrcLoc
import Language.Haskell.Brittany.Internal.ExactPrintCompat (Anns)
import Language.Haskell.Brittany.Internal.ExtractAnns
  ( extractAnnsFromModule
  , recoverMissingCommentsWithAnnotations
  )
import Language.Haskell.Brittany.Internal.Performance
  ( PerformanceCollector
  , PerformancePhase(..)
  , measurePhase
  , measurePhaseM
  )
import qualified Language.Haskell.GHC.ExactPrint.Parsers as ExactPrint

-- | Effective parser flags for a source file. Reusing this context for the
-- formatted form of the same file avoids constructing a second GHC session.
type ParserContext :: Type
newtype ParserContext = ParserContext GHC.Driver.Session.DynFlags

-- | One scoped, single-threaded GHC session. Each parse starts from the saved
-- base environment so source pragmas, forwarded options, and failures cannot
-- affect the next file in the batch.
type ParserSession :: Type
data ParserSession = ParserSession
  { parserSessionHandle :: DriverMonad.Session
  , parserSessionBaseEnvironment :: DriverEnv.HscEnv
  }

withParserSession :: (ParserSession -> IO a) -> IO a
withParserSession = withParserSessionWithMetrics Nothing

withParserSessionWithMetrics
  :: Maybe PerformanceCollector
  -> (ParserSession -> IO a)
  -> IO a
withParserSessionWithMetrics metrics action = do
  parserSession <- measurePhase metrics GhcSession
    $ GHC.runGhc (Just GHCPaths.libdir)
    $ do
      session <- DriverMonad.Ghc pure
      baseEnvironment <- GHC.getSession
      pure ParserSession
        { parserSessionHandle = session
        , parserSessionBaseEnvironment = baseEnvironment
        }
  action parserSession

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
  fmap dropParserContext
    $ parseModuleWithMetricsAndContext metrics arguments1 filePath checkDynFlags
      string
 where
  dropParserContext = fmap $ \(anns, parsedSource, result, _) ->
    (anns, parsedSource, result)

parseModuleWithMetricsAndContext
  :: IO.MonadIO io
  => Maybe PerformanceCollector
  -> [String]
  -> FilePath
  -> (GHC.Driver.Session.DynFlags -> IO (Either String a))
  -> String
  -> io (Either String (Anns, GHC.ParsedSource, a, ParserContext))
parseModuleWithMetricsAndContext metrics arguments1 filePath checkDynFlags
    string = IO.liftIO $ withParserSessionWithMetrics metrics $ \session ->
  parseModuleInSessionWithMetrics metrics session arguments1 filePath
    checkDynFlags string

parseModuleInSessionWithMetrics
  :: Maybe PerformanceCollector
  -> ParserSession
  -> [String]
  -> FilePath
  -> (GHC.Driver.Session.DynFlags -> IO (Either String a))
  -> String
  -> IO (Either String (Anns, GHC.ParsedSource, a, ParserContext))
parseModuleInSessionWithMetrics metrics session arguments1 filePath
    checkDynFlags string = do
  result <- Exception.try $ DriverMonad.reflectGhc parseInSession
    $ parserSessionHandle session
  case result of
    Left (exception :: Exception.SomeException) -> pure $ Left $ show exception
    Right parsed -> pure parsed
 where
  parseInSession = do
    GHC.setSession $ parserSessionBaseEnvironment session
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
      Left message -> IO.liftIO
        $ Exception.throwIO
        $ Exception.ErrorCall message
      Right dynFlagsResult -> do
        parsed <- IO.liftIO $ parseWithDynFlags metrics dflags1 filePath string
        pure $ fmap (\(anns, parsedSource) ->
          (anns, parsedSource, dynFlagsResult, ParserContext dflags1)
          ) parsed

parseModuleInContextWithMetrics
  :: IO.MonadIO io
  => Maybe PerformanceCollector
  -> ParserContext
  -> FilePath
  -> String
  -> io (Either String (Anns, GHC.ParsedSource))
parseModuleInContextWithMetrics metrics (ParserContext dflags) filePath string =
  IO.liftIO $ do
    result <- Exception.try $ parseWithDynFlags metrics dflags filePath string
    case result of
      Left (exception :: Exception.SomeException) -> pure $ Left $ show exception
      Right parsed -> pure parsed

parseWithDynFlags
  :: Maybe PerformanceCollector
  -> GHC.Driver.Session.DynFlags
  -> FilePath
  -> String
  -> IO (Either String (Anns, GHC.ParsedSource))
parseWithDynFlags metrics dflags filePath string = do
  parseResult <- measurePhase metrics SourceParsing
    $ Exception.evaluate
    $ ExactPrint.parseModuleFromStringInternal dflags filePath string
  case parseResult of
    Left _parseError -> pure $ Left "parse error"
    Right parsedSource -> do
      initialAnns <- extractAnnotations parsedSource
      (parsedSource', annotationsChanged) <- measurePhase metrics CommentRecovery
        $ Exception.evaluate
        $ recoverMissingCommentsWithAnnotations initialAnns string filePath
          parsedSource
      anns <- if annotationsChanged
        then extractAnnotations parsedSource'
        else pure initialAnns
      pure $ Right (anns, parsedSource')
 where
  extractAnnotations parsedSource = measurePhase metrics AnnotationExtraction
    $ do
      let extracted = extractAnnsFromModule parsedSource
      _ <- Exception.evaluate $ Map.size extracted
      pure extracted
