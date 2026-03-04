{-# OPTIONS_GHC -Wno-implicit-prelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Brittany.Internal.ParseModule where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Except as Except
import qualified GHC
import qualified GHC.Driver.Session
import qualified GHC.Paths as GHCPaths
import qualified GHC.Types.SrcLoc
import Language.Haskell.Brittany.Internal.ExactPrintCompat (Anns)
import Language.Haskell.Brittany.Internal.ExtractAnns (extractAnnsFromModule)
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
  Except.runExceptT $ Except.ExceptT $ IO.liftIO $ do
    result <-
      Exception.try
        $ GHC.runGhc (Just GHCPaths.libdir)
        $ do
          dflags0 <- ExactPrint.initDynFlagsPure filePath string
          logger <- GHC.getLogger
          (dflags1, leftovers1, _) <-
            GHC.parseDynamicFlags logger dflags0
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
              let parseResult =
                    ExactPrint.parseModuleFromStringInternal dflags1 filePath string
              case parseResult of
                Left _parseErr ->
                  IO.liftIO
                    $ Exception.throwIO
                    $ Exception.ErrorCall
                    $ "parse error"
                Right parsedSource ->
                  pure $ Right (extractAnnsFromModule parsedSource, parsedSource, dynFlagsResult)
    case result of
      Left (e :: Exception.SomeException) -> pure $ Left (show e)
      Right r -> pure r
