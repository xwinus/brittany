module FallbackSweepSpec (spec) where

import qualified Control.Exception as Exception
import Data.Functor.Identity (Identity(..))
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import Language.Haskell.Brittany
import qualified Language.Haskell.Brittany.Internal as Internal
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.Types (PerItemConfig(..))
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.IO as IO
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "maintained-source fallback sweep" $ do
  Hspec.it "formats nested ordinary declarations without fallback" $ do
    firstPass <- formatChecked staticDefaultConfig "OrdinaryDeclarations.hs"
      ordinarySource
    firstPass `Hspec.shouldContain` "Row { rowValue }"
    formatChecked staticDefaultConfig "OrdinaryDeclarations.hs" firstPass
      `Hspec.shouldReturn` firstPass

  Hspec.it "keeps contextual H98 constructors stable at narrow widths" $ do
    firstPass <- formatChecked narrowConfig "ContextualConstructors.hs"
      contextualConstructorSource
    firstPass `Hspec.shouldContain` "forall value ."
    firstPass `Hspec.shouldContain` "-- retained constructor note"
    formatChecked narrowConfig "ContextualConstructors.hs" firstPass
      `Hspec.shouldReturn` firstPass

  Hspec.it "reports an unsupported expression without changing inplace input" $
    withTemporaryModule unsupportedExpressionSource $ \path -> do
      original <- readFile path
      messagesRef <- IORef.newIORef []
      result <- Brittany.coreIO
        (appendMessage messagesRef)
        strictFallbackConfig
        True
        False
        (Just path)
        (Just path)
      messages <- IORef.readIORef messagesRef
      case result of
        Left 70 -> pure ()
        _ -> Hspec.expectationFailure "expected strict fallback exit code 70"
      messages `Hspec.shouldSatisfy` any (List.isInfixOf "ExpressionFallback")
      messages `Hspec.shouldNotSatisfy` any
        (List.isInfixOf "DeclarationFallback")
      messages `Hspec.shouldNotSatisfy` any
        (List.isInfixOf "WholeModuleFallback")
      readFile path `Hspec.shouldReturn` original

formatChecked :: Config -> FilePath -> String -> IO String
formatChecked config filename source = do
  parsed <- ParseModule.parseModule [] filename (const $ pure $ Right ()) source
  case parsed of
    Left parseError -> Hspec.expectationFailure parseError >> fail parseError
    Right (annotations, parsedSource, ()) -> do
      (errors, output) <- Internal.pPrintModuleAndCheckWithSource
        (Just $ Text.pack source)
        config
        emptyPerItemConfig
        annotations
        parsedSource
      case errors of
        [] -> pure $ Text.unpack $ TextLazy.toStrict output
        _ -> Hspec.expectationFailure
          ("formatting returned " ++ show (length errors) ++ " errors")
          >> fail "formatting failed"

emptyPerItemConfig :: PerItemConfig
emptyPerItemConfig =
  PerItemConfig { _icd_perBinding = Map.empty, _icd_perKey = Map.empty }

narrowConfig :: Config
narrowConfig = staticDefaultConfig
  { _conf_layout = (_conf_layout staticDefaultConfig)
    { _lconfig_cols = Identity $ Semigroup.Last 42
    }
  }

strictFallbackConfig :: Config
strictFallbackConfig = staticDefaultConfig
  { _conf_errorHandling = (_conf_errorHandling staticDefaultConfig)
    { _econf_failOnExactSourceFallback = Identity $ Semigroup.Last True
    }
  }

appendMessage :: IORef.IORef [String] -> String -> IO ()
appendMessage messagesRef message =
  IORef.modifyIORef' messagesRef (++ [message])

withTemporaryModule :: String -> (FilePath -> IO value) -> IO value
withTemporaryModule contents = Exception.bracket create Directory.removeFile
 where
  create = do
    temporaryDirectory <- Directory.getTemporaryDirectory
    (path, handle) <- IO.openTempFile temporaryDirectory "brittany-fallback-sweep.hs"
    IO.hPutStr handle contents
    IO.hClose handle
    pure path

ordinarySource :: String
ordinarySource = unlines
  [ "{-# LANGUAGE NamedFieldPuns #-}"
  , "module OrdinaryDeclarations where"
  , ""
  , "data Row = Row { rowValue :: Int }"
  , ""
  , "render rows = do"
  , "  let renderRow row@Row {rowValue}"
  , "        | rowValue > 0 = case row of"
  , "            Row value -> (value, show value)"
  , "        | otherwise = (rowValue, \"zero\")"
  , "  pure (map renderRow rows)"
  ]

contextualConstructorSource :: String
contextualConstructorSource = unlines
  [ "{-# LANGUAGE ExistentialQuantification #-}"
  , "module ContextualConstructors where"
  , ""
  , "data Result"
  , "  = Plain Int"
  , "  -- retained constructor note"
  , "  | forall value. Show value => Wrapped value"
  ]

unsupportedExpressionSource :: String
unsupportedExpressionSource = unlines
  [ "module UnsupportedExpression where"
  , ""
  , "profiled value ="
  , "  {-# SCC \"profiled\" #-}"
  , "  value"
  ]
