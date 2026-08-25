{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module ScopedExpressionFallback where

nativeAction = do
  AppConfig {..} <- viewL
  pure ()

fallbackAction = do
  AppConfig {..} <- viewL
  logInfo [i|message|]
  pure ()
