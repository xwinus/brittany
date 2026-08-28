module LetStatementBoundaryInvalid where

broken = do
  let cleanup = pure ()
  (do
    first
  ) `onException`
