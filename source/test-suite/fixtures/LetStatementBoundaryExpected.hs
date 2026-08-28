module LetStatementBoundaryExpected where

example = do
  let cleanup = pure ()
  (
    do
      first
      second
    ) `onException` cleanup

writeTwo operations target = do
  let cleanupReplacement = cleanupPath operations target
  (
    do
      writeReplacement operations target
      setPermissions operations target
    ) `onException` cleanupReplacement
