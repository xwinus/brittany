{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module MultilineConstructorPatternEdge where

handle
  (Run
    (Nested sourcePaths)
    (excludedPaths   , ignoredPaths)
    [builtInTemplates, templateRefs]
    variables
    runMode
    debug@DebugMode
    dryRun) =
    result

commented command = case command of
  Run
    sourcePaths
    -- Keep this comment with the following argument.
    excludedPaths
    excludeIgnoredPaths
    builtInTemplates
    templateRefs
    variables
    runMode
    debug
    dryRun ->
      result

guarded command = case command of
  Run
    sourcePaths
    excludedPaths
    excludeIgnoredPaths
    builtInTemplates
    templateRefs
    variables
    runMode
    debug
    dryRun
    | dryRun    -> preview
    | otherwise -> execute

dispatch = \case
  Run
    sourcePaths
    excludedPaths
    excludeIgnoredPaths
    builtInTemplates
    templateRefs
    variables
    runMode
    debug
    dryRun ->
      execute
  Stop reason -> report reason

adorned command = case command of
  Decorated
    !strictValueWithLongName
    ~lazyValueWithLongName
    alias@(Nested nestedValue    )
    (      typedValue :: Int     )
    (      extract -> viewedValue) ->
      result
