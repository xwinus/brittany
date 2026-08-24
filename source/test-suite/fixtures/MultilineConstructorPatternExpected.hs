module MultilineConstructorPatternExpected where

run command = case command of
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
      result

short command = case command of
  Stop reason -> reason
