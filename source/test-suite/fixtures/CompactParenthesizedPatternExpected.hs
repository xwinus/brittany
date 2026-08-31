{-# LANGUAGE LambdaCase #-}
module CompactParenthesizedPattern where

configurationErrorContaining :: String -> ConfigurationParseError -> Bool
configurationErrorContaining
  expected
  (ConfigurationParseError scope parseError) =
    scope == expected && parseError == expected

configurationErrorMatching expected (ConfigurationParseError scope parseError)
  | scope == expected && parseError == expected = True
  | otherwise = False

caseCompact value = case value of
  (ConfigurationParseError scope parseError) ->
    combineConfigurationErrorDetails scope parseError

caseGuardCompact value = case value of
  (ConfigurationParseError scope parseError)
    | scope == expectedConfigurationScope -> parseError
    | otherwise -> defaultParseError

lambdaCaseCompact = \case
  (ConfigurationParseError scope parseError) ->
    combineConfigurationErrorDetails scope parseError

(ConfigurationParseError boundScope boundParseError) =
  configurationErrorSourceWithLongDescriptiveName
