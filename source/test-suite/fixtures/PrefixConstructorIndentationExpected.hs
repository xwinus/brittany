module PrefixConstructorIndentation where

data ConfigurationParseError
  = ConfigurationParseError
      ConfigurationScope
      Y.ParseException
  deriving Show
