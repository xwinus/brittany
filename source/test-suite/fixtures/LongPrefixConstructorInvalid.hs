module LongPrefixConstructorInvalid where

data Broken
  = Good
  | Bad FirstArgument
      (SecondArgument
