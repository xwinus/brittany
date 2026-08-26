{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DocumentedSingleConstructor where

type CodeLine = String

-- | Lines in a source file.
newtype SourceCode
    = SourceCode [CodeLine]
    deriving stock (Eq, Show)
    -- Keep this deriving comment structural.
    deriving newtype (Semigroup, Monoid)

-- | Classification remains on the same indentation policy.
data LineType
    = -- | Line of code.
      Code
    | -- | Line of comment.
      Comment
    deriving stock (Eq, Show)

newtype Undocumented
    = Undocumented Int
    deriving stock (Eq, Show)
