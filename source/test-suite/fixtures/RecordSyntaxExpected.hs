{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TraditionalRecordSyntax #-}
module RecordSyntaxExpected where
data User = User
  { name   :: String
  , active :: Bool
  }
data Team = Team
  { name :: String
  }
construct name active = User { name, active }
activate user = user { active = True }
readName User { name, ..} = name
isActive user = active user
