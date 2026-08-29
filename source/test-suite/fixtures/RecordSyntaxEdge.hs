{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TraditionalRecordSyntax #-}
module RecordSyntaxEdge where
data Profile = Profile
  { name   :: String
  , active :: Bool
  }
data Group = Group
  { name :: String
  }
construct name active =
  Profile
    -- Keep the construction field comment.
    { name
    , active
    }
update profile active =
  profile
    -- Keep the update field comment.
    { active
    }
readName Profile { name, ..} =
  -- Keep the pattern field comment.
  name
isActive profile = active profile
