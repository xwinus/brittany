{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TraditionalRecordSyntax #-}
module RecordSyntaxInvalid where

data User = User { name :: String }

broken = User { name = }
