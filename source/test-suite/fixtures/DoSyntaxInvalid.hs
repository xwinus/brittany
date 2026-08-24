{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecursiveDo #-}
module DoSyntaxInvalid where

import qualified Prelude as P

broken = P.do
  value <-
