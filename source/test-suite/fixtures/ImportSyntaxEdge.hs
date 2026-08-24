{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PackageImports #-}
module ImportSyntaxEdge where
-- Keep the import modifier comment.
import {-# SOURCE #-} safe "base" Data.List qualified as List

value = ()
