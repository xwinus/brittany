{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PackageImports #-}
module ImportSyntaxInvalid where
import {-# SOURCE #-} safe "base" qualified Data.List
