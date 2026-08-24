{-# LANGUAGE ExplicitNamespaces #-}
module ExplicitNamespacesEdge
  ( data Just
    -- Keep the export namespace comment.
  , type Maybe
  ) where
import Data.Maybe
  ( data Just
  , -- Keep the import namespace comment.
    type Maybe
  )
