{-# LANGUAGE TypeFamilies #-}
type family Family value where
  -- Keep this family equation comment.
  Family Int = Bool
