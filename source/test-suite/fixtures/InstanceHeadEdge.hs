{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}
instanceHeadAnchor = ()
instance Display Int where
instance {-# OVERLAPPABLE #-} forall a.
  (Display (Nested [Maybe a]), Eq (Nested [Maybe a]))
  => Display (Wrapper (Nested [Maybe a]))
  where
  type Result (Wrapper (Nested [Maybe a])) = Maybe a
  display = render
-- Preserve this context comment through the guarded exact-source path.
instance
  ( Display a -- context comment
  , Eq a
  ) => Display (Commented a) where
  display = render
