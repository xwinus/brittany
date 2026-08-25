{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}
instanceHeadAnchor = ()
instance Display Int where
  display = render
instance {-# OVERLAPPABLE #-} forall a. (Display (Nested [Maybe a]), Eq (Nested [Maybe a])) => Display (Wrapper (Nested [Maybe a])) where
  type Result a = Maybe a
  data ResultData a = ResultData a
  display :: Wrapper a -> String
  display = render
  displayFallback = render
-- exact-source instance comment
instance
  ( Display a -- context comment
  , Eq a
  ) => Display (Commented a) where
  -- associated member comment
  type Result a = a
  display = render
