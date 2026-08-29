{-# LANGUAGE ExistentialQuantification #-}

-- test comment
data MyRecord
  = forall a b
  . ( Loooooooooooooooooooooooooooooooong a
    , Loooooooooooooooooooooooooooooooong b
    ) =>
    MyConstructor a b
