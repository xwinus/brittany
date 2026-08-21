data Result a = Result a
  deriving (Eq, Ord) via (Either String a)
