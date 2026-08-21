infixr 5 :+:
data Expression
  = Literal Int
  | Expression :+: Expression
  | Record
      { fieldName :: String
      }
  deriving (Eq, Show)
