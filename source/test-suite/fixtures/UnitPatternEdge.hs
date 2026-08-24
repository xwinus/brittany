nestedUnit value = case value of
  Just (Left ()) -> ()
collectionUnits value = case value of
  ([()], ((), [])) -> ()
symbolicCons value = case value of
  (:) item rest -> ()
-- preserve the explicit pattern parentheses
explicitParens value = case value of
  Right (()) -> ()
parenthesizedOperator = (.)
