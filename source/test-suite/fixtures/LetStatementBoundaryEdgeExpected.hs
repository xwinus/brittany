module LetStatementBoundaryEdge where

edge action = do
  let first  = prepare action
      second = prepareOther action
  (
    do
      nestedStart first
      (
        do
          innerStep second
        ) `nestedOperator` second
      nestedFinish second
    ) `onException` first
  (
    case action of
      Just value -> accept value
      Nothing    -> reject
    ) `orElse` fallback
  continue action
