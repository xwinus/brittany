combine first second =
  -- declaration
  let
    -- bindings
    selected = (first  -- first option
                      <|> second -- second option
                                )
  in  selected -- result
