module MembershipPredicateFixtures
  ( moduleSource
  , predicateUnits
  , contextualCases
  , commentCases
  , shadowedCases
  ) where

moduleSource :: [String] -> String
moduleSource declarations = unlines $ ["module MembershipPredicate where", ""] ++ declarations

predicateUnits :: Int -> String -> (String, String)
predicateUnits columns membership
  | columns == 40 =
      ("name `" ++ membership ++ "` items value", "result value == Enabled")
  | otherwise =
      ( "name `" ++ membership ++ "` matrixCaseFeatures matrixCase"
      , "matrixCaseExpectedResult matrixCase == Formats"
      )

contextualCases :: [(String, [String], [String])]
contextualCases =
  [ ( "explicit parentheses"
    , ["selected = (name `elem` features value) && (result value == Enabled) || (name `notElem` disabled value)"]
    , ["(name `elem` features value)", "(result value == Enabled)", "(name `notElem` disabled value)"]
    )
  , ( "a nested do expression"
    , [ "selected value = do"
      , "  pure $ name `elem` features value && result value == Enabled || name `notElem` disabled value"
      ]
    , ["name `elem` features value", "result value == Enabled", "name `notElem` disabled value"]
    )
  , ( "multiple boolean groups"
    , ["selected = first `elem` allowed && second `notElem` excluded || status == Ready && final /= Disabled"]
    , ["first `elem` allowed", "second `notElem` excluded", "status == Ready", "final /= Disabled"]
    )
  , ( "a parenthesized callback operand"
    , ["selected = name `elem` map (\\x -> transform x) values && result value == Enabled"]
    , ["name `elem` map (\\x -> transform x) values", "result value == Enabled"]
    )
  ]

commentCases :: [(String, [String], [String])]
commentCases =
  [ ( "an inline line comment"
    , ["selected = (x `elem` xs) -- membership note", "  && (y `notElem` ys) && (z == Z)"]
    , ["-- membership note"]
    )
  , ( "an own-line block comment"
    , ["selected = (x `elem` xs) &&", "  {- conjunction note -}", "  (y `notElem` ys) && (z == Z)"]
    , ["{- conjunction note -}"]
    )
  , ( "an own-line explanation"
    , ["selected = (x `elem` xs) &&", "  -- conjunction note", "  (y `notElem` ys) && (z == Z)"]
    , ["-- conjunction note"]
    )
  , ( "a multiline block comment"
    , ["selected = (x `elem` xs) &&", "  {- conjunction note", "     continuation note -}", "  (y `notElem` ys) && (z == Z)"]
    , ["{- conjunction note", "continuation note -}"]
    )
  ]

shadowedCases :: [(String, [String])]
shadowedCases =
  [ ( "declared membership fixities"
    , [ "infixr 0 `elem`"
      , "infixl 8 `notElem`"
      , "elem left right = left"
      , "notElem left right = right"
      , "selected = (first `elem` allowed) && second `notElem` excluded || status == Ready"
      ]
    )
  , ( "locally shadowed membership names"
    , [ "selected = first `elem` allowed && second `notElem` excluded || status == Ready"
      , " where"
      , "  elem left right = left"
      , "  notElem left right = right"
      ]
    )
  ]
