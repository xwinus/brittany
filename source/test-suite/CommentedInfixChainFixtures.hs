module CommentedInfixChainFixtures where

moduleSource :: [String] -> String
moduleSource declarations = unlines $ "module CommentedInfixChains where" : "" : declarations

compositionSource :: String
compositionSource = moduleSource
  [ "f = take limit"
  , "       -- prune so we always consider a constant"
  , "       -- amount of spacings per node of the BriDoc."
  , "     . specialNub"
  , "       -- Preserve deterministic order."
  , "       -- Keep valid spacings."
  , "     . filter hasOkColCount"
  , "     . preFilterLimit"
  , "     $ values"
  ]

applicationSource :: String
applicationSource = moduleSource
  [ "f = [ docSetParSpacing -- this is most likely superfluous because"
  , "                      -- this is a sequence of a one-line and a par-space"
  , "                      -- anyways, so it is _always_ par-spaced."
  , "    $ docAddBaseY BrIndentRegular"
  , "    $ docSeq [appSep $ docForceSingleline expDoc1, docForceParSpacing expDoc2]"
  , "    ]"
  ]

chainSource :: String -> Bool -> String
chainSource operator inline = moduleSource $
  (if inline
    then ["f = initialValue -- first note", "                 -- continuation"]
    else ["f = initialValue", "    -- first note", "    -- continuation"])
  ++ [ "  " ++ operator ++ " firstPart"
     , "    -- middle note"
     , "  " ++ operator ++ " secondPart"
     , "  " ++ operator ++ " finalPart"
     ]

-- Only operators in the same lexical chain share a requested column.
edgeCases :: [(String, String, [[String]])]
edgeCases =
  [ ( "parenthesized nested chains"
    , moduleSource
      [ "f = outer -- outer note", "  $ (inner -- inner note"
      , "       . secondFunction", "       . thirdFunction)"
      , "  $ finalValue"
      ]
    , [["$ (", "$ finalValue"], [". secondFunction", ". thirdFunction"]]
    )
  , ( "mixed composition and application"
    , moduleSource
      [ "f = firstFunction -- first note", "  . secondFunction"
      , "    -- middle note", "  . thirdFunction", "  $ finalValue"
      ]
    , [[". secondFunction", ". thirdFunction", "$ finalValue"]]
    )
  , ( "a do-block RHS"
    , moduleSource
      [ "f = decorate -- first note", "  $ transform", "    -- middle note"
      , "  $ do", "      firstAction", "      secondAction"
      ]
    , [["$ transform", "$ do"]]
    )
  , ( "a case-block RHS"
    , moduleSource
      [ "f = decorate -- first note", "  $ transform", "    -- middle note"
      , "  $ case value of", "      Just item -> use item", "      Nothing -> fallback"
      ]
    , [["$ transform", "$ case"]]
    )
  , ( "blank-separated comment runs"
    , moduleSource
      [ "f = firstPart -- first run", "", "    -- second run"
      , "  ++ secondPart", "    -- middle note", "  ++ thirdPart", "  ++ finalPart"
      ]
    , [["++ secondPart", "++ thirdPart", "++ finalPart"]]
    )
  , ( "block comments between operators"
    , moduleSource
      [ "f = firstPart", "    {- first block -}", "  ++ secondPart"
      , "    {- second block -}", "  ++ thirdPart", "  ++ finalPart"
      ]
    , [["++ secondPart", "++ thirdPart", "++ finalPart"]]
    )
  ]

blockEdgeCases :: [(String, String, String)]
blockEdgeCases =
  [ ( "an if RHS", "$ if"
    , moduleSource
      [ "f = firstFunction -- C1", "  . secondFunction $ if condition"
      , "    then action -- C2", "    else fallback"
      ]
    )
  , ( "a lambda RHS", "$ \\value"
    , moduleSource
      [ "f = firstFunction -- C1", "  . secondFunction $ \\value ->"
      , "    -- C2", "    otherFunction value"
      ]
    )
  , ( "a case RHS with a wide indent", "$ case"
    , moduleSource
      [ "f = firstFunction -- C1", "  . secondFunction $ case value of"
      , "    -- C2", "    Just x -> action x", "    Nothing -> fallback"
      ]
    )
  ]

scopedHeaderCases :: [(String, String, [String], String)]
scopedHeaderCases =
  [ ( "explanation after an application operator", "outer $"
    , ["outer $", "-- C1", "-- C2", "middle", "$ finalValue"]
    , moduleSource
      [ "f = outer $", "  -- C1", "  -- C2", "  middle $ finalValue" ]
    )
  , ( "a list operand's internal comment", "wrap $ build"
    , ["wrap $ build", "[ firstValue", "-- C1", ", secondValue", "]", ": remainingValues"]
    , moduleSource
      [ "f = wrap $ build", "  [ firstValue", "  -- C1", "  , secondValue"
      , "  ] : remainingValues"
      ]
    )
  ]

inlineCompositionSource :: String
inlineCompositionSource = moduleSource
  [ "f = firstFunction -- C1", "  . secondFunction -- C2"
  , "  . thirdFunction . fourthFunction"
  ]

rhsBaseCases :: [(String, String, String)]
rhsBaseCases =
  [ ( "list comprehension", "++", moduleSource
      [ "f = outer"
      , "  $ -- C1"
      , "    [ buildResult"
      , "        (Constructor argument)"
      , "        [firstArgument, secondArgument]"
      , "    | predicate"
      , "    , otherPredicate"
      , "    ]"
      , "  ++ -- C2"
      , "    [otherValue | otherPredicate]"
      ]
    )
  , ( "tuple", "++", moduleSource
      [ "f = outer"
      , "  $ -- C1"
      , "    (buildResult firstArgument secondArgument, otherValue)"
      , "  ++ -- C2"
      , "    finalValue"
      ]
    )
  , ( "application", "++", moduleSource
      [ "f = outer"
      , "  $ -- C1"
      , "    buildResult"
      , "      (Constructor argument)"
      , "      [firstArgument, secondArgument]"
      , "  ++ -- C2"
      , "    finalValue"
      ]
    )
  , ( "do block", ">>", moduleSource
      [ "f = outer"
      , "  $ -- C1"
      , "    (do"
      , "      firstAction"
      , "      secondAction)"
      , "  >> -- C2"
      , "    finalAction"
      ]
    )
  , ( "case block", ">>", moduleSource
      [ "f = outer"
      , "  $ -- C1"
      , "    (case value of"
      , "      Just x -> firstAction x"
      , "      Nothing -> secondAction)"
      , "  >> -- C2"
      , "    finalAction"
      ]
    )
  ]

operatorBlockSource :: String
operatorBlockSource = moduleSource
  [ "f = outer", "  $ {- C1 -}"
  , "    [ buildResult firstArgument secondArgument thirdArgument"
  , "    | predicate", "    ]", "  ++ -- C3"
  , "    [otherValue | otherPredicate]"
  ]
