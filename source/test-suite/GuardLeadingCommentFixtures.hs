module GuardLeadingCommentFixtures where

moduleSource :: [String] -> String
moduleSource declarations = unlines $ "module GuardLeadingComments where" : "" : declarations

primarySource :: String
primarySource = moduleSource
  [ "f x = g x"
  , "  where"
  , "        g x"
  , "          -- explain the guard"
  , "          | x > 0 = x"
  , "        g x = 0"
  ]

edgeCases :: [(String, String, [(String, String, Int)])]
edgeCases =
  [ ( "subsequent line", moduleSource
      [ "f x = g x"
      , "  where"
      , "        g x"
      , "          | x > 0 = x"
      , "          -- explain the fallback"
      , "          | otherwise = 0"
      ]
    , [("-- explain the fallback", "| otherwise", 0)]
    )
  , ( "let", moduleSource
      [ "f x ="
      , "  let"
      , "        g y"
      , "          -- explain the guard"
      , "          | y > 0 = y"
      , "        g y = 0"
      , "  in g x"
      ]
    , [("-- explain the guard", "| y > 0", 0)]
    )
  , ( "first block", moduleSource
      [ "f x = g x"
      , "  where"
      , "        g x"
      , "          {- explain the guard -}"
      , "          | x > 0 = x"
      , "        g x = 0"
      ]
    , [("{- explain the guard -}", "| x > 0", 0)]
    )
  , ( "subsequent block", moduleSource
      [ "f x = g x"
      , "  where"
      , "        g x"
      , "          | x > 0 = x"
      , "          {- explain the fallback -}"
      , "          | otherwise = 0"
      ]
    , [("{- explain the fallback -}", "| otherwise", 0)]
    )
  , ( "blank runs", moduleSource
      [ "f x = g x"
      , "  where"
      , "        g x"
      , "          -- first run"
      , ""
      , "          -- second run"
      , "          | x > 0 = x"
      , "        g x = 0"
      ]
    , [("-- first run", "| x > 0", 0), ("-- second run", "| x > 0", 0)]
    )
  , ( "protected", moduleSource
      [ "f x = g x"
      , "  where"
      , "        g x"
      , "          -- Example:"
      , "            -- > first step"
      , "              -- > nested step"
      , "          | x > 0 = x"
      , "        g x = 0"
      ]
    , [("-- Example:", "| x > 0", 0), ("-- > first step", "| x > 0", 2), ("-- > nested step", "| x > 0", 4)]
    )
  , ( "multiline pattern", moduleSource
      [ "f x = g x"
      , "  where"
      , "        g (firstValue, secondValue, thirdValue)"
      , "          -- explain the guard"
      , "          | firstValue > 0 = secondValue"
      , "        g _ = 0"
      ]
    , [("-- explain the guard", "| firstValue > 0", 0)]
    )
  ]

trailingSource :: String
trailingSource = moduleSource
  [ "f x = x -- result trailing"
  , "g y"
  , "  | y > 0 = y"
  , "  | otherwise = 0"
  ]

haddockSource :: String
haddockSource = moduleSource
  [ "-- | Function documentation."
  , "f x"
  , "  | x > 0 = x"
  , "  | otherwise = 0"
  ]

diagramSource :: String
diagramSource = moduleSource
  [ "f x = x"
  , "    -- Example:"
  , "    --   +---+"
  , "    --   | x |"
  , "    --   +---+"
  , "g y = y"
  ]

boundaryMutants :: [(String, [String], [String])]
boundaryMutants =
  [ ( "moving a leading comment after its guard pipe"
    , ["f x", "  -- boundary note", "  | x > 0 = x", "  | otherwise = 0"]
    , ["f x", "  | -- boundary note", "    x > 0 = x", "  | otherwise = 0"]
    )
  , ( "moving a comment to a different guarded RHS"
    , ["f x", "  | x > 0 = x", "  -- boundary note", "  | otherwise = 0"]
    , ["f x", "  -- boundary note", "  | x > 0 = x", "  | otherwise = 0"]
    )
  ]

shortGuardSource :: String
shortGuardSource = moduleSource
  [ "f x", "        -- explain the guard", "        | x > 0 = x"
  , "        | otherwise = 0"
  ]

blankBeforeGuardSource :: String
blankBeforeGuardSource = moduleSource
  [ "f x", "        -- explain the guard", "", "        | x > 0 = x"
  , "        | otherwise = 0"
  ]

suffixCases :: [(String, String, String, [(String, Int)])]
suffixCases =
  [ ( "inline seed blank suffix", moduleSource
      [ "f x -- C1 inline"
      , "        -- C2 continuation"
      , ""
      , "        -- C3 guard"
      , "        | x > 0 = x"
      ]
    , unlines ["f x -- C1 inline", "    -- C2 continuation", ""]
    , [("-- C3 guard", 0)]
    )
  , ( "haddock blank suffix", moduleSource
      [ "f x"
      , "        -- | C1 Haddock"
      , ""
      , "        -- C2 guard"
      , "        | x > 0 = x"
      ]
    , unlines ["f x", "        -- | C1 Haddock", ""]
    , [("-- C2 guard", 0)]
    )
  , ( "ordinary protected suffix", moduleSource
      [ "f x"
      , "        -- C1 ordinary"
      , ""
      , "        -- | C2 Haddock"
      , ""
      , "          -- C3 guard"
      , "            -- C4 offset"
      , "        | x > 0 = x"
      ]
    , unlines ["f x", "        -- C1 ordinary", "", "        -- | C2 Haddock", ""]
    , [("-- C3 guard", 0), ("-- C4 offset", 2)]
    )
  ]
