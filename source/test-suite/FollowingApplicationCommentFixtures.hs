module FollowingApplicationCommentFixtures where

moduleSource :: [String] -> String
moduleSource declarations = unlines $ "module FollowingApplications where" : "" : declarations

primarySource :: String
primarySource = moduleSource
  [ "f = [ docNodeAnnKW lmod Nothing docEmpty"
  , "       -- A pseudo node that serves merely to force documentation"
  , "       -- before the node"
  , "    , next"
  , "    ]"
  ]

listSource :: String
listSource = moduleSource
  [ "f = [ build first second third"
  , "       -- after the call"
  , "       -- continuation"
  , "    , next"
  , "    ]"
  ]

longSource :: String
longSource = moduleSource
  [ "f = [ build longFirstArgument longSecondArgument longThirdArgument"
  , "       -- after the call"
  , "    , next"
  , "    ]"
  ]

nestedParenthesesSource :: String
nestedParenthesesSource = moduleSource
  [ "f = ((combine first second third"
  , "      -- after the call"
  , "    ))"
  ]

reportedComments :: [String]
reportedComments =
  [ "-- A pseudo node that serves merely to force documentation", "-- before the node" ]

contextCases :: [(String, String, [String])]
contextCases =
  [ ( "inline", moduleSource
      [ "f = [ build first second third -- after the call"
      , "    , next"
      , "    ]"
      ]
    , ["build first second third", "-- after the call", ", next"]
    )
  , ( "block", moduleSource
      [ "f = [ build first second third"
      , "       {- after the call -}"
      , "    , next"
      , "    ]"
      ]
    , ["build first second third", "{- after the call -}", ", next"]
    )
  , ( "tuple", moduleSource
      [ "f = ( build first second third"
      , "       -- after the call"
      , "    , next"
      , "    )"
      ]
    , ["build first second third", "-- after the call", ", next"]
    )
  , ( "record", moduleSource
      [ "f = Result { firstField = build first second third"
      , "             -- after the call"
      , "           , nextField = next"
      , "           }"
      ]
    , ["build first second third", "-- after the call", ", nextField"]
    )
  , ( "do", moduleSource
      [ "f = do"
      , "  build first second third"
      , "  -- after the call"
      , "  next"
      ]
    , ["build first second third", "-- after the call", "next"]
    )
  , ( "parens", moduleSource
      [ "f = (build first second third"
      , "       -- after the call"
      , "    ) `combine` next"
      ]
    , ["build first second third", "-- after the call", ")", "`combine`"]
    )
  , ( "blank runs", moduleSource
      [ "f = [ build first second third"
      , "       -- first run"
      , ""
      , "       -- second run"
      , "    , next"
      , "    ]"
      ]
    , ["build first second third", "-- first run", "-- second run", ", next"]
    )
  ]

betweenCases :: [(String, String, [String])]
betweenCases =
  [ ( "first", moduleSource
      [ "f = [build first -- middle note"
      , "             second third"
      , "    , next]"
      ]
    , ["build", "first", "-- middle note", "second", "third"]
    )
  , ( "second", moduleSource
      [ "f = [build first second -- middle note"
      , "             third"
      , "    , next]"
      ]
    , ["build", "first", "second", "-- middle note", "third"]
    )
  ]

boundaryCall :: Int -> Int -> Int -> String
boundaryCall columns indent extra =
  "build first second " ++ replicate (columns - indent - 2 - length "build first second " + extra) 'a'

boundarySource :: Int -> Int -> Int -> String
boundarySource columns indent extra = moduleSource
  [ "f = [" ++ boundaryCall columns indent extra, "       -- after the call"
  , "    , next", "    ]"
  ]

protectedRuns :: [(Bool, String, [Int])]
protectedRuns =
  [ ( False, moduleSource
      [ "f = [ build first second third"
      , "       -- Example:"
      , "         -- > first step"
      , "           -- > nested step"
      , "    , next"
      , "    ]"
      ]
    , [0, 2, 4]
    )
  , ( True, moduleSource
      [ "f = [ build first second third -- Example:"
      , "                               -- > first step"
      , "                                 -- > nested step"
      , "    , next"
      , "    ]"
      ]
    , [0, 0, 2]
    )
  ]

protectedPrefixCases :: [(String, String, [String], [Int], [String])]
protectedPrefixCases =
  [ withSeed "Haddock line" "-- ^ C1 documentation"
  , withSeed "Haddock block" "{-^ C1 documentation -}"
  , withSeed "pragma-like line" "-- BRITTANY C1 protected marker"
  , ( "protected prefix and separate ordinary suffix", moduleSource
      [ "f = [combine first second third"
      , "      -- ^ C1 documentation"
      , "      -- C2 continuation"
      , ""
      , "          -- C3 ordinary"
      , "            -- C4 offset"
      , "    , next]"
      ]
    , ["-- ^ C1 documentation", "-- C2 continuation"], [6, 6]
    , ["-- C3 ordinary", "-- C4 offset"]
    )
  ]
 where
  withSeed name seed =
    ( name, moduleSource
      [ "f = [combine first second third"
      , "      " ++ seed
      , "      -- C2 continuation"
      , "        -- C3 offset"
      , "    , next]"
      ]
    , [seed, "-- C2 continuation", "-- C3 offset"], [6, 6, 8], []
    )
