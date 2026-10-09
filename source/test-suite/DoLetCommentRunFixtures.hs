module DoLetCommentRunFixtures
  ( moduleSource
  , runSource
  , ordinaryComments
  , bindingContexts
  , protectedRuns
  , blockSources
  , controlSources
  , failingCommentSource
  , precedingStatementSource
  ) where

moduleSource :: [String] -> String
moduleSource declarations = unlines $ ["module DoLetCommentRuns where", ""] ++ declarations

ordinaryComments :: [String]
ordinaryComments = ["-- First explanation.", "-- Continuing explanation."]

runSource :: Bool -> Bool -> String
runSource inline separated = moduleSource $
  ["f = do"] ++ beginning ++ ["    x = 1", "  pure x"]
 where
  [first, second] = ordinaryComments
  beginning = if inline
    then ["  let " ++ first] ++ blank ++ ["      " ++ second]
    else ["  let", "    " ++ first] ++ blank ++ ["    " ++ second]
  blank = ["" | separated]

bindingContexts :: [(String, [String], String)]
bindingContexts =
  [ ( "multiple bindings"
    , [ "f = do", "  let", "    -- First explanation.", "    -- Continuing explanation."
      , "    x = 1", "    y = 2", "  pure (x + y)"
      ]
    , "x"
    )
  , ( "a local type signature"
    , [ "f = do", "  let", "    -- First explanation.", "    -- Continuing explanation."
      , "    x :: Int", "    x = 1", "  pure x"
      ]
    , "x"
    )
  , ( "a nested do block"
    , [ "f = do", "  when ready $ do", "    let"
      , "      -- First explanation.", "      -- Continuing explanation."
      , "      x = 1", "    pure x"
      ]
    , "x"
    )
  , ( "a later binding"
    , [ "f = do", "  let x = 1", "      -- First explanation."
      , "      -- Continuing explanation.", "      y = 2", "  pure (x + y)"
      ]
    , "y"
    )
  ]

protectedRuns :: [(String, [String])]
protectedRuns =
  [ ("Haddock", ["-- | Local documentation.", "-- More documentation."])
  , ("a diagram", ["-- +---+", "-- | x |", "-- +---+"])
  , ("indented examples", ["-- Example:", "  -- > x + 1", "  -- > pure x"])
  , ("mixed prose and documentation", ["-- Ordinary intro.", "-- | Documentation.", "-- More prose."])
  , ("separated protected runs", ["-- First prose.", "", "-- > example", "", "-- Last prose."])
  , ("repeated comments", ["-- Repeated note.", "-- Repeated note."])
  ]

blockSources :: [(String, [String])]
blockSources =
  [ ( "an own-line multiline block"
    , [ "f = do", "  let", "    {- First line.", "       Body note."
      , "    -}", "    x = 1", "  pure x"
      ]
    )
  , ( "an inline multiline block"
    , [ "f = do", "  let {- First line.", "        Body note."
      , "      -}", "    x = 1", "  pure x"
      ]
    )
  ]

controlSources :: [(String, [String])]
controlSources =
  [ ( "let-in"
    , [ "f = let", "      -- First explanation.", "      -- Continuing explanation."
      , "      x = 1", "    in x"
      ]
    )
  , ( "where"
    , [ "f = x", " where", "  -- First explanation.", "  -- Continuing explanation."
      , "  x = 1"
      ]
    )
  , ( "comments before the let statement"
    , [ "f = do", "  -- First explanation.", "  -- Continuing explanation."
      , "  let x = 1", "  pure x"
      ]
    )
  ]

failingCommentSource :: String
failingCommentSource = moduleSource
  [ "choose value | ready = case value of -- Header explanation."
  , "  True -> good"
  , "  False -> bad"
  ]

precedingStatementSource :: String
precedingStatementSource = moduleSource
  [ "f = do"
  , "  prepare -- Comment about prior statement."
  , "  -- Comment about the let statement."
  , "  let"
  , "    -- Comment about local binding."
  , "    -- Continuing binding explanation."
  , "    x = 1"
  , "  pure x"
  ]
