module LambdaCaseCommentFixtures
  ( ordinaryCases
  , protectedRuns
  , moduleSource
  , shiftedSource
  ) where

moduleSource :: [String] -> String
moduleSource declarations = unlines $
  ["{-# LANGUAGE LambdaCase #-}", "module LambdaCaseComments where", ""]
    ++ declarations

shiftedSource :: [String] -> String
shiftedSource alternatives = moduleSource $
  [ "outer = stepFull"
  , " where"
  , "  stepFull = -- trace"
  , "             id $ \\case"
  ] ++ alternatives

ordinaryCases :: [(String, [String], [(String, String)])]
ordinaryCases =
  [ ( "the reported later explanation"
    , [ "    True -> 1"
      , "    -- Return the default value."
      , "    False -> 0"
      ]
    , [("-- Return the default value.", "False ->")]
    )
  , ( "first and later explanations"
    , [ "    -- First explanation."
      , "    True -> 1"
      , "    -- Later explanation."
      , "    False -> 0"
      ]
    , [("-- First explanation.", "True ->"), ("-- Later explanation.", "False ->")]
    )
  , ( "nested lambda-case explanations"
    , [ "    -- Outer first explanation."
      , "    Just value -> consume value $ \\case"
      , "      -- Inner first explanation."
      , "      True -> 1"
      , "      -- Inner later explanation."
      , "      False -> 0"
      , "    -- Outer later explanation."
      , "    Nothing -> fallback"
      ]
    , [ ("-- Outer first explanation.", "Just value")
      , ("-- Inner first explanation.", "True ->")
      , ("-- Inner later explanation.", "False ->")
      , ("-- Outer later explanation.", "Nothing ->")
      ]
    )
  , ( "explanations after a multiline right-hand side"
    , [ "    True -> do"
      , "      inspect value"
      , "      finish value"
      , "    -- Later multiline explanation."
      , "    False -> fallback"
      ]
    , [("-- Later multiline explanation.", "False ->")]
    )
  , ( "contiguous prose before both alternatives"
    , [ "    -- First paragraph."
      , "    -- First continuation."
      , "    True -> 1"
      , "    -- Later paragraph."
      , "    -- Later continuation."
      , "    False -> 0"
      ]
    , [ ("-- First paragraph.", "True ->")
      , ("-- First continuation.", "True ->")
      , ("-- Later paragraph.", "False ->")
      , ("-- Later continuation.", "False ->")
      ]
    )
  ]

protectedRuns :: [(String, [String])]
protectedRuns =
  [ ("Haddock", ["    -- | Branch documentation."])
  , ( "a diagram with varying physical columns"
    , [ "    -- branch diagram"
      , "      --   left -> right"
      , "    --        ^ pointer"
      ]
    )
  , ( "an indented code example"
    , ["    -- > case value of", "    -- >   Just x -> x"]
    )
  , ( "deliberately indented prose"
    , ["    -- Outer explanation.", "      -- Inner explanation."]
    )
  , ( "a contiguous mixed protected and prose run"
    , [ "    -- Ordinary before."
      , "    --   left -> right"
      , "    -- Ordinary after."
      ]
    )
  , ( "separated ordinary and protected runs"
    , [ "    -- Earlier ordinary explanation."
      , ""
      , "    -- | Protected documentation."
      , ""
      , "    -- Later ordinary explanation."
      ]
    )
  ]
