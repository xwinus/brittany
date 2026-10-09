module GuardedBlockHeaderFixtures
  ( moduleSource
  , caseHeader
  , guardedSource
  , attachedSource
  , extraGuardCases
  , neighboringBlocks
  , commentedBlocks
  ) where

moduleSource :: [String] -> String
moduleSource declarations = unlines $
  ["{-# LANGUAGE LambdaCase #-}", "module GuardedBlockHeaders where", ""] ++ declarations

caseHeader :: Int -> String
caseHeader columns = if columns == 40
  then "case pick value of"
  else "case filter (not . null . dropWhile (== ' ')) rest of"

guardedSource :: String -> String -> String
guardedSource context header = moduleSource $ case context of
  "function" -> ["choose value"] ++ equation
  "pattern" -> ["(first, second)"] ++ equation
  "case" -> ["choose input = case input of", "  Just value"] ++ alternative
    ++ ["  Nothing -> fallback"]
  _ -> ["choose = inspect $ \\case", "  Just value"] ++ alternative
    ++ ["  Nothing -> fallback"]
 where
  equation =
    [ "  | isSelected value && isEnabled value ="
    , "    " ++ header
    , "      True -> good"
    , "      False -> bad"
    ]
  alternative =
    [ "    | isSelected value && isEnabled value ->"
    , "      " ++ header
    , "        True -> good"
    , "        False -> bad"
    ]

attachedSource :: Bool -> String
attachedSource alternative = moduleSource $ if alternative
  then
    [ "choose input = case input of"
    , "  Just value | ready -> case value of"
    , "    True -> good"
    , "    False -> bad"
    , "  Nothing -> fallback"
    ]
  else
    [ "choose value | ready = case value of"
    , "  True -> good"
    , "  False -> bad"
    ]

extraGuardCases :: [(String, [String], [String])]
extraGuardCases =
  [ ( "multiple boolean guards"
    , [ "choose value | isSelected value, isEnabled value = case pick value of"
      , "  True -> good", "  False -> bad"
      ]
    , ["case pick value of"]
    )
  , ( "a pattern guard"
    , [ "choose value"
      , "  | Just selected <- lookup value, ready selected = case pick selected of"
      , "    True -> good", "    False -> bad"
      , "  | otherwise = fallback"
      ]
    , ["case pick selected of"]
    )
  , ( "multiple guarded clauses"
    , [ "choose value"
      , "  | isSelected value = case pick value of"
      , "    True -> good", "    False -> bad"
      , "  | otherwise = case fallback value of"
      , "    True -> otherGood", "    False -> otherBad"
      ]
    , ["case pick value of", "case fallback value of"]
    )
  ]

neighboringBlocks :: [(String, [String], String, String)]
neighboringBlocks =
  [ ( "do"
    , ["choose value | ready = do", "  first <- " ++ doCall, "  finish first"]
    , "do"
    , doCall
    )
  , ( "let"
    , [ "choose value | ready ="
      , "  let first = " ++ letCall
      , "      second = transform first"
      , "  in use second"
      ]
    , "let"
    , letCall
    )
  , ( "if"
    , [ "choose value | ready = if isReady value"
      , "  then " ++ ifCall, "  else secondAction value"
      ]
    , "if isReady value"
    , ifCall
    )
  ]
 where
  arguments = " firstArgument secondArgument thirdArgument fourthArgument"
  doCall = "performSelectedAction" ++ arguments
  letCall = "buildSelectedValue" ++ arguments
  ifCall = "firstSelectedAction" ++ arguments

commentedBlocks :: [(String, [String], String)]
commentedBlocks =
  [ ( "a guard explanation"
    , [ "choose value"
      , "  -- Guard explanation."
      , "  | isSelected value && isEnabled value = case pick value of"
      , "    True -> good", "    False -> bad"
      ]
    , "-- Guard explanation."
    )
  , ( "a binder line comment"
    , [ "choose value | isSelected value = -- Binder explanation."
      , "  case pick value of", "    True -> good", "    False -> bad"
      ]
    , "-- Binder explanation."
    )
  , ( "an own-line header explanation"
    , [ "choose value | isSelected value ="
      , "  -- Header explanation."
      , "  case pick value of"
      , "    True -> good", "    False -> bad"
      ]
    , "-- Header explanation."
    )
  , ( "a branch explanation"
    , [ "choose value | isSelected value = case pick value of"
      , "  -- Branch explanation."
      , "  True -> good", "  False -> bad"
      ]
    , "-- Branch explanation."
    )
  , ( "a binder block comment"
    , [ "choose value | isSelected value = {- Binder explanation. -}"
      , "  case pick value of", "    True -> good", "    False -> bad"
      ]
    , "{- Binder explanation. -}"
    )
  ]
