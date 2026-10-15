module BindArrowCommentFixtures where

moduleSource :: [String] -> String
moduleSource declarations = unlines $ "module BindArrowComments where" : "" : declarations

ordinaryComments :: [String]
ordinaryComments = ["-- explain the action", "-- continuation"]

runSource :: Bool -> String
runSource inline = moduleSource $
  ["f = do"]
  ++ (if inline
      then ["  x <- -- explain the action", "       -- continuation"]
      else ["  x <-", "    -- explain the action", "    -- continuation"])
  ++ ["       performAction", "  pure x"]

-- Each sequence describes punctuation-side ownership without fixing whitespace.
edgeCases :: [(String, Int, String, [String])]
edgeCases =
  [ ( "distinct comments before and after the arrow", 80
    , moduleSource
      [ "f = do", "  x -- binder note", "    <- -- action note"
      , "       performAction", "  pure x"
      ]
    , ["-- binder note", "<-", "-- action note", "performAction"]
    )
  , ( "a block comment before the arrow", 80
    , moduleSource
      [ "f = do", "  x {- binder block -} <- performAction", "  pure x" ]
    , ["{- binder block -}", "<-", "performAction"]
    )
  , ( "a pattern guard binding", 80
    , moduleSource
      [ "f value", "  | x <- -- guard action", "         -- guard continuation"
      , "         performAction value", "  = x", "  | otherwise = value"
      ]
    , ["<-", "-- guard action", "-- guard continuation", "performAction", "otherwise"]
    )
  , ( "blank-separated post-arrow runs", 80
    , moduleSource
      [ "f = do", "  x <- -- first run", "", "       -- second run"
      , "       performAction", "  pure x"
      ]
    , ["<-", "-- first run", "-- second run", "performAction"]
    )
  , ( "an inline multiline block comment", 80
    , moduleSource
      [ "f = do", "  x <- {- First line.", "          Body note."
      , "       -}", "       performAction", "  pure x"
      ]
    , ["<-", "{- First line.", "Body note.", "-}", "performAction"]
    )
  , ( "a compact post-arrow block comment", 80
    , moduleSource
      [ "f = do", "  x <- {- action note -} performAction", "  pure x" ]
    , ["<-", "{- action note -}", "performAction"]
    )
  , ( "an own-line block followed by an inline action", 80
    , moduleSource
      [ "f = do", "  x <-", "    {- action block -} performAction", "  pure x" ]
    , ["<-", "{- action block -}", "performAction"]
    )
  , ( "a post-doc marker after the arrow", 80
    , moduleSource
      [ "f = do", "  x <- -- ^ action documentation", "       -- continuation"
      , "       performAction", "  pure x"
      ]
    , ["<-", "-- ^ action documentation", "-- continuation", "performAction"]
    )
  , ( "a dedented run after a multiline pattern", 40
    , moduleSource
      [ "f = do", "  (firstValue, secondValue, thirdValue, fourthValue) <- -- action"
      , "    -- continuation", "    performAction", "  pure firstValue"
      ]
    , ["fourthValue", "<-", "-- action", "-- continuation", "performAction"]
    )
  , ( "an own-line block comment", 80
    , moduleSource
      [ "f = do", "  x <-", "    {- Own-line action note. -}"
      , "    performAction", "  pure x"
      ]
    , ["<-", "{- Own-line action note. -}", "performAction"]
    )
  , ( "a multiline tuple pattern", 40
    , moduleSource
      [ "f = do", "  (firstResult, secondResult, thirdResult) <- -- tuple note"
      , "    performAction", "  pure firstResult"
      ]
    , ["thirdResult", "<-", "-- tuple note", "performAction"]
    )
  , ( "a list comprehension generator", 80
    , moduleSource
      [ "f = [x | x <- -- generator note", "              -- generator continuation"
      , "              sourceValues, enabled x]"
      ]
    , ["<-", "-- generator note", "-- generator continuation", "sourceValues", "enabled"]
    )
  , ( "a monadic comprehension generator", 80
    , "{-# LANGUAGE MonadComprehensions #-}\n" ++ moduleSource
      [ "f = [x | x <- -- monadic note", "              sourceValues]" ]
    , ["<-", "-- monadic note", "sourceValues"]
    )
  , ( "a nested binding beside other statement comments", 80
    , moduleSource
      [ "f = do", "  prepare -- prior statement", "  when ready $ do"
      , "    x <- -- nested action", "         -- nested continuation"
      , "         performAction", "    pure x", "  finish -- later statement"
      ]
    , ["-- prior statement", "<-", "-- nested action", "-- nested continuation"
      , "performAction", "-- later statement"]
    )
  ]

protectedSource :: String
protectedSource = moduleSource
  [ "f = do", "  x <- -- Example:", "         -- > first step"
  , "         -- > second step", "       performAction", "  pure x"
  ]

maintainedComments :: [String]
maintainedComments =
  [ "-- at the moment, we don't support splitting patterns into"
  , "-- multiple lines. but we cannot enforce pasting everything"
  , "-- into one line either, because the type signature will ignore"
  , "-- this if we overflow sufficiently."
  , "-- In order to prevent syntactically invalid results in such"
  , "-- cases, we need the AddBaseY here."
  , "-- This can all change when patterns get multiline support."
  ]

widthCases :: [(String, Int, Int, String)]
widthCases =
  [ ( "long tuple pattern", 60, 2
    , moduleSource
      [ "f = do", "  (firstValue, secondValue, thirdValue, fourthValue) <- -- action"
      , "    -- continuation", "    action", "  pure firstValue"
      ]
    )
  , ( "view pattern", 40, 4
    , "{-# LANGUAGE ViewPatterns #-}\n" ++ moduleSource
      [ "f = do", "  (projectValue -> Just value) <- -- action"
      , "    -- continuation", "    action", "  pure value"
      ]
    )
  ]
