module StructuralCaseDoFixtures where

import Data.List (intercalate)

moduleSource :: [String] -> String
moduleSource declarations = unlines $
  ["{-# LANGUAGE LambdaCase #-}", "module StructuralCaseBody where", ""]
    ++ declarations

constructorPattern :: String
constructorPattern = "Constructor firstSelectedValue secondSelectedValue thirdSelectedValue"

reducedSource :: String
reducedSource = unlines
  [ "module StructuralCaseBody where"
  , ""
  , "match value = case value of"
  , "  " ++ constructorPattern ++ " -> do"
  , "    check firstSelectedValue"
  , "    check secondSelectedValue"
  ]

reducedExpected :: String
reducedExpected = unlines
  [ "module StructuralCaseBody where"
  , ""
  , "match value = case value of"
  , "  Constructor"
  , "    firstSelectedValue"
  , "    secondSelectedValue"
  , "    thirdSelectedValue -> do"
  , "      check firstSelectedValue"
  , "      check secondSelectedValue"
  ]

patternCases :: [(String, String)]
patternCases =
  [ ("constructor", "Constructor " ++ unwords arguments)
  , ("record", "Record { " ++ intercalate ", " fields ++ " }")
  , ("list", "[" ++ intercalate ", " arguments ++ "]")
  , ("tuple", "(" ++ intercalate ", " arguments ++ ")")
  , ("cons", intercalate " : " arguments)
  ]
 where
  arguments = ["firstSelectedArgument", "secondSelectedArgument"
    , "thirdSelectedArgument", "fourthSelectedArgument", "fifthSelectedArgument"]
  fields = zipWith (\name argument -> name ++ " = " ++ argument)
    ["firstField", "secondField", "thirdField", "fourthField", "fifthField"] arguments

caseSource :: Bool -> String -> String
caseSource lambdaCase patternText = moduleSource
  [ if lambdaCase then "match = \\case" else "match value = case value of"
  , "  " ++ patternText ++ " -> do"
  , "    firstAction"
  , "    secondAction"
  , "  _ -> fallback"
  ]

nestedSource :: String -> String
nestedSource patternText = moduleSource
  [ "outer value = do"
  , "  case value of"
  , "    " ++ patternText ++ " -> do"
  , "      result <- do"
  , "        innerAction"
  , "        pure value"
  , "      case result of"
  , "        Just selected -> consume selected"
  , "        Nothing -> recover"
  , "      afterInnerCase"
  , "    _ -> fallback"
  , "  afterOuterCase"
  ]

boundarySource :: Int -> Int -> Int -> String
boundarySource columns indent extra = caseSource False $
  "Constructor firstSelectedValue " ++ boundaryArgument columns indent extra

boundaryArgument :: Int -> Int -> Int -> String
boundaryArgument columns indent extra =
  "last" ++ replicate (columns - 2 * indent - 10 + extra) 'x'

commentCases :: [(String, [String])]
commentCases =
  [ ("a line comment after the arrow",
      ["  " ++ constructorPattern ++ " -> -- arrow note"
      , "    do", "      firstAction", "      secondAction"])
  , ("a leading comment before do",
      ["  " ++ constructorPattern ++ " ->"
      , "    -- body note", "    do", "      firstAction", "      secondAction"])
  , ("an inline statement block comment",
      ["  " ++ constructorPattern ++ " -> do"
      , "    firstAction {- action note -}", "    secondAction"])
  , ("a line comment after do",
      ["  " ++ constructorPattern ++ " -> do -- keyword note"
      , "    firstAction", "    secondAction"])
  , ("a leading statement comment",
      ["  " ++ constructorPattern ++ " -> do"
      , "    -- first action", "    firstAction", "    secondAction"])
  , ("a source-sensitive multiline statement comment",
      ["  " ++ constructorPattern ++ " -> do"
      , "    {- first action", "       continuation -}"
      , "    firstAction", "    secondAction"])
  ]

malformedCases :: [(String, [String])]
malformedCases =
  [ ("a missing case arrow",
      ["match value = case value of", "  Constructor selected do", "    firstAction"])
  , ("a malformed do binding",
      ["match value = case value of", "  Constructor selected -> do", "    <- selected"])
  ]
