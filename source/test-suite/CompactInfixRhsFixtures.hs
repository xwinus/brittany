module CompactInfixRhsFixtures
  ( moduleSource
  , assertionSource
  , structuredRhs
  , reportedTuple
  , nestedSource
  , commentSources
  ) where

moduleSource :: [String] -> String
moduleSource declarations = unlines $ ["module CompactInfixRhs where", ""] ++ declarations

assertionSource :: Bool -> String -> String -> String
assertionSource chain operator rhs = moduleSource
  [ "example = do"
  , "  values " ++ (if chain then operator ++ " [0] " else "") ++ operator ++ " " ++ rhs
  ]

structuredRhs :: Bool -> Int -> String
structuredRhs tuple target = build (replicate first 'a') (replicate second 'b')
 where
  build left right = if tuple
    then "[(" ++ show left ++ ", " ++ show right ++ ", marker)]"
    else "[" ++ show left ++ " ++ " ++ show right ++ "]"
  padding = max 0 $ target - length (build "" "")
  first = padding `div` 2
  second = padding - first

reportedTuple :: String
reportedTuple = "[([0, 1], AlignmentCost 13 3 3 3 3 1 2, ConfiguredWidthOverflow)]"

nestedSource :: String -> String
nestedSource operator = moduleSource
  [ "example = do"
  , "  when condition $ do"
  , "    case value of"
  , "      Right plan -> do"
  , "        fmap rejectionSummary (alignmentPlanRejections plan)"
  , "          " ++ operator
  , "            " ++ reportedTuple
  , "      Left err -> fail err"
  ]

commentSources :: [(String, [String], String)]
commentSources =
  [ ( "an operator line comment"
    , ["  values `Hspec.shouldContain` -- Operator boundary", "    " ++ reportedTuple]
    , "-- Operator boundary"
    )
  , ( "an operator block comment"
    , ["  values `Hspec.shouldContain` {- Operator boundary -}", "    " ++ reportedTuple]
    , "{- Operator boundary -}"
    )
  , ( "a comment after the opening bracket"
    , [ "  values `Hspec.shouldContain` [ -- Opening boundary"
      , "    (first, second, marker)"
      , "    ]"
      ]
    , "-- Opening boundary"
    )
  , ( "a comment before the closing bracket"
    , [ "  values `Hspec.shouldContain` [ (first, second, marker)"
      , "    -- Closing boundary"
      , "    ]"
      ]
    , "-- Closing boundary"
    )
  , ( "a comment between tuple children"
    , [ "  values `Hspec.shouldContain` [(first -- Tuple boundary"
      , "    , second, marker)]"
      ]
    , "-- Tuple boundary"
    )
  ]
