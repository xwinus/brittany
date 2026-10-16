module TrailingSignatureCommentFixtures where

moduleSource :: [String] -> String
moduleSource declarations = unlines $ "module TrailingSignatureComments where" : "" : declarations

reportedSource :: String
reportedSource = moduleSource
  [ "f"
  , "  :: Map.Map AnnKey ((Int, Int), (Int, Int))  -- span map: key -> (start, end)"
  , "  -> Map.Map AnnKey ()  -- override keys to skip"
  , "  -> Anns  -- input annotations"
  , "  -> Anns  -- output with inner comments redistributed"
  , "f = undefined"
  ]

finalComments :: [String]
finalComments = ["-- final result", "{- final result -}", "-- ^ final result"]

signatureSource :: String -> String -> String
signatureSource firstType finalComment = moduleSource
  ["f", "  :: " ++ firstType, "  -> Result " ++ finalComment, "f = undefined"]

controlCases :: [(String, String, [String])]
controlCases =
  [ ( "a comment after the name"
    , moduleSource
      [ "f -- name note", "  :: Input -> Result -- final note", "f = undefined" ]
    , ["-- name note", "::", "Input", "Result", "-- final note"]
    )
  , ( "a comment after the separator"
    , moduleSource
      [ "f", "  :: -- separator note", "     Input -> Result -- final note"
      , "f = undefined"
      ]
    , ["::", "-- separator note", "Input", "Result", "-- final note"]
    )
  , ( "comments after internal type components"
    , moduleSource
      [ "f", "  :: Input -- first component", "  -> Middle -- middle component"
      , "  -> Result -- final note", "f = undefined"
      ]
    , ["Input", "-- first component", "Middle", "-- middle component", "Result", "-- final note"]
    )
  , ( "an own-line comment between type components"
    , moduleSource
      [ "f", "  :: Input", "  -- input note", "  -> Result -- final note"
      , "f = undefined"
      ]
    , ["Input", "-- input note", "Result", "-- final note"]
    )
  , ( "a long first type"
    , signatureSource "Either (FirstType LongArgument) (SecondType AnotherArgument) -- first type" "-- final note"
    , ["Either", "FirstType", "LongArgument", "SecondType", "AnotherArgument", "Result", "-- final note"]
    )
  , ( "multiple signature names"
    , moduleSource
      [ "f, g :: Input -> Result -- final note", "f = undefined", "g = undefined" ]
    , ["f", "g", "::", "Input", "Result", "-- final note"]
    )
  ]

widthRegressionCases :: [(String, Int, Int, Bool, String)]
widthRegressionCases =
  [ ( "a long final type", 80, 4, True
    , moduleSource
      [ "f :: FirstType -> LongContainerType FirstLongParameter SecondLongParameter ThirdLongParameter -- final result"
      , "f = undefined"
      ]
    )
  , ( "hanging multiple names", 60, 4, True, multipleNameSource )
  , ( "narrow hanging multiple names", 40, 2, True, multipleNameSource )
  , ( "mixed final block and line comments", 60, 4, False
    , moduleSource
      [ "f :: FirstType -> ResultType {- final block -} -- final line", "f = undefined" ]
    )
  , ( "protected final continuations", 40, 2, False
    , moduleSource
      [ "f :: FirstType -> ResultType -- result root"
      , "                              -- child"
      , "                                -- grandchild"
      , "f = undefined"
      ]
    )
  ]

multipleNameSource :: String
multipleNameSource = moduleSource
  [ "firstFunction, secondFunction :: FirstType -> ResultType -- final result"
  , "firstFunction = undefined", "secondFunction = undefined"
  ]
