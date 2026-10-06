module SourceFragmentBoundaryFixtures
  ( bindingSource
  , nestedSources
  , moduleSource
  , condition
  , narrowCondition
  , overwideCondition
  , longComment
  ) where

moduleSource :: [String] -> String
moduleSource declarations = unlines $ ["module SourceFragmentBoundary where", ""] ++ declarations

condition :: String
condition = "List.last fixedPosXs + fst (List.last list) > colMax"

narrowCondition :: String
narrowCondition = "left + right > limit"

overwideCondition :: String
overwideCondition = "firstValue + secondValue + thirdValue + fourthValue + fifthValue > limit"

longComment :: String
longComment = "-- Keep this explanation beside the binding before testing the limit."

bindingSource :: String -> String -> String
bindingSource comment predicate = moduleSource
  [ "f = do"
  , "  let result = " ++ comment
  , "        if " ++ predicate
  , "          then noAlignAct"
  , "          else alignAct"
  , "  result"
  ]

nestedSources :: [(String, String)]
nestedSources =
  [ ( "a nested where binding"
    , moduleSource
        [ "outer = wrapper result"
        , " where"
        , "  result = " ++ longComment
        , "    if " ++ condition
        , "      then noAlignAct"
        , "      else alignAct"
        ]
    )
  , ( "an analogous case arrow"
    , moduleSource
        [ "f value = case value of"
        , "  Just selected -> " ++ longComment
        , "    if " ++ condition
        , "      then noAlignAct"
        , "      else alignAct"
        , "  Nothing -> alignAct"
        ]
    )
  ]
