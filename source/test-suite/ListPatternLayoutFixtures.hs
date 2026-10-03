module ListPatternLayoutFixtures
  ( reportedSource
  , commentCases
  , barePatternContexts
  , malformedLists
  , moduleSource
  ) where

reportedSource :: String -> String
reportedSource owner = moduleSource
  [ "outer = do"
  , "  describe \"repro\" $ do"
  , "    case value of"
  , "      Right lowered -> do"
  , "        case lowered of"
  , "          BDSeq"
  , "            [ BDComment planned"
  , "            , BDAnnotationPrior PriorCommentSource " ++ owner
  , "                (BDAlt alternatives)"
  , "            ] -> do"
  , "              check " ++ owner
  , "              check alternatives"
  ]

commentCases :: [(String, [String])]
commentCases =
  [ ("comments before a comma",
      [ "  [ First firstArgument secondArgument"
      , "    -- [,] Separator explanation."
      , "  , Second thirdArgument fourthArgument"
      , "  , Third fifthArgument sixthArgument"
      ])
  , ("comments after a comma",
      [ "  [ First firstArgument secondArgument"
      , "  , -- [,] Next element explanation."
      , "    Second thirdArgument fourthArgument"
      , "  , Third fifthArgument sixthArgument"
      ])
  , ("inline element comments",
      [ "  [ First firstArgument secondArgument -- First note."
      , "  , Second thirdArgument fourthArgument -- Second note."
      , "  , Third fifthArgument sixthArgument"
      ])
  , ("block comments at a separator",
      [ "  [ First firstArgument secondArgument"
      , "  , {- [,] Separator note. -} Second thirdArgument fourthArgument"
      , "  , Third fifthArgument sixthArgument"
      ])
  , ("comments before the closing bracket",
      [ "  [ First firstArgument secondArgument"
      , "  , Second thirdArgument fourthArgument"
      , "  , Third fifthArgument sixthArgument"
      , "    -- [,] Closing note."
      ])
  , ("source-sensitive post-documentation comments",
      [ "  [ First firstArgument secondArgument"
      , "  , Second thirdArgument fourthArgument"
      , "  , Third fifthArgument sixthArgument"
      , "    -- ^ Source-sensitive pattern documentation."
      ])
  , ("comments after the opening bracket",
      [ "  [ -- [,] Opening note."
      , "    First firstArgument secondArgument"
      , "  , Second thirdArgument fourthArgument"
      , "  , Third fifthArgument sixthArgument"
      ])
  ]

barePatternContexts :: [(String, [String])]
barePatternContexts =
  [ ("a top-level list pattern binding",
      ["[First firstArgument secondArgument, Second thirdArgument fourthArgument] = value"])
  , ("a let list pattern binding",
      [ "example = let"
      , "  [First firstArgument secondArgument, Second thirdArgument fourthArgument] = value"
      , "  in done"
      ])
  , ("a do list-pattern bind statement",
      [ "example = do"
      , "  [First firstArgument secondArgument, Second thirdArgument fourthArgument] <- values"
      , "  done"
      ])
  ]

malformedLists :: [(String, [String])]
malformedLists =
  [ ("a missing closing pattern bracket", ["example [firstValue, secondValue = done"])
  , ("a missing pattern between commas", ["example [firstValue,, secondValue] = done"])
  ]

moduleSource :: [String] -> String
moduleSource declarations = unlines $ ["module ListPatternLayout where", ""] ++ declarations

