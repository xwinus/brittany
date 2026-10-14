module DetachedPredicateFixtures where

moduleSource :: [String] -> String
moduleSource declarations = unlines $ "module DetachedPredicate where" : "" : declarations

predicateShapes :: [String]
predicateShapes =
  [ "right section", "left section", "arithmetic section", "composition"
  , "parenthesized arithmetic", "nested parentheses", "parenthesized application"
  ]

predicateOfSize :: String -> Int -> String
predicateOfSize shape target = build $ 'x' : replicate (max 0 $ target - length (build "x")) 'x'
 where
  build value = case shape of
    "right section" -> "(<= count " ++ value ++ ")"
    "left section" -> "(count " ++ value ++ " +)"
    "arithmetic section" -> "(+ count " ++ value ++ ")"
    "composition" -> "(predicate . select " ++ value ++ ")"
    "parenthesized arithmetic" -> "(count " ++ value ++ " + extra)"
    "nested parentheses" -> "((<= count " ++ value ++ "))"
    "parenthesized application" -> "(all (valid . select " ++ value ++ "))"
    _ -> error "unknown predicate fixture"

assertionSource :: Bool -> String -> String -> String
assertionSource chain operator predicate = moduleSource
  [ "example = do"
  , "  values " ++ (if chain then operator ++ " (<= 1) " else "")
      ++ operator ++ " " ++ predicate
  ]

maintainedCases :: [(FilePath, Int, String)]
maintainedCases =
  [ ( "source/test-suite/AnnotationIndexSpec.hs", 80
    , "(<= AnnotationIndex.indexNodeCount annotationIndex)"
    )
  , ( "source/test-suite/SiblingBindingAlignmentSpec.hs", 80
    , "(<= length (lines formatted) * alignmentPaddingLimit 30)"
    )
  , ( "source/test-suite/ExtractAnnsSpec.hs", 80
    , "((== 1) . length . List.elemIndices \"-- ^ conversion result\")"
    )
  , ( "source/test-suite/CompactInfixRhsSpec.hs", 100
    , "(any (List.isInfixOf (compact $ operator ++ \" \" ++ rhs) . compact) . lines)"
    )
  ]

commentCases :: [(String, [String], String)]
commentCases =
  [ ( "a leading line comment on the assertion"
    , ["  -- boundary", "  values `shouldSatisfy` (<= count selectedValues)"]
    , "-- boundary"
    )
  , ( "a leading block comment on the assertion"
    , ["  {- boundary -}", "  values `shouldSatisfy` (<= count selectedValues)"]
    , "{- boundary -}"
    )
  , ( "a comment inside the predicate"
    , [ "  values `shouldSatisfy` (<= count -- argument"
      , "    selectedValues)"
      ]
    , "-- argument"
    )
  , ( "a comment after the complete predicate"
    , ["  values `shouldSatisfy` (<= count selectedValues) -- complete"]
    , "-- complete"
    )
  ]

oversizedPredicates :: [String]
oversizedPredicates =
  [ "(<= buildComparison firstArgument secondArgument thirdArgument)"
  , "(firstPredicate . secondPredicate . thirdPredicate . fourthPredicate)"
  ]

unchangedOperandCases :: [(String, String)]
unchangedOperandCases =
  [ ("list", "[first, second, third]")
  , ("literal", "\"a compact literal\"")
  , ("lambda", "\\value -> firstAction value >> secondAction value")
  , ("do block", "do\n    firstAction\n    secondAction")
  ]

noncompactBlockSource :: String
noncompactBlockSource = moduleSource
  [ "result placement boundary ="
  , "  placementLineRelation placement == InlineComment"
  , "    && ((case placementOwner placement of"
  , "          NodeId (AnnKey _ constructor) -> unConName constructor `elem` [\"ConDeclH98\", \"ConDeclGADT\"]"
  , "        ) || case (commentBoundaryPath boundary, commentBoundaryGap boundary) of"
  , "          (ConstructorBoundaryPath{}, AfterLastBoundary) -> True"
  , "          _ -> False)"
  ]
