module TupleOpeningColumnFixtures where

moduleSource :: [String] -> String
moduleSource declarations = unlines $
  [ "{-# LANGUAGE TupleSections #-}", "{-# LANGUAGE UnboxedTuples #-}"
  , "module TupleOpeningColumn where", ""
  ] ++ declarations

standardSource :: String -> String -> String -> String
standardSource open close suffix = moduleSource
  [ "f =", "  " ++ open ++ " firstTransform firstArgument"
  , "  , secondTransform secondArgument", "  , thirdTransform thirdArgument"
  , "  " ++ close ++ suffix
  ]

primarySource :: String
primarySource = moduleSource
  [ "f sourceComment nodeSpan ="
  , "  ( srcSpanEndLine $ sourceCommentSpan sourceComment"
  , "  , srcSpanEndCol $ sourceCommentSpan sourceComment"
  , "  ) <= (srcSpanStartLine nodeSpan, srcSpanStartCol nodeSpan)"
  ]

edgeCases :: [(String, String, [(String, Int, Int)])]
edgeCases =
  [ ( "nested", moduleSource
      [ "f ="
      , "  ( ( firstTransform firstArgument"
      , "    , secondTransform secondArgument"
      , "    )"
      , "  , thirdTransform thirdArgument"
      , "  ) <= referenceValue"
      ]
    , [("firstTransform", 0, 2), ("firstTransform", 1, 2)]
    )
  , ( "unboxed", moduleSource
      [ "f ="
      , "  (# firstTransform firstArgument"
      , "  , secondTransform secondArgument"
      , "  , thirdTransform thirdArgument"
      , "  #) `combine` referenceValue"
      ]
    , [("firstTransform", 0, 3)]
    )
  , ( "first hole", moduleSource
      [ "f ="
      , "  ( , firstTransform firstArgument"
      , "  , secondTransform secondArgument"
      , "  ) `combine` referenceValue"
      ]
    , [("firstTransform", 0, 3)]
    )
  , ( "last hole", moduleSource
      [ "f ="
      , "  ( firstTransform firstArgument"
      , "  , secondTransform secondArgument"
      , "  , ) `combine` referenceValue"
      ]
    , [("firstTransform", 0, 3)]
    )
  , ( "inline comment", moduleSource
      [ "f ="
      , "  ( firstTransform firstArgument -- (,)"
      , "  , secondTransform secondArgument"
      , "  , thirdTransform thirdArgument"
      , "  ) <= referenceValue"
      ]
    , [("firstTransform", 0, 3)]
    )
  , ( "ownline comment", moduleSource
      [ "f ="
      , "  ( firstTransform firstArgument"
      , "    -- before (,) separator"
      , "  , secondTransform secondArgument"
      , "  , thirdTransform thirdArgument"
      , "  ) <= referenceValue"
      ]
    , [("firstTransform", 0, 3)]
    )
  , ( "block comment", moduleSource
      [ "f ="
      , "  ( firstTransform firstArgument"
      , "  , {- next (,) value -} secondTransform secondArgument"
      , "  , thirdTransform thirdArgument"
      , "  ) <= referenceValue"
      ]
    , [("firstTransform", 0, 3)]
    )
  ]

unboxedApplicationSource :: String -> String
unboxedApplicationSource suffix = moduleSource
  [ "f = (# longFirstFunction firstArgument secondArgument"
  , "    , longSecondFunction thirdArgument fourthArgument"
  , "    #)" ++ suffix
  ]
