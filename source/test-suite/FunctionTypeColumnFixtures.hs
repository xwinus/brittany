module FunctionTypeColumnFixtures
  ( moduleSource
  , signatureSource
  , componentsAt
  , parserResult
  , tupleList
  , contextualSignatures
  , overwideComponent
  , nearBudgetComponent
  , nearBudgetSources
  , wrappedForallSources
  ) where

import qualified Data.List as List

moduleSource :: [String] -> String
moduleSource declarations = unlines $
  ["{-# LANGUAGE ExplicitForAll #-}", "module FunctionTypeColumns where", ""]
    ++ declarations

signatureSource :: Int -> Bool -> String -> String
signatureSource preceding intermediate component = moduleSource
  [ "f :: " ++ List.intercalate " -> "
      (replicate preceding "A" ++ [component] ++ ["Result" | intermediate])
  , "f = undefined"
  ]

parserResult :: String
parserResult = "io (Either String (Anns, GHC.ParsedSource, a, ParserContext))"

tupleList :: String
tupleList = "[([BriDocNumbered], BriDocNumbered, LHsExpr GhcPs, [SourceComment])]"

componentsAt :: Int -> [(String, String)]
componentsAt columns
  | columns == 40 =
      [ ("application", "io (Either Error (Value, State))")
      , ("list", "[(Input, Output, State)]")
      , ("tuple", "(First, Second, Third)")
      ]
  | otherwise =
      [ ("application", parserResult)
      , ("list", tupleList)
      , ("tuple", "(FirstArgument, SecondArgument, ThirdArgument, FourthArgument)")
      ]

contextualSignatures :: [(String, String, [String])]
contextualSignatures =
  [ ( "a qualified context and explicit forall"
    , "forall io a. (Show a, IO.MonadIO io) => " ++ prefix ++ parserResult
    , []
    )
  , ( "a nested callback"
    , prefix ++ "(Input -> " ++ parserResult ++ ") -> Result"
    , []
    )
  , ( "an ordinary comment between arrows"
    , prefix ++ "\n  -- Intermediate explanation.\n  " ++ parserResult ++ " -> Result"
    , ["-- Intermediate explanation."]
    )
  , ( "an intermediate Haddock post-doc"
    , prefix ++ parserResult ++ " -- ^ R.\n  -> Result"
    , ["-- ^ R."]
    )
  , ( "a standalone block comment between arrows"
    , "A\n  {- Parameter note. -}\n  -> " ++ prefix ++ parserResult
    , ["{- Parameter note. -}"]
    )
  ]
 where
  prefix = concat $ replicate 6 "A -> "

overwideComponent :: String
overwideComponent = "io (Either LongErrorType (FirstArgument, SecondArgument, ThirdArgument))"

nearBudgetComponent :: String
nearBudgetComponent = "IO (Either String (Annotations, GHC.ParsedSource, value, ParserContext))"

nearBudgetSources :: [(String, String, Bool)]
nearBudgetSources =
  [ ("an outer forall", source "ExplicitForAll" $ "forall a. A -> " ++ result, True)
  , ( "an outer forall and context"
    , source "ExplicitForAll, FlexibleContexts" $ "forall a. Constraint a => A -> " ++ result
    , True
    )
  , ( "a nested forall callback"
    , source "RankNTypes" $ "A -> (forall a. A -> " ++ result ++ ") -> B"
    , True
    )
  , ( "a nested qualified callback"
    , source "RankNTypes, FlexibleContexts" $ "A -> (Constraint a => A -> " ++ result ++ ") -> B"
    , True
    )
  , ( "kinded forall binders"
    , source "ExplicitForAll, KindSignatures" $
        "forall (a :: SomeExtremelyLongKindName) (b :: AnotherLongKindName). A -> " ++ result
    , False
    )
  ]
 where
  result = nearBudgetComponent
  source extensions signature = unlines
    [ "{-# LANGUAGE " ++ extensions ++ " #-}"
    , "module FunctionTypeColumns where"
    , "f :: " ++ signature
    , "f = undefined"
    ]

wrappedForallSources :: [(String, String)]
wrappedForallSources =
  [ ("a callback", source ["f :: (" ++ quantified ++ ") -> B", "f = undefined"])
  , ("a type synonym", source ["type Wrapped = " ++ quantified])
  ]
 where
  quantified = "forall (first :: SomeExtremelyLongKindName) "
    ++ "(second :: AnotherExtremelyLongKindName) "
    ++ "(third :: ThirdExtremelyLongKindName). A -> " ++ nearBudgetComponent
  source declarations = unlines $
    [ "{-# LANGUAGE RankNTypes, KindSignatures #-}"
    , "module FunctionTypeColumns where"
    ] ++ declarations
