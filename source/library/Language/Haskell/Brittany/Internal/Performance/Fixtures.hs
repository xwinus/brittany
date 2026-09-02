{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.Performance.Fixtures
  ( BenchmarkInput(..)
  , sourceFileInput
  , declarationScalingInput
  , nestingScalingInput
  , commentScalingInput
  , layoutAlternativeScalingInput
  , delimiterCountScalingInput
  , delimiterDepthScalingInput
  , declarationSizeScalingInput
  , malformedInput
  , malformedLayoutInput
  ) where

import qualified Data.Kind as Kind
import qualified Data.List as List
import Language.Haskell.Brittany.Internal.Prelude
import qualified System.IO as IO

type BenchmarkInput :: Kind.Type
data BenchmarkInput = BenchmarkInput
  { benchmarkInputName :: String
  , benchmarkInputOrigin :: String
  , benchmarkInputSource :: String
  , benchmarkInputDeclarationCount :: Maybe Int
  , benchmarkInputNestingDepth :: Maybe Int
  , benchmarkInputCommentCount :: Maybe Int
  , benchmarkInputAlternativeCount :: Maybe Int
  , benchmarkInputAlternativeDepth :: Maybe Int
  , benchmarkInputDelimiterGroupCount :: Maybe Int
  , benchmarkInputDeclarationSize :: Maybe Int
  }
  deriving (Eq, Show)

sourceFileInput :: String -> IO BenchmarkInput
sourceFileInput path = do
  benchmarkInputSource <- IO.readFile path
  pure BenchmarkInput
    { benchmarkInputName = path
    , benchmarkInputOrigin = "source-file"
    , benchmarkInputSource
    , benchmarkInputDeclarationCount = Nothing
    , benchmarkInputNestingDepth = Nothing
    , benchmarkInputCommentCount = Nothing
    , benchmarkInputAlternativeCount = Nothing
    , benchmarkInputAlternativeDepth = Nothing
    , benchmarkInputDelimiterGroupCount = Nothing
    , benchmarkInputDeclarationSize = Nothing
    }

declarationScalingInput :: Int -> BenchmarkInput
declarationScalingInput requestedCount = BenchmarkInput
  { benchmarkInputName = "generated-declarations-" ++ show count
  , benchmarkInputOrigin = "generated-declaration-scaling"
  , benchmarkInputSource = List.unlines
      $ ["module GeneratedDeclarations where", ""]
      ++ List.concatMap declaration [1 .. count]
  , benchmarkInputDeclarationCount = Just count
  , benchmarkInputNestingDepth = Nothing
  , benchmarkInputCommentCount = Just 0
  , benchmarkInputAlternativeCount = Just 0
  , benchmarkInputAlternativeDepth = Just 0
  , benchmarkInputDelimiterGroupCount = Just 0
  , benchmarkInputDeclarationSize = Just 1
  }
 where
  count = max 0 requestedCount
  declaration index =
    [ "value" ++ show index ++ " = " ++ show index
    , ""
    ]

nestingScalingInput :: Int -> BenchmarkInput
nestingScalingInput requestedDepth = BenchmarkInput
  { benchmarkInputName = "generated-nesting-" ++ show depth
  , benchmarkInputOrigin = "generated-nesting-scaling"
  , benchmarkInputSource = List.unlines
      [ "module GeneratedNesting where"
      , ""
      , "nested value ="
      , nestedExpression depth
      ]
  , benchmarkInputDeclarationCount = Just 1
  , benchmarkInputNestingDepth = Just depth
  , benchmarkInputCommentCount = Just 0
  , benchmarkInputAlternativeCount = Nothing
  , benchmarkInputAlternativeDepth = Nothing
  , benchmarkInputDelimiterGroupCount = Just 0
  , benchmarkInputDeclarationSize = Just 1
  }
 where
  depth = max 0 requestedDepth
  nestedExpression 0 = "  value"
  nestedExpression remaining =
    replicate (2 * (depth - remaining + 1)) ' '
      ++ "let value" ++ show remaining ++ " = value"
      ++ "\n"
      ++ replicate (2 * (depth - remaining + 1)) ' '
      ++ "in\n"
      ++ nestedExpression (remaining - 1)

commentScalingInput :: Int -> BenchmarkInput
commentScalingInput requestedCount = BenchmarkInput
  { benchmarkInputName = "generated-comments-" ++ show count
  , benchmarkInputOrigin = "generated-comment-scaling"
  , benchmarkInputSource = List.unlines
      $ ["module GeneratedComments where", ""]
      ++ ["-- benchmark comment " ++ show index | index <- [1 .. count]]
      ++ ["commented = 1", ""]
  , benchmarkInputDeclarationCount = Just 1
  , benchmarkInputNestingDepth = Nothing
  , benchmarkInputCommentCount = Just count
  , benchmarkInputAlternativeCount = Just 0
  , benchmarkInputAlternativeDepth = Just 0
  , benchmarkInputDelimiterGroupCount = Just 0
  , benchmarkInputDeclarationSize = Just 1
  }
 where
  count = max 0 requestedCount

layoutAlternativeScalingInput :: Int -> Int -> BenchmarkInput
layoutAlternativeScalingInput requestedCount requestedDepth = BenchmarkInput
  { benchmarkInputName = "generated-alternatives-" ++ show count
      ++ "-depth-" ++ show depth
  , benchmarkInputOrigin = "generated-layout-alternative-scaling"
  , benchmarkInputSource = "module GeneratedAlternatives where\n"
  , benchmarkInputDeclarationCount = Just 0
  , benchmarkInputNestingDepth = Nothing
  , benchmarkInputCommentCount = Just 0
  , benchmarkInputAlternativeCount = Just count
  , benchmarkInputAlternativeDepth = Just depth
  , benchmarkInputDelimiterGroupCount = Just 0
  , benchmarkInputDeclarationSize = Just 0
  }
 where
  count = max 0 requestedCount
  depth = max 0 requestedDepth

delimiterCountScalingInput :: Int -> BenchmarkInput
delimiterCountScalingInput requestedCount = BenchmarkInput
  { benchmarkInputName = "generated-delimiter-count-" ++ show count
  , benchmarkInputOrigin = "generated-delimiter-count-scaling"
  , benchmarkInputSource = List.unlines
      [ "module GeneratedDelimiterCount where"
      , ""
      , "delimited = [" ++ List.intercalate ", "
          ["[" ++ show index ++ "]" | index <- [1 .. count]] ++ "]"
      ]
  , benchmarkInputDeclarationCount = Just 1
  , benchmarkInputNestingDepth = Just 1
  , benchmarkInputCommentCount = Just 0
  , benchmarkInputAlternativeCount = Nothing
  , benchmarkInputAlternativeDepth = Nothing
  , benchmarkInputDelimiterGroupCount = Just count
  , benchmarkInputDeclarationSize = Just 1
  }
 where
  count = max 0 requestedCount

delimiterDepthScalingInput :: Int -> BenchmarkInput
delimiterDepthScalingInput requestedDepth = BenchmarkInput
  { benchmarkInputName = "generated-delimiter-depth-" ++ show depth
  , benchmarkInputOrigin = "generated-delimiter-depth-scaling"
  , benchmarkInputSource = List.unlines
      [ "module GeneratedDelimiters where"
      , ""
      , "delimited = " ++ replicate depth '[' ++ "0" ++ replicate depth ']'
      ]
  , benchmarkInputDeclarationCount = Just 1
  , benchmarkInputNestingDepth = Just depth
  , benchmarkInputCommentCount = Just 0
  , benchmarkInputAlternativeCount = Nothing
  , benchmarkInputAlternativeDepth = Nothing
  , benchmarkInputDelimiterGroupCount = Just 1
  , benchmarkInputDeclarationSize = Just 1
  }
 where
  depth = max 0 requestedDepth

declarationSizeScalingInput :: Int -> BenchmarkInput
declarationSizeScalingInput requestedSize = BenchmarkInput
  { benchmarkInputName = "generated-declaration-size-" ++ show size
  , benchmarkInputOrigin = "generated-declaration-size-scaling"
  , benchmarkInputSource = List.unlines
      [ "module GeneratedDeclarationSize where"
      , ""
      , "largeDeclaration = [" ++ List.intercalate ", "
          (show <$> [1 .. size]) ++ "]"
      ]
  , benchmarkInputDeclarationCount = Just 1
  , benchmarkInputNestingDepth = Nothing
  , benchmarkInputCommentCount = Just 0
  , benchmarkInputAlternativeCount = Nothing
  , benchmarkInputAlternativeDepth = Nothing
  , benchmarkInputDelimiterGroupCount = Just 1
  , benchmarkInputDeclarationSize = Just size
  }
 where
  size = max 0 requestedSize

malformedInput :: BenchmarkInput
malformedInput = BenchmarkInput
  { benchmarkInputName = "generated-malformed"
  , benchmarkInputOrigin = "generated-error-path"
  , benchmarkInputSource = List.unlines
      [ "module GeneratedMalformed where"
      , ""
      , "broken ="
      ]
  , benchmarkInputDeclarationCount = Just 1
  , benchmarkInputNestingDepth = Nothing
  , benchmarkInputCommentCount = Just 0
  , benchmarkInputAlternativeCount = Just 0
  , benchmarkInputAlternativeDepth = Just 0
  , benchmarkInputDelimiterGroupCount = Just 0
  , benchmarkInputDeclarationSize = Just 1
  }

malformedLayoutInput :: BenchmarkInput
malformedLayoutInput = BenchmarkInput
  { benchmarkInputName = "generated-malformed-layout"
  , benchmarkInputOrigin = "generated-layout-error-path"
  , benchmarkInputSource = "module GeneratedMalformedLayout where\n"
  , benchmarkInputDeclarationCount = Just 0
  , benchmarkInputNestingDepth = Just 0
  , benchmarkInputCommentCount = Just 0
  , benchmarkInputAlternativeCount = Just 0
  , benchmarkInputAlternativeDepth = Just 0
  , benchmarkInputDelimiterGroupCount = Just 0
  , benchmarkInputDeclarationSize = Just 0
  }
