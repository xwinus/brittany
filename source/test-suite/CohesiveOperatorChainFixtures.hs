module CohesiveOperatorChainFixtures where

import qualified Data.List as List

moduleSource :: [String] -> String
moduleSource declarations = unlines $ "module CohesiveChain where" : "" : declarations

cohesiveCases :: [(String, [String], [String])]
cohesiveCases =
  [ ( "arithmetic after a low-precedence operator"
    , ["result = applySettings selectedConfiguration $ " ++ arithmetic]
    , [arithmetic]
    )
  , ( "comparisons before a multiline case"
    , [ "result = first == First && second /= Second && case value of"
      , "  Just item -> allowed item"
      , "  Nothing -> False"
      ]
    , ["first == First", "second /= Second"]
    )
  , ( "applicative operands within alternatives"
    , ["result = " ++ List.intercalate " <|> " alternatives]
    , alternatives
    )
  , ( "local lookup operators within an applicative chain"
    , ["result = Config <$> " ++ List.intercalate " <*> " lookups]
    , lookups
    )
  ]
 where
  arithmetic = "80 - 2 * n - len op - 1 + k"
  alternatives = [name ++ " <$> input" ++ show n | (name, n) <- zip ["first", "next", "third", "fourth", "fifth", "sixth"] [1 :: Int ..]]
  lookups = ["v " ++ op ++ " key" ++ show n | (op, n) <- zip [".:?", ".:?=", ".:?=", ".:?", ".:?=", ".:?"] [1 :: Int ..]]

booleanSource :: Int -> String
booleanSource columns = moduleSource
  [ "result = " ++ first ++ " || " ++ second
    ++ " || (not $ null types) && all allowed types"
  ]
 where
  first = 'a' : replicate (columns `div` 2) 'x'
  second = 'b' : replicate (columns `div` 2) 'y'

commentCases :: [(String, [String], [String])]
commentCases =
  [ ( "an inline comment between parenthesized predicates"
    , [ "result = (a == A) -- first predicate"
      , "  && (b /= B) && (c == C)"
      ]
    , ["-- first predicate"]
    )
  , ( "a leading comment on a later predicate"
    , [ "result = (a == A)"
      , "  -- later predicate"
      , "  && (b /= B) && (c == C)"
      ]
    , ["-- later predicate"]
    )
  , ( "a trailing comment on the last group"
    , [ "result = (a == A) && (b /= B)"
      , "  && (c == C) -- final"
      ]
    , ["-- final"]
    )
  ]

shadowedCases :: [(String, [String])]
shadowedCases =
  [ ( "a locally defined arithmetic operator with unusual fixity"
    , [ "infixr 1 +"
      , "left + right = combine left right"
      , "result = transform selectedConfiguration $ first + second * third + fourth"
      ]
    )
  , ( "a local operator resembling a lookup"
    , [ "infixl 1 .:?="
      , "left .:?= right = combine left right"
      , "result = Config <$> value .:?= firstKey <*> value .:?= secondKey"
        ++ " <*> value .:? thirdKey"
      ]
    )
  ]

maintainedCases :: [(String, FilePath, [String])]
maintainedCases =
  [ ( "arithmetic in CompactInfixRhsSpec"
    , "source/test-suite/CompactInfixRhsSpec.hs"
    , ["80 - 2 * indent - length operator - 1 + extra"]
    )
  , ( "Boolean comparisons before the case in SourceComment.Indentation"
    , "source/library/Language/Haskell/Brittany/Internal/SourceComment/Indentation.hs"
    , [ "placementLineRelation placement == CommentOwnLine"
      , "placementAnchor placement == BeforeNode"
      , "placementRole placement == LeadingOrdinary"
      ]
    )
  , ( "mixed Boolean groups in SemanticFingerprint.Ghc"
    , "source/library/Language/Haskell/Brittany/Internal/SemanticFingerprint/Ghc.hs"
    , ["all ignoredRepresentation typeArguments"]
    )
  , ( "applicative alternatives in ExtractAnns"
    , "source/library/Language/Haskell/Brittany/Internal/ExtractAnns.hs"
    , [ "(\\e -> (entry e, comments e)) <$> fromDynamic @(EpAnn AnnContext) dyn"
      , "(\\e -> (entry e, comments e)) <$> fromDynamic @(EpAnn AnnListItem) dyn"
      , "(\\e -> (entry e, comments e)) <$> fromDynamic @(EpAnn NoEpAnns) dyn"
      , "(\\e -> (entry e, comments e)) <$> fromDynamic @(EpAnn (AnnList ())) dyn"
      , "(\\e -> (entry e, comments e)) <$> fromDynamic @(EpAnn ()) dyn"
      , "(\\e -> (entry e, comments e)) <$> fromDynamic @(EpAnn [()]) dyn"
      ]
    )
  , ( "local lookup groups in Config.Types.Instances"
    , "source/library/Language/Haskell/Brittany/Internal/Config/Types/Instances.hs"
    , [ "v .:? Key.fromString \"conf_version\""
      , "v .:?= Key.fromString \"conf_debug\""
      , "v .:?= Key.fromString \"conf_layout\""
      , "v .:?= Key.fromString \"conf_errorHandling\""
      , "v .:?= Key.fromString \"conf_forward\""
      , "v .:?= Key.fromString \"conf_preprocessor\""
      , "v .:? Key.fromString \"conf_roundtrip_exactprint_only\""
      , "v .:? Key.fromString \"conf_disable_formatting\""
      , "v .:? Key.fromString \"conf_obfuscate\""
      ]
    )
  ]
