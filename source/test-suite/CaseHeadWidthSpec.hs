{-# LANGUAGE LambdaCase #-}

module CaseHeadWidthSpec (spec) where

import qualified Control.Exception as Exception
import Control.Monad (forM_)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import Data.Semigroup (Last(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Language.Haskell.Brittany
  ( CConfig(..)
  , CErrorHandlingConfig(..)
  , CLayoutConfig(..)
  , Config
  , parsePrintModule
  , staticDefaultConfig
  )
import Language.Haskell.Brittany.Internal.CommentPlan
  ( commentPlanFingerprint
  , normalizeCommentPlan
  )
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SemanticFingerprint
  ( compareSemanticSyntax
  )
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "complete case head width" $ do
  forM_ corpusCases $ \(description, relativePath, prefix) ->
    Hspec.it ("wraps " ++ description ++ " when formatting the complete module") $ do
      source <- readFile $ projectRoot </> relativePath
      let config = configWithLayout 80 2
      output <- formatChecked config source
      heads <- caseHeads prefix output
      filter ((> 80) . length) (concat heads) `Hspec.shouldBe` []
      assertStableAndEquivalent config source output

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ compoundPatterns $ \(description, patternSource) ->
      Hspec.it
        ("wraps " ++ description ++ " at width " ++ show columns
          ++ " and indent " ++ show indent) $ do
          _ <- checkWithinColumns columns indent $ caseSource patternSource "True"
          pure ()

  forM_ [2, 4] $ \indent -> forM_ compoundPatterns $ \(description, patternSource) ->
    Hspec.it ("wraps nested " ++ description ++ " at indent " ++ show indent) $ do
      _ <- checkWithinColumns 40 indent $ moduleSource
        [ "choose outerValue = case outerValue of"
        , "  Just value -> case value of"
        , "    " ++ patternSource ++ " -> True"
        , "    _ -> False"
        , "  Nothing -> False"
        ]
      pure ()

  Hspec.it "preserves a structural explicit pattern-synonym builder head" $ do
    _ <- checkWithinColumns 40 2 $ "{-# LANGUAGE PatternSynonyms #-}\n" ++ moduleSource
      [ "pattern Pair left right <- (left, right)"
      , "  where"
      , "    Pair (First first second) (Second third fourth) = (first, third)"
      ]
    pure ()

  Hspec.it "wraps a constructor head in lambda-case" $ do
    _ <- checkWithinColumns 40 2 $ "{-# LANGUAGE LambdaCase #-}\n" ++ moduleSource
      [ "choose = \\case"
      , "  Candidate firstArgument secondArgument thirdArgument -> True"
      , "  _ -> False"
      ]
    pure ()

  Hspec.it "keeps a where binding attached to a wrapped case alternative" $ do
    _ <- checkWithinColumns 40 2 $ moduleSource
      [ "choose value = case value of"
      , "  Candidate firstArgument secondArgument thirdArgument -> selected"
      , "    where"
      , "      selected = True"
      , "  _ -> False"
      ]
    pure ()

  Hspec.it "preserves a fitting tuple head and its multiline application body" $ do
    source <- readFile $ projectRoot </> "source/library/Language/Haskell/Brittany/Main.hs"
    let config = configWithLayout 80 2
    output <- formatChecked config source
    output `Hspec.shouldContain` unlines
      [ "        (Inplace, False, Just paths) -> runTransactionalInplace"
      , "          parserSession"
      , "          putStrErrLn"
      , "          config"
      , "          suppressOutput"
      , "          paths"
      ]
    assertStableAndEquivalent config source output

  Hspec.it "preserves the body indentation below a fitting commented head" $ do
    source <- readFile $ projectRoot </>
      "source/library/Language/Haskell/Brittany/Internal/Transformations/Par.hs"
    let config = configWithLayout 80 2
    output <- formatChecked config source
    output `Hspec.shouldContain` unlines
      [ "    x@(BDPar _ (BDPar _ BDPar{} _) _) ->"
      , "      x"
      ]
    assertStableAndEquivalent config source output

  Hspec.it "keeps a short pattern together without widening an unavoidable arrow comment" $ do
    let comment = "-- " ++ replicate 90 'x'
        source = moduleSource
          [ "choose value = case value of"
          , "  ColInfo ind _ list -> " ++ comment
          , "    list"
          , "  _ -> []"
          ]
        config = configWithLayout 80 2
    output <- formatChecked config source
    output `Hspec.shouldContain` "  ColInfo ind _ list\n    -> "
    maximum (map length $ lines output) `Hspec.shouldBe` (7 + length comment)
    assertStableAndEquivalent config source output

  forM_ compactPatterns $ \patternSource -> forM_ [False, True] $ \longRhs ->
    Hspec.it
      ("keeps a fitting " ++ patternSource ++ " head compact with "
        ++ (if longRhs then "an unavoidable long RHS" else "a short RHS")) $ do
        let result = if longRhs then "unavoidable" ++ replicate 80 'x' else "True"
            source = caseSource patternSource result
            config = configWithLayout 40 2
        output <- formatChecked config source
        output `Hspec.shouldContain` (patternSource ++ " ->")
        map (dropWhile (== ' ')) (filter ((> 40) . length) $ lines output)
          `Hspec.shouldBe` (if longRhs then [result] else [])
        assertStableAndEquivalent config source output

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ [False, True] $ \guarded -> forM_ [0, 1, 2] $ \remaining ->
      Hspec.it
        ("reserves arrow space after " ++ (if guarded then "a guarded" else "an ordinary")
          ++ " head at width " ++ show columns ++ " and indent " ++ show indent
          ++ " with " ++ show remaining ++ " columns remaining") $ do
          let guardSource = if guarded then " | True" else ""
              nameLength = columns - indent - remaining
                - length "Candidate " - length guardSource
              patternSource = "Candidate " ++ ('v' : replicate (nameLength - 1) 'a')
                ++ guardSource
          _ <- checkWithinColumns columns indent $ caseSource patternSource "True"
          pure ()

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ [0, 1, 2, 3] $ \remaining ->
      Hspec.it
        ("reserves arrow space after a tight Wrap operand at width " ++ show columns
          ++ " and indent " ++ show indent ++ " with " ++ show remaining
          ++ " columns remaining") $ do
          let nameLength = columns - indent - 5 - remaining
              patternSource = "Wrap " ++ ('v' : replicate (nameLength - 1) 'a')
          _ <- checkWithinColumns columns indent $ caseSource patternSource "True"
          pure ()

  forM_ commentedCases $ \(description, declarations) ->
    Hspec.it ("preserves " ++ description ++ " while wrapping case heads") $ do
      _ <- checkWithinColumns 80 2 $ moduleSource declarations
      pure ()

  forM_ malformedCases $ \(description, declarations) ->
    Hspec.it ("rejects " ++ description ++ " without changing inplace input") $ do
      let source = moduleSource declarations
      directory <- Directory.getTemporaryDirectory
      Exception.bracket
        (do
          (path, handle) <- IO.openTempFile directory "brittany-case-head-invalid.hs"
          IO.hPutStr handle source
          IO.hClose handle
          pure path)
        Directory.removeFile
        $ \path -> do
          Brittany.mainWith "brittany"
            [ "--no-user-config"
            , "--write-mode", "inplace"
            , "--werror"
            , "--fail-on-fallback"
            , path
            ] `Hspec.shouldThrow` (== Exit.ExitFailure 60)
          TextIO.readFile path `Hspec.shouldReturn` Text.pack source

caseHeads :: String -> String -> IO [[String]]
caseHeads prefix output = do
  let heads =
        [ beforeArrow ++ take 1 arrowAndBody
        | suffix@(first : _) <- List.tails $ lines output
        , prefix `List.isPrefixOf` dropWhile (== ' ') first
        , let (beforeArrow, arrowAndBody) = break (List.isInfixOf "->") suffix
        , not $ null arrowAndBody
        ]
  heads `Hspec.shouldSatisfy` (not . null)
  pure heads

checkWithinColumns :: Int -> Int -> String -> IO String
checkWithinColumns columns indent source = do
  let config = configWithLayout columns indent
  output <- formatChecked config source
  filter ((> columns) . length) (lines output) `Hspec.shouldBe` []
  assertStableAndEquivalent config source output
  pure output

formatChecked :: Config -> String -> IO String
formatChecked config source = parsePrintModule config (Text.pack source) >>= \case
  Left errors -> Hspec.expectationFailure
    ("formatting returned " ++ show (length errors) ++ " errors")
    >> fail "formatting failed"
  Right output -> pure $ Text.unpack output

assertStableAndEquivalent :: Config -> String -> String -> IO ()
assertStableAndEquivalent config original firstPass = do
  (inputAnns, inputParsed, ()) <- parseSource original
  (outputAnns, outputParsed, ()) <- parseSource firstPass
  compareSemanticSyntax inputParsed outputParsed `Hspec.shouldBe` Right Nothing
  case (normalizeCommentPlan inputAnns, normalizeCommentPlan outputAnns) of
    (Right inputPlan, Right outputPlan) ->
      commentPlanFingerprint outputPlan
        `Hspec.shouldBe` commentPlanFingerprint inputPlan
    _ -> Hspec.expectationFailure "input or output has an invalid comment plan"
  secondPass <- formatChecked config firstPass
  thirdPass <- formatChecked config secondPass
  secondPass `Hspec.shouldBe` firstPass
  thirdPass `Hspec.shouldBe` firstPass
 where
  parseSource source = do
    parsed <- ParseModule.parseModule ["-haddock"] "CaseHeadWidth.hs"
      (const $ pure $ Right ()) source
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError >> fail parseError
      Right result -> pure result

configWithLayout :: Int -> Int -> Config
configWithLayout columns indent = staticDefaultConfig
  { _conf_layout = (_conf_layout staticDefaultConfig)
      { _lconfig_cols = Identity $ Last columns
      , _lconfig_indentAmount = Identity $ Last indent
      }
  , _conf_errorHandling = (_conf_errorHandling staticDefaultConfig)
      { _econf_Werror = Identity $ Last True
      , _econf_failOnExactSourceFallback = Identity $ Last True
      }
  }

moduleSource :: [String] -> String
moduleSource declarations = unlines $ ["module CaseHeadWidth where", ""] ++ declarations

caseSource :: String -> String -> String
caseSource patternSource result = moduleSource
  [ "choose value = case value of"
  , "  " ++ patternSource ++ " -> " ++ result
  , "  _ -> False"
  ]

corpusCases :: [(String, FilePath, String)]
corpusCases =
  [ ("the ImportDecl record pattern",
      "source/library/Language/Haskell/Brittany/Internal/Layouters/Module.hs", "ImportDecl")
  , ("the benchmark worker cons pattern", "benchmark/Main.hs", "\"--worker\"")
  , ("the HsModule constructor patterns",
      "source/library/Language/Haskell/Brittany/Internal/Layouters/Module.hs", "HsModule")
  , ("the long ConPat constructor patterns",
      "source/library/Language/Haskell/Brittany/Internal/Layouters/Pattern.hs", "ConPat")
  ]

compoundPatterns :: [(String, String)]
compoundPatterns =
  [ ("record patterns", "Record { firstField = firstValue, secondField = secondValue }")
  , ("cons patterns",
      "\"--worker\" : scenario : \"--root\" : root : \"--config\" : config : []")
  , ("constructor patterns",
      "Candidate firstArgument secondArgument thirdArgument fourthArgument")
  ]

compactPatterns :: [String]
compactPatterns = ["Record { field = selected }", "first : remaining", "Just selected"]

commentedCases :: [(String, [String])]
commentedCases =
  [ ("a line comment before a record pattern",
      [ "choose value = case value of"
      , "  -- record pattern note"
      , "  Record { firstRecordField = firstValue, secondRecordField = secondValue } -> True"
      , "  _ -> False"
      ])
  , ("a line comment within a cons pattern",
      [ "choose value = case value of"
      , "  \"--worker\" : scenario -- scenario note"
      , "    : \"--root\" : root : \"--config\" : config : [] -> True"
      , "  _ -> False"
      ])
  , ("a block comment before a constructor pattern",
      [ "choose value = case value of"
      , "  {- constructor pattern note -}"
      , "  Candidate firstSelectedArgument secondArgument thirdArgument fourthArgument -> True"
      , "  _ -> False"
      ])
  , ("a line comment immediately before the arrow",
      [ "choose value = case value of"
      , "  Candidate firstArgument secondArgument thirdArgument -- pattern note"
      , "    -> True"
      , "  _ -> False"
      ])
  ]

malformedCases :: [(String, [String])]
malformedCases =
  [ ("a case alternative missing its arrow",
      ["choose value = case value of", "  Just selected True"])
  , ("a case alternative missing its RHS",
      ["choose value = case value of", "  Just selected ->"])
  ]
