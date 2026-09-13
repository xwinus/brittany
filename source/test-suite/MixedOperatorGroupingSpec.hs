{-# LANGUAGE LambdaCase #-}

module MixedOperatorGroupingSpec (spec) where

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
import qualified System.Timeout as Timeout
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "mixed operator grouping" $ do
  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent -> do
    forM_ expressionCases $ \(description, declarations, units) ->
      Hspec.it (description ++ " at width " ++ show columns ++ " and indent " ++ show indent) $ do
        output <- checkedSource columns indent $ moduleSource declarations
        forM_ units $ \unit -> assertCohesive unit output
    forM_ commentCases $ \(description, declarations, comments) ->
      Hspec.it ("preserves " ++ description ++ " at width " ++ show columns
        ++ " and indent " ++ show indent) $ do
        output <- checkedSource columns indent $ moduleSource declarations
        forM_ comments $ \comment -> assertUniqueComment comment output
        forM_ ["(a == A)", "(b == B)", "(c /= C)"] $ \unit ->
          assertCohesive unit output
    Hspec.it ("keeps fitting chains compact at width " ++ show columns
      ++ " and indent " ++ show indent) $ do
      output <- checkedSource columns indent $ moduleSource
        ["result = x == y && z /= w", "go v = Pair <$> v .: \"x\" <*> v .: \"y\""]
      output `Hspec.shouldContain` "result = x == y && z /= w"
      output `Hspec.shouldContain` "go v = Pair <$> v .: \"x\" <*> v .: \"y\""

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ [False, True] $ \lookupGroup -> forM_ [0, 1] $ \excess ->
      Hspec.it ((if lookupGroup then "lookup" else "comparison")
        ++ " group includes its leading separator at width " ++ show columns
        ++ ", indent " ++ show indent ++ " and excess " ++ show excess) $ do
        let (declaration, prefix, unit) = boundaryCase columns indent lookupGroup excess
        output <- checkedSource columns indent $ moduleSource [declaration]
        if excess == 0
          then do
            let completeRows = filter (List.isInfixOf $ prefix ++ unit) $ lines output
            map length completeRows `Hspec.shouldBe` [columns]
          else pure ()

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ ["string", "integer", "parenthesized string"] $ \kind ->
      Hspec.it ("does not add indentation to an indivisible " ++ kind
        ++ " at width " ++ show columns ++ " and indent " ++ show indent) $ do
        let token = if kind == "integer"
              then replicate (columns + 12) '7'
              else show $ replicate (columns + 12) 'x'
            expression = if kind == "parenthesized string" then "(" ++ token ++ ")" else token
            source = moduleSource ["result = actual == " ++ expression ++ " && flag == expectedFlag"]
            config = configWithLayout columns indent
            baselineIndent = if indent == 2 then 6 else 8
        output <- formatChecked config source
        assertStableAndEquivalent config source output
        case filter (List.isInfixOf token) $ lines output of
          [literalLine] ->
            length (takeWhile (== ' ') literalLine) `Hspec.shouldSatisfy` (<= baselineIndent)
          _ -> Hspec.expectationFailure "the complete literal must appear exactly once"
        filter (\line -> length line > columns && not (token `List.isInfixOf` line))
          (lines output) `Hspec.shouldBe` []
        assertCohesive "flag == expectedFlag" output

  Hspec.it "formats twenty lookup groups within a bounded time for three strict passes" $ do
    let units = ["value .: " ++ show ("key" ++ show index) | index <- [1 :: Int .. 20]]
        source = moduleSource
          ["parse value = Config <$> " ++ List.intercalate " <*> " units]
    result <- Timeout.timeout 20000000 $ checkedSource 80 2 source
    case result of
      Nothing -> Hspec.expectationFailure "twenty lookup groups exceeded the 20-second regression budget"
      Just output -> forM_ units $ \unit -> assertCohesive unit output

  Hspec.it "keeps the single-group comparisons together in the complete Alignment module" $ do
    output <- checkedModule projectRoot
      "source/library/Language/Haskell/Brittany/Internal/Alignment.hs"
    let target = unlines $ takeWhile (not . List.isInfixOf "bestPlans =")
          $ dropWhile (not . List.isInfixOf "singleGroupIsOptimal =") $ lines output
    forM_ [ "unitCount == 1"
          , "alignmentMaximumPadding wholeCost <= paddingLimit"
          , "alignmentTotalOverflow wholeCost == 0"
          ] $ \unit -> assertCohesive unit target

  Hspec.it "keeps applicative lookups together in the complete CompatibilityMatrix module" $ do
    output <- checkedModule projectRoot "source/test-suite/CompatibilityMatrix.hs"
    forM_ [ "object .: \"tracking-issue\""
          , "object .: \"expected-result\""
          , "object .:? \"skip\" .!= False"
          , "object .: \"schema-version\""
          ] $ \unit -> assertCohesive unit output

  Hspec.it "keeps optional applicative lookups together in the complete Performance Report module" $ do
    output <- checkedModule projectRoot
      "source/library/Language/Haskell/Brittany/Internal/Performance/Report.hs"
    forM_ [ "value .:? \"declarations\""
          , "value .:? \"nestingDepth\""
          , "value .:? \"alternativeDepth\""
          , "value .:? \"declarationSize\""
          ] $ \unit -> assertCohesive unit output

  forM_ malformedExpressions $ \(description, declarations) ->
    Hspec.it ("rejects " ++ description ++ " without replacing inplace input") $ do
      let source = moduleSource declarations
      directory <- Directory.getTemporaryDirectory
      Exception.bracket
        (do
          (path, handle) <- IO.openTempFile directory "brittany-mixed-operator-invalid.hs"
          IO.hPutStr handle source
          IO.hClose handle
          pure path)
        Directory.removeFile
        $ \path -> do
          Brittany.mainWith "brittany"
            [ "--no-user-config", "--write-mode", "inplace"
            , "--werror", "--fail-on-fallback", path
            ] `Hspec.shouldThrow` (== Exit.ExitFailure 60)
          TextIO.readFile path `Hspec.shouldReturn` Text.pack source

checkedSource :: Int -> Int -> String -> IO String
checkedSource columns indent source = do
  let config = configWithLayout columns indent
  output <- formatChecked config source
  filter ((> columns) . length) (lines output) `Hspec.shouldBe` []
  assertStableAndEquivalent config source output
  pure output

checkedModule :: FilePath -> FilePath -> IO String
checkedModule projectRoot path = do
  source <- readFile $ projectRoot </> path
  let config = configWithLayout 80 2
  output <- formatChecked config source
  assertStableAndEquivalent config source output
  pure output

assertCohesive :: String -> String -> IO ()
assertCohesive unit output =
  filter (List.isInfixOf unit) (lines output) `Hspec.shouldSatisfy` (not . null)

assertUniqueComment :: String -> String -> IO ()
assertUniqueComment comment output =
  length (filter (List.isInfixOf comment) $ lines output) `Hspec.shouldBe` 1

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
    parsed <- ParseModule.parseModule ["-haddock"] "MixedOperatorGrouping.hs"
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
moduleSource declarations = unlines $ ["module MixedOperatorGrouping where", ""] ++ declarations

boundaryCase :: Int -> Int -> Bool -> Int -> (String, String, String)
boundaryCase columns indent lookupGroup excess
  | lookupGroup =
      let key = replicate (columns - 2 * indent - length "<*> value .: \"\"" + excess) 'k'
          unit = "value .: " ++ show key
      in ( "parse value = Config <$> value .: \"first\" <*> " ++ unit ++ " <*> value .: \"last\""
         , "<*> "
         , unit
         )
  | otherwise =
      let name = 'p' : replicate (columns - 2 * indent - length "&&  == expectedValue" - 1 + excess) 'a'
          unit = name ++ " == expectedValue"
      in ( "example = firstValue == initialValue && " ++ unit ++ " && lastValue == finalValue"
         , "&& "
         , unit
         )

expressionCases :: [(String, [String], [String])]
expressionCases =
  [ ("comparison", ["example value = kind value == LineComment && size value == expectedSize && status value /= Disabled"], ["kind value == LineComment", "size value == expectedSize", "status value /= Disabled"])
  , ("applicative", ["parse value = SomeConstructor <$> value .: \"one\" <*> value .: \"two\" <*> value .: \"three\" <*> value .: \"four\""], ["value .: \"one\"", "value .: \"two\"", "value .: \"three\"", "value .: \"four\""])
  , ("optional", ["parse value = Config <$> value .:? \"one\" .!= 0 <*> value .:? \"two\" .!= 1 <*> value .:? \"three\" .!= 2"], ["value .:? \"one\" .!= 0", "value .:? \"two\" .!= 1", "value .:? \"three\" .!= 2"])
  , ("homogeneous", ["example = firstSelectedValue && secondSelectedValue && thirdSelectedValue && fourthSelectedValue"], [])
  , ("custom", ["infixl 7 %%%", "example = (firstValue %%% secondValue) && (thirdValue %%% fourthValue)", "unknown = firstValue <+> secondValue <~> thirdValue <+> fourthValue"], ["(firstValue %%% secondValue)", "(thirdValue %%% fourthValue)"])
  , ("parenthesized", ["example value = (kind value == LineComment) && (size value == expectedSize) && (status value /= Disabled)"], ["(kind value == LineComment)", "(size value == expectedSize)", "(status value /= Disabled)"])
  , ("oversized", ["example value = buildSelectedResult firstArgument secondArgument thirdArgument == expectedValue && kind value == LineComment"], ["kind value == LineComment"])
  , ("nested", ["example value = do", "  pure $ kind value == LineComment && size value == wantedSize && status value /= Disabled"], ["kind value == LineComment", "size value == wantedSize", "status value /= Disabled"])
  ]

commentCases :: [(String, [String], [String])]
commentCases =
  [ ("inline-line", ["example = (a == A) -- comparison note", "  && (b == B) && (c /= C)"], ["-- comparison note"])
  , ("own-line-block", ["example = (a == A) &&", "  {- conjunction note -}", "  (b == B) && (c /= C)"], ["{- conjunction note -}"])
  , ("own-line", ["example = (a == A) &&", "  -- conjunction note", "  (b == B) && (c /= C)"], ["-- conjunction note"])
  , ("multiline-block", ["example = (a == A) &&", "  {- conjunction note", "     continuation note -}", "  (b == B) && (c /= C)"], ["{- conjunction note", "continuation note -}"])
  ]

malformedExpressions :: [(String, [String])]
malformedExpressions =
  [ ("a missing comparison operand", ["example = left == right && other =="])
  , ("an unclosed applicative operand", ["example = Constructor <$> value .: (\"key\""])
  ]
