{-# LANGUAGE LambdaCase #-}

module MembershipPredicateSpec (spec) where

import qualified Control.Exception as Exception
import Control.Monad (forM_)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import Data.Semigroup (Last(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import MembershipPredicateFixtures
import Language.Haskell.Brittany
  ( CConfig(..), CErrorHandlingConfig(..), CLayoutConfig(..), Config
  , parsePrintModule, staticDefaultConfig
  )
import Language.Haskell.Brittany.Internal.CommentPlan
  ( commentPlanFingerprint, normalizeCommentPlan )
import Language.Haskell.Brittany.Internal.Config.Types (IndentPolicy(..))
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SemanticFingerprint (compareSemanticSyntax)
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Timeout as Timeout
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "membership predicate grouping" $ do
  Hspec.it "keeps the complete CompatibilityMatrix membership and equality predicates cohesive" $ do
    source <- readFile $ projectRoot </> "source/test-suite/CompatibilityMatrix.hs"
    output <- checkedSource 80 2 IndentPolicyFree source
    forM_
      [ "name `elem` matrixCaseFeatures matrixCase"
      , "matrixCaseExpectedResult matrixCase == Formats"
      ] $ \unit -> assertCohesive unit output

  forM_ [40, 80, 100] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ policies $ \policy -> forM_ ["elem", "notElem"] $ \membership ->
      forM_ ["&&", "||"] $ \boolean ->
        Hspec.it ("groups " ++ membership ++ " predicates around " ++ boolean
          ++ layoutDescription columns indent policy) $ do
          let (member, comparison) = predicateUnits columns membership
          forM_ [[member, comparison], [comparison, member]] $ \units -> do
            output <- checkedSource columns indent policy $ moduleSource
              ["selected = " ++ List.intercalate (" " ++ boolean ++ " ") units]
            forM_ units $ \unit -> assertCohesive unit output
            assertWithinColumns columns output

  forM_ [2, 4] $ \indent -> forM_ policies $ \policy -> do
    let context = layoutDescription 80 indent policy
    forM_ contextualCases $ \(name, declarations, units) ->
      Hspec.it ("retains cohesive units in " ++ name ++ context) $ do
        output <- checkedSource 80 indent policy $ moduleSource declarations
        forM_ units $ \unit -> assertCohesive unit output
        assertWithinColumns 80 output
    forM_ ["`List.elem`", "`List.notElem`", "`contains`", "<~>", "+", "++", "."] $ \operator ->
      Hspec.it ("retains the conservative grouping barrier at " ++ operator ++ context) $ do
        let member = "name " ++ operator ++ " matrixCaseFeatures matrixCase"
            comparison = "matrixCaseExpectedResult matrixCase == Formats"
        output <- checkedSource 80 indent policy $ moduleSource
          ["selected = " ++ member ++ " && " ++ comparison]
        output `Hspec.shouldNotContain` member
        output `Hspec.shouldNotContain` comparison
        output `Hspec.shouldContain` operator
        assertWithinColumns 80 output
    forM_ shadowedCases $ \(name, declarations) ->
      Hspec.it ("preserves parsed semantics and token order for " ++ name ++ context) $ do
        output <- checkedSource 80 indent policy $ moduleSource declarations
        forM_ ["first `elem` allowed", "second `notElem` excluded", "status == Ready"] $
          \unit -> assertCohesive unit output
        assertWithinColumns 80 output
    forM_ [0, 1] $ \excess ->
      Hspec.it ("accounts for the boolean prefix at a membership width boundary plus " ++ show excess ++ context) $ do
        let prefix = "&& "
            suffix = " `notElem` values"
            subject = 'n' : replicate (80 - 2 * indent - length prefix - length suffix - 1 + excess) 'a'
            unit = subject ++ suffix
        output <- checkedSource 80 indent policy $ moduleSource
          ["selected = first == initial && " ++ unit ++ " && last == final"]
        if excess == 0
          then map length (filter (List.isInfixOf $ prefix ++ unit) $ lines output) `Hspec.shouldBe` [80]
          else pure ()
        assertWithinColumns 80 output
    Hspec.it ("wraps an individually over-width membership predicate" ++ context) $ do
      let oversized = "name `notElem` buildCollection firstArgument secondArgument thirdArgument"
      output <- checkedSource 40 indent policy $ moduleSource
        ["selected = " ++ oversized ++ " && status == Ready"]
      output `Hspec.shouldNotContain` oversized
      assertCohesive "status == Ready" output
      assertWithinColumns 40 output
    Hspec.it ("keeps fitting complete chains on one line" ++ context) $ do
      output <- checkedSource 80 indent policy $ moduleSource
        ["selected = x `elem` xs && y `notElem` ys", "other = x `notElem` xs || y == z"]
      output `Hspec.shouldContain` "selected = x `elem` xs && y `notElem` ys"
      output `Hspec.shouldContain` "other = x `notElem` xs || y == z"

  forM_ [2, 4] $ \indent -> forM_ commentCases $ \(name, declarations, comments) ->
    Hspec.it ("preserves " ++ name ++ " at indent " ++ show indent) $ do
      output <- checkedSource 40 indent IndentPolicyFree $ moduleSource declarations
      forM_ comments $ \comment ->
        length (filter (List.isInfixOf comment) $ lines output) `Hspec.shouldBe` 1
      forM_ ["(x `elem` xs)", "(y `notElem` ys)", "(z == Z)"] $ \unit -> assertCohesive unit output
      assertWithinColumns 40 output

  forM_ policies $ \policy -> forM_
    [ ( "a comparison before membership"
      , "matrixCaseExpectedResult matrixCase == Formats && name `elem` matrixCaseFeatures matrixCase"
      )
    , ( "multiple mixed boolean groups"
      , "name `elem` matrixCaseFeatures matrixCase && matrixCaseExpectedResult matrixCase == Formats"
        ++ " || name `notElem` excludedFeatures && state /= Invalid"
      )
    ] $ \(name, expression) ->
      Hspec.it ("does not introduce narrow nested overflow for " ++ name
        ++ layoutDescription 40 4 policy) $ do
        output <- checkedSource 40 4 policy $ moduleSource
          [ "outer = result"
          , " where"
          , "  successfulCaseFor name matrixCase ="
          , "    " ++ expression
          ]
        assertWithinColumns 40 output

  forM_ policies $ \policy ->
    Hspec.it ("does not widen the existing nested callback overflow"
      ++ layoutDescription 40 4 policy) $ do
      output <- checkedSource 40 4 policy $ moduleSource
        [ "outer = do"
        , "  when condition $ do"
        , "    case result of"
        , "      Right matrixCase -> pure (name `elem` matrixCaseFeatures matrixCase"
          ++ " && matrixCaseExpectedResult matrixCase == Formats)"
        , "      Left err -> fail err"
        ]
      let rows = lines output
          operatorRows = filter (List.isInfixOf "`elem`") rows
          argumentRows = filter ((== "matrixCase") . dropWhile (== ' ')) rows
      case operatorRows of
        [row] -> length (takeWhile (== ' ') row) `Hspec.shouldSatisfy` (<= 16)
        _ -> Hspec.expectationFailure "expected exactly one membership operator row"
      rows `Hspec.shouldSatisfy` all ((<= 43) . length)
      argumentRows `Hspec.shouldSatisfy` all ((<= 40) . length)

  Hspec.it "formats twenty membership groups within a bounded three-pass budget" $ do
    let units = ["value" ++ show index ++ " `elem` collection" ++ show index | index <- [1 :: Int .. 20]]
        source = moduleSource ["selected = " ++ List.intercalate " && " units]
    result <- Timeout.timeout 20000000 $ checkedSource 80 2 IndentPolicyFree source
    case result of
      Nothing -> Hspec.expectationFailure "twenty membership groups exceeded the 20-second budget"
      Just output -> do
        forM_ units $ \unit -> assertCohesive unit output
        assertWithinColumns 80 output

  forM_
    [ ("a missing membership operand", "selected = name `elem`")
    , ("an unfinished boolean continuation", "selected = name `notElem` values &&")
    ] $ \(name, declaration) ->
      Hspec.it ("rejects " ++ name ++ " without replacing inplace input") $ do
        let source = moduleSource [declaration]
        directory <- Directory.getTemporaryDirectory
        Exception.bracket
          (do
            (path, handle) <- IO.openTempFile directory "brittany-membership-invalid.hs"
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

assertCohesive :: String -> String -> IO ()
assertCohesive unit output =
  filter (List.isInfixOf unit) (lines output) `Hspec.shouldSatisfy` (not . null)

assertWithinColumns :: Int -> String -> IO ()
assertWithinColumns columns output = filter ((> columns) . length) (lines output) `Hspec.shouldBe` []

policies :: [IndentPolicy]
policies = [IndentPolicyLeft, IndentPolicyMultiple, IndentPolicyFree]

layoutDescription :: Int -> Int -> IndentPolicy -> String
layoutDescription columns indent policy = " at width " ++ show columns
  ++ ", indent " ++ show indent ++ ", " ++ show policy

checkedSource :: Int -> Int -> IndentPolicy -> String -> IO String
checkedSource columns indent policy original = do
  let config = configWithLayout columns indent policy
  first <- formatChecked config original
  (inputAnns, inputParsed, ()) <- parseSource original
  (outputAnns, outputParsed, ()) <- parseSource first
  compareSemanticSyntax inputParsed outputParsed `Hspec.shouldBe` Right Nothing
  case (normalizeCommentPlan inputAnns, normalizeCommentPlan outputAnns) of
    (Right inputPlan, Right outputPlan) ->
      commentPlanFingerprint outputPlan `Hspec.shouldBe` commentPlanFingerprint inputPlan
    _ -> Hspec.expectationFailure "input or output has an invalid comment plan"
  second <- formatChecked config first
  third <- formatChecked config second
  second `Hspec.shouldBe` first
  third `Hspec.shouldBe` first
  pure first
 where
  parseSource source = do
    parsed <- ParseModule.parseModule ["-haddock"] "MembershipPredicate.hs"
      (const $ pure $ Right ()) source
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError >> fail parseError
      Right result -> pure result

formatChecked :: Config -> String -> IO String
formatChecked config source = parsePrintModule config (Text.pack source) >>= \case
  Left errors -> Hspec.expectationFailure
    ("formatting returned " ++ show (length errors) ++ " errors") >> fail "formatting failed"
  Right output -> pure $ Text.unpack output

configWithLayout :: Int -> Int -> IndentPolicy -> Config
configWithLayout columns indent policy = staticDefaultConfig
  { _conf_layout = (_conf_layout staticDefaultConfig)
      { _lconfig_cols = Identity $ Last columns
      , _lconfig_indentAmount = Identity $ Last indent
      , _lconfig_indentPolicy = Identity $ Last policy
      }
  , _conf_errorHandling = (_conf_errorHandling staticDefaultConfig)
      { _econf_Werror = Identity $ Last True
      , _econf_failOnExactSourceFallback = Identity $ Last True
      }
  }
