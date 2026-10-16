{-# LANGUAGE LambdaCase #-}

module TrailingSignatureCommentSpec (spec) where

import TrailingSignatureCommentFixtures
import qualified Control.Exception as Exception
import Control.Monad (forM_)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import Data.Semigroup (Last(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified GHC
import Language.Haskell.Brittany
  ( CConfig(..), CErrorHandlingConfig(..), CLayoutConfig(..), Config
  , parsePrintModule, staticDefaultConfig
  )
import Language.Haskell.Brittany.Internal.CommentBoundary (canonicalCommentGraph)
import Language.Haskell.Brittany.Internal.CommentPlan
  ( commentPlanFingerprint, normalizeCommentPlan )
import Language.Haskell.Brittany.Internal.Config.Types (IndentPolicy(..))
import Language.Haskell.Brittany.Internal.ExactPrintCompat (Anns)
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SemanticFingerprint (compareSemanticSyntax)
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "trailing signature comments" $ do
  Hspec.it "keeps the reported first type and its comment after the separator" $ do
    output <- checkedSource 80 2 False IndentPolicyFree reportedSource
    row <- uniqueLine "-- span map:" output
    row `Hspec.shouldContain` ":: Map.Map AnnKey"
    assertOrdered ["-- span map:", "-- override keys", "-- input annotations"
      , "-- output with inner comments redistributed"] output
    assertWithin 80 output

  Hspec.it "keeps the maintained ExtractAnns separator with its fitting first type" $ do
    source <- readFile $ projectRoot
      </> "source/library/Language/Haskell/Brittany/Internal/ExtractAnns.hs"
    output <- checkedSource 80 2 False IndentPolicyFree source
    row <- uniqueLine "-- span map: key -> (start, end)" output
    row `Hspec.shouldContain` ":: Map.Map AnnKey"
    length row `Hspec.shouldSatisfy` (<= 80)

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ [False, True] $ \hanging -> forM_ policies $ \policy ->
      Hspec.it ("attaches a fitting first type with final line, block and Haddock comments"
        ++ describeLayout columns indent hanging policy) $ do
        forM_ finalComments $ \comment -> do
          output <- checkedSource columns indent hanging policy $ signatureSource "Input" comment
          assertFirstType "Input" output
          assertOrdered ["::", "Input", "Result", comment, "f = undefined"] output
          assertWithin columns output

  forM_ policies $ \policy -> forM_ [0, 1] $ \excess ->
    Hspec.it ("handles a first type at width forty plus " ++ show excess ++ ", " ++ show policy) $ do
      let firstType = 'T' : replicate (40 - 2 - 3 - 1 + excess) 'x'
      output <- checkedSource 40 2 False policy $ signatureSource firstType "-- final result"
      row <- uniqueLine firstType output
      if excess == 0
        then do
          row `Hspec.shouldContain` "::"
          length row `Hspec.shouldBe` 40
        else row `Hspec.shouldNotContain` "::"
      assertWithin 40 output

  forM_ policies $ \policy -> forM_ controlCases $ \(name, source, markers) ->
    Hspec.it ("preserves " ++ name ++ ", " ++ show policy) $ do
      output <- checkedSource 80 2 False policy source
      assertOrdered markers output
      assertWithin 80 output
      if name `elem` ["comments after internal type components", "an own-line comment between type components"]
        then assertFirstType "Input" output
        else pure ()

  forM_ policies $ \policy ->
    forM_ widthRegressionCases $ \(name, columns, indent, hanging, source) ->
      Hspec.it ("avoids added overflow with " ++ name
        ++ describeLayout columns indent hanging policy) $ do
        output <- checkedSource columns indent hanging policy source
        assertExecutableWithin columns output
        case name of
          "narrow hanging multiple names" -> do
            let overflow = filter ((> columns) . length) $ lines output
            overflow `Hspec.shouldSatisfy` all (List.isInfixOf "-- final result")
            map length overflow `Hspec.shouldSatisfy` all (<= 41)
          "protected final continuations" -> do
            seed <- uniqueLine "-- result root" output
            child <- uniqueLine "-- child" output
            grandchild <- uniqueLine "-- grandchild" output
            length seed `Hspec.shouldSatisfy` (<= columns)
            markerColumn "-- grandchild" grandchild - markerColumn "-- child" child
              `Hspec.shouldBe` 2
            assertOrdered ["-- result root", "-- child", "-- grandchild"] output
          _ -> assertWithin columns output

  forM_
    [ ["f :: Input -> -- incomplete result", "f = undefined"]
    , ["f :: (Input -> Result -- unclosed type", "f = undefined"]
    ] $ \declarations ->
      Hspec.it ("rejects malformed signatures without inplace replacement: " ++ show declarations) $ do
        let source = moduleSource declarations
        withSource source $ \path -> do
          Brittany.mainWith "brittany"
            [ "--no-user-config", "--write-mode", "inplace"
            , "--werror", "--fail-on-fallback", path
            ] `Hspec.shouldThrow` (== Exit.ExitFailure 60)
          TextIO.readFile path `Hspec.shouldReturn` Text.pack source

  forM_
    [ ("loss", ["-- first note"])
    , ("duplication", ["-- first note", "-- second note", "-- second note"])
    , ("reordering", ["-- second note", "-- first note"])
    ] $ \(name, comments) ->
      Hspec.it ("detects signature comment " ++ name) $ do
        let source notes = moduleSource $
              ["f :: Input -> Result"] ++ map ("  " ++) notes ++ ["f = undefined"]
        assertDifferentCommentGraphs (source ["-- first note", "-- second note"]) (source comments)

  Hspec.it "detects moving a final Haddock comment to an earlier type component" $ do
    let original = signatureSource "Input" "-- ^ result documentation"
        mutated = moduleSource
          [ "f :: Input -- ^ result documentation", "  -> Result", "f = undefined" ]
    assertHaddockReassociation original mutated

  Hspec.it "detects moving signature documentation to the next declaration" $ do
    let original = moduleSource
          [ "f :: Input -> Result -- ^ result documentation", "f = undefined"
          , "g :: Result", "g = undefined"
          ]
        mutated = moduleSource
          [ "f :: Input -> Result", "f = undefined"
          , "g :: Result -- ^ result documentation", "g = undefined"
          ]
    assertHaddockReassociation original mutated

  Hspec.it "detects a result type mutation despite unchanged final comments" $ do
    (_, before, ()) <- parsedSource $ signatureSource "Input" "-- final result"
    (_, after, ()) <- parsedSource $ moduleSource
      ["f :: Input -> DifferentResult -- final result", "f = undefined"]
    compareSemanticSyntax before after `Hspec.shouldNotBe` Right Nothing

assertFirstType :: String -> String -> IO ()
assertFirstType firstType output = do
  row <- uniqueLine "::" output
  row `Hspec.shouldContain` firstType

assertOrdered :: [String] -> String -> IO ()
assertOrdered [] _ = pure ()
assertOrdered (marker : remaining) source =
  case List.findIndex (List.isPrefixOf marker) $ List.tails source of
    Nothing -> Hspec.expectationFailure $ "missing or misplaced marker: " ++ marker
    Just offset -> assertOrdered remaining $ drop (offset + length marker) source

uniqueLine :: String -> String -> IO String
uniqueLine marker source = case filter (List.isInfixOf marker) $ lines source of
  [row] -> pure row
  found -> Hspec.expectationFailure ("expected one " ++ marker ++ " row: " ++ show found)
    >> fail "missing or repeated marker"

assertWithin :: Int -> String -> IO ()
assertWithin columns source = filter ((> columns) . length) (lines source) `Hspec.shouldBe` []

-- These fixtures contain no string literals; only comment text may exceed the limit.
assertExecutableWithin :: Int -> String -> IO ()
assertExecutableWithin columns output =
  filter ((> columns) . length) (map beforeLineComment $ lines output) `Hspec.shouldBe` []
 where
  beforeLineComment row = case List.findIndex (List.isPrefixOf "--") $ List.tails row of
    Nothing -> row
    Just offset -> take offset row

markerColumn :: String -> String -> Int
markerColumn marker row = length $ takeWhile (not . List.isPrefixOf marker) $ List.tails row

assertDifferentCommentGraphs :: String -> String -> IO ()
assertDifferentCommentGraphs original mutated = do
  before@(_, beforeParsed, ()) <- parsedSource original
  after@(_, afterParsed, ()) <- parsedSource mutated
  compareSemanticSyntax beforeParsed afterParsed `Hspec.shouldBe` Right Nothing
  assertChangedGraph before after

assertHaddockReassociation :: String -> String -> IO ()
assertHaddockReassociation original mutated = do
  before@(_, beforeParsed, ()) <- parsedSource original
  after@(_, afterParsed, ()) <- parsedSource mutated
  compareSemanticSyntax beforeParsed afterParsed `Hspec.shouldSatisfy` \case
    Right (Just _) -> True
    _ -> False
  assertChangedGraph before after

assertChangedGraph
  :: (Anns, GHC.ParsedSource, ()) -> (Anns, GHC.ParsedSource, ()) -> IO ()
assertChangedGraph (beforeAnns, beforeParsed, ()) (afterAnns, afterParsed, ()) =
  case (normalizeCommentPlan beforeAnns, normalizeCommentPlan afterAnns) of
    (Right before, Right after) ->
      canonicalCommentGraph afterParsed after
        `Hspec.shouldNotBe` canonicalCommentGraph beforeParsed before
    _ -> Hspec.expectationFailure "invalid plan in comment mutation control"

checkedSource :: Int -> Int -> Bool -> IndentPolicy -> String -> IO String
checkedSource columns indent hanging policy source = do
  let config = configWithLayout columns indent hanging policy
  first <- formatChecked config source
  (inputAnns, inputParsed, ()) <- parsedSource source
  (outputAnns, outputParsed, ()) <- parsedSource first
  compareSemanticSyntax inputParsed outputParsed `Hspec.shouldBe` Right Nothing
  case (normalizeCommentPlan inputAnns, normalizeCommentPlan outputAnns) of
    (Right inputPlan, Right outputPlan) -> do
      commentPlanFingerprint outputPlan `Hspec.shouldBe` commentPlanFingerprint inputPlan
      canonicalCommentGraph outputParsed outputPlan
        `Hspec.shouldBe` canonicalCommentGraph inputParsed inputPlan
    _ -> Hspec.expectationFailure "input or output has an invalid comment plan"
  second <- formatChecked config first
  third <- formatChecked config second
  second `Hspec.shouldBe` first
  third `Hspec.shouldBe` first
  pure first

parsedSource :: String -> IO (Anns, GHC.ParsedSource, ())
parsedSource source = do
  parsed <- ParseModule.parseModule ["-haddock"] "TrailingSignatureComments.hs"
    (const $ pure $ Right ()) source
  case parsed of
    Left failure -> Hspec.expectationFailure failure >> fail failure
    Right result -> pure result

formatChecked :: Config -> String -> IO String
formatChecked config source = parsePrintModule config (Text.pack source) >>= \case
  Left errors -> Hspec.expectationFailure
    ("formatting returned " ++ show (length errors) ++ " errors") >> fail "formatting failed"
  Right output -> pure $ Text.unpack output

configWithLayout :: Int -> Int -> Bool -> IndentPolicy -> Config
configWithLayout columns indent hanging policy = staticDefaultConfig
  { _conf_layout = (_conf_layout staticDefaultConfig)
      { _lconfig_cols = Identity $ Last columns
      , _lconfig_indentAmount = Identity $ Last indent
      , _lconfig_indentPolicy = Identity $ Last policy
      , _lconfig_hangingTypeSignature = Identity $ Last hanging
      }
  , _conf_errorHandling = (_conf_errorHandling staticDefaultConfig)
      { _econf_Werror = Identity $ Last True
      , _econf_failOnExactSourceFallback = Identity $ Last True
      }
  }

policies :: [IndentPolicy]
policies = [IndentPolicyLeft, IndentPolicyMultiple, IndentPolicyFree]

describeLayout :: Int -> Int -> Bool -> IndentPolicy -> String
describeLayout columns indent hanging policy =
  " at width " ++ show columns ++ ", indent " ++ show indent ++ ", hanging " ++ show hanging ++ ", " ++ show policy

withSource :: String -> (FilePath -> IO a) -> IO a
withSource source action = do
  directory <- Directory.getTemporaryDirectory
  Exception.bracket
    (do
      (path, handle) <- IO.openTempFile directory "brittany-trailing-signature-comment.hs"
      IO.hPutStr handle source
      IO.hClose handle
      pure path)
    Directory.removeFile
    action
