{-# LANGUAGE LambdaCase #-}

module DoLetCommentRunSpec (spec) where

import qualified Control.Exception as Exception
import Control.Monad (forM_)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import Data.Semigroup (Last(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import DoLetCommentRunFixtures
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
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "do-let comment runs" $ do
  Hspec.it "keeps the complete Decl explanation above its local binding" $ do
    source <- readFile $ projectRoot
      </> "source/library/Language/Haskell/Brittany/Internal/Layouters/Decl.hs"
    output <- checkedSource 80 2 IndentPolicyFree source
    let comments =
          [ "-- GHC 9.14: annotations lack AnnOpenP/AnnBackquote, so lrdrNameToTextAnn"
          , "-- returns the raw name. Parenthesize operators in prefix position; add"
          , "-- backticks for alphanumeric names in infix position."
          ]
    let section = unlines $ dropWhile (not . List.isInfixOf (head comments)) $ lines output
    binding <- uniqueLine "isSymOcc (rdrNameOcc" section
    forM_ comments $ \marker -> do
      line <- uniqueLine marker output
      dropWhile (== ' ') line `Hspec.shouldBe` marker
      indentation line `Hspec.shouldBe` indentation binding
      length line `Hspec.shouldSatisfy` (<= 80)

  Hspec.it "keeps both complete Alt inline-seeded runs coherent" $ do
    source <- readFile $ projectRoot
      </> "source/library/Language/Haskell/Brittany/Internal/Transformations/Alt.hs"
    output <- checkedSource 80 2 IndentPolicyFree source
    forM_
      [ [ "-- this is like List.nub, with one difference: if two elements"
        , "-- are unequal only in _vs_paragraph, with both ParAlways, we"
        , "-- treat them like equals and replace the first occurence with the"
        , "-- smallest member of this \"equal group\"."
        ]
      , [ "-- the standard function used to enforce a constant upper bound"
        , "-- on the number of elements returned for each node. Should be"
        , "-- applied whenever in a parent the combination of spacings from"
        , "-- its children might cause excess of the upper bound."
        ]
      ] $ \comments -> assertAlignedRun comments output

  forM_ [40, 80, 100] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ policies $ \policy -> do
      let context = layoutDescription columns indent policy
      forM_ [False, True] $ \inline ->
        Hspec.it ("preserves a " ++ (if inline then "coherent inline-seeded" else "binding-aligned own-line")
          ++ " run" ++ context) $ do
          output <- checkedSource columns indent policy $ runSource inline False
          assertAlignedRun ordinaryComments output
          assertBeforeBinding ordinaryComments "x" output
          if inline then pure () else assertOwnLineRun ordinaryComments "x" output
          assertWithinColumns columns output
      Hspec.it ("preserves a blank boundary between own-line runs" ++ context) $ do
        output <- checkedSource columns indent policy $ runSource False True
        assertOwnLineRun ordinaryComments "x" output
        assertBlankBetween (head ordinaryComments) (last ordinaryComments) output
        assertWithinColumns columns output
      Hspec.it ("keeps earlier statement annotations from claiming local comments" ++ context) $ do
        output <- checkedSource columns indent policy precedingStatementSource
        let comments = ["-- Comment about local binding.", "-- Continuing binding explanation."]
        assertOwnLineRun comments "x" output
        assertBeforeBinding comments "x" output
        prior <- uniqueLine "-- Comment about prior statement." output
        prior `Hspec.shouldSatisfy` List.isInfixOf "prepare"
        assertBeforeBinding
          [ "-- Comment about prior statement.", "-- Comment about the let statement."
          , "-- Comment about local binding.", "-- Continuing binding explanation."
          ] "x" output

  forM_ [2, 4] $ \indent -> forM_ policies $ \policy -> do
    let context = layoutDescription 80 indent policy
    Hspec.it ("preserves a blank boundary after an inline seed" ++ context) $ do
      output <- checkedSource 80 indent policy $ runSource True True
      assertBlankBetween (head ordinaryComments) (last ordinaryComments) output
      assertBeforeBinding ordinaryComments "x" output
    Hspec.it ("preserves the blank gap after a complete run" ++ context) $ do
      output <- checkedSource 80 indent policy $ moduleSource
        [ "f = do", "  let", "    -- First explanation.", "    -- Continuing explanation."
        , "", "    x = 1", "  pure x"
        ]
      assertOwnLineRun ordinaryComments "x" output
      assertBlankBetween (last ordinaryComments) "x = 1" output
    Hspec.it ("preserves relative columns in an inline-seeded example" ++ context) $ do
      let comments = ["-- Example:", "-- > x + 1", "-- > pure x"]
      output <- checkedSource 80 indent policy $ moduleSource
        [ "f = do", "  let -- Example:", "        -- > x + 1", "        -- > pure x"
        , "    x = 1", "  pure x"
        ]
      rows <- mapM (`uniqueLine` output) comments
      let columns = zipWith markerColumn comments rows
      map (subtract $ head columns) columns `Hspec.shouldBe` [0, 2, 2]
      assertBeforeBinding comments "x" output
    forM_ bindingContexts $ \(name, declarations, variable) ->
      Hspec.it ("retains comment ownership with " ++ name ++ context) $ do
        output <- checkedSource 80 indent policy $ moduleSource declarations
        assertOwnLineRun ordinaryComments variable output
        assertBeforeBinding ordinaryComments variable output
    forM_ protectedRuns $ \(name, comments) ->
      Hspec.it ("preserves " ++ name ++ context) $ do
        let sourceLines = ["f = do", "  let"]
              ++ map (\line -> if null line then "" else "    " ++ line) comments
              ++ ["    x = 1", "  pure x"]
            expected = filter (not . null) comments
        output <- checkedSource 80 indent policy $ moduleSource sourceLines
        let actual = filter (List.isPrefixOf "--" . dropWhile (== ' ')) $ lines output
        map (dropWhile (== ' ')) actual `Hspec.shouldBe` map (dropWhile (== ' ')) expected
        relativeColumns actual `Hspec.shouldBe` relativeColumns expected
        if name == "separated protected runs"
          then do
            assertBlankBetween "-- First prose." "-- > example" output
            assertBlankBetween "-- > example" "-- Last prose." output
          else pure ()
    forM_ blockSources $ \(name, declarations) ->
      Hspec.it ("preserves the exact text of " ++ name ++ context) $ do
        output <- checkedSource 80 indent policy $ moduleSource declarations
        _ <- uniqueLine "{- First line." output
        _ <- uniqueLine "Body note." output
        assertBeforeBinding ["{- First line.", "Body note."] "x" output
    forM_ controlSources $ \(name, declarations) ->
      Hspec.it ("retains the established coherent " ++ name ++ " control" ++ context) $ do
        output <- checkedSource 80 indent policy $ moduleSource declarations
        assertAlignedRun ordinaryComments output

  forM_
    [ ("a missing local binding RHS", ["f = do", "  let", "    -- Explanation.", "    x =", "  pure x"])
    , ("a malformed local signature", ["f = do", "  let", "    -- Explanation.", "    x ::", "    x = 1", "  pure x"])
    ] $ \(name, declarations) ->
      Hspec.it ("rejects " ++ name ++ " without replacing inplace input") $ do
        let source = moduleSource declarations
        withSource source $ \path -> do
          Brittany.mainWith "brittany" (inplaceArguments ++ [path])
            `Hspec.shouldThrow` (== Exit.ExitFailure 60)
          TextIO.readFile path `Hspec.shouldReturn` Text.pack source

  Hspec.it "keeps every inplace input unchanged when comment validation rejects a sibling module" $ do
    let valid = runSource False False
    withSource valid $ \validPath -> withSource failingCommentSource $ \invalidPath -> do
      Brittany.mainWith "brittany" (inplaceArguments ++ [validPath, invalidPath])
        `Hspec.shouldThrow` (== Exit.ExitFailure 1)
      TextIO.readFile validPath `Hspec.shouldReturn` Text.pack valid
      TextIO.readFile invalidPath `Hspec.shouldReturn` Text.pack failingCommentSource

assertOwnLineRun :: [String] -> String -> String -> IO ()
assertOwnLineRun comments variable output = do
  binding <- bindingLine variable output
  forM_ comments $ \marker -> do
    line <- uniqueLine marker output
    dropWhile (== ' ') line `Hspec.shouldBe` marker
    indentation line `Hspec.shouldBe` indentation binding

assertAlignedRun :: [String] -> String -> IO ()
assertAlignedRun comments output = do
  rows <- mapM (`uniqueLine` output) comments
  let columns = zipWith markerColumn comments rows
  columns `Hspec.shouldSatisfy` (not . null)
  columns `Hspec.shouldSatisfy` all (== head columns)

assertBeforeBinding :: [String] -> String -> String -> IO ()
assertBeforeBinding comments variable output = do
  binding <- bindingLine variable output
  let preceding = takeWhile (/= binding) $ lines output
  forM_ comments $ \marker -> preceding `Hspec.shouldSatisfy` any (List.isInfixOf marker)

assertBlankBetween :: String -> String -> String -> IO ()
assertBlankBetween first second output = do
  let between = takeWhile (not . List.isInfixOf second)
        $ drop 1 $ dropWhile (not . List.isInfixOf first) $ lines output
  between `Hspec.shouldSatisfy` any (null . words)

bindingLine :: String -> String -> IO String
bindingLine variable output = case filter matches $ lines output of
  [line] -> pure line
  found -> Hspec.expectationFailure ("expected one binding for " ++ variable ++ ", found " ++ show found)
    >> fail "missing or repeated binding"
 where
  matches line = case words line of
    name : "=" : _ -> name == variable
    _ -> False

uniqueLine :: String -> String -> IO String
uniqueLine marker output = case filter (List.isInfixOf marker) $ lines output of
  [line] -> pure line
  found -> Hspec.expectationFailure ("expected one " ++ marker ++ " line, found " ++ show found)
    >> fail "missing or repeated marker"

indentation :: String -> Int
indentation = length . takeWhile (== ' ')

relativeColumns :: [String] -> [Int]
relativeColumns [] = []
relativeColumns rows@(first : _) = map ((subtract $ indentation first) . indentation) rows

markerColumn :: String -> String -> Int
markerColumn marker line = length $ takeWhile (not . List.isPrefixOf marker) $ List.tails line

assertWithinColumns :: Int -> String -> IO ()
assertWithinColumns columns output = filter ((> columns) . length) (lines output) `Hspec.shouldBe` []

inplaceArguments :: [String]
inplaceArguments = ["--no-user-config", "--write-mode", "inplace", "--werror", "--fail-on-fallback"]

withSource :: String -> (FilePath -> IO a) -> IO a
withSource source action = do
  directory <- Directory.getTemporaryDirectory
  Exception.bracket
    (do
      (path, handle) <- IO.openTempFile directory "brittany-do-let-comments.hs"
      IO.hPutStr handle source
      IO.hClose handle
      pure path)
    Directory.removeFile
    action

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
    parsed <- ParseModule.parseModule ["-haddock"] "DoLetCommentRuns.hs"
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
