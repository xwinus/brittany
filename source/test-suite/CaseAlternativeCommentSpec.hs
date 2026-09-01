{-# LANGUAGE LambdaCase #-}

module CaseAlternativeCommentSpec (spec) where

import           Control.Monad                            ( forM_ )
import           Data.Functor.Identity                    ( Identity(..) )
import qualified Data.List                               as List
import           Data.Semigroup                           ( Last(..) )
import qualified Data.Text                               as Text
import qualified GHC
import           Language.Haskell.Brittany                ( CConfig(..)
                                                          , CErrorHandlingConfig(..)
                                                          , CLayoutConfig(..)
                                                          , Config
                                                          , parsePrintModule
                                                          , staticDefaultConfig
                                                          )
import           Language.Haskell.Brittany.Internal.CommentBoundary
                                                          ( canonicalCommentGraph
                                                          )
import           Language.Haskell.Brittany.Internal.CommentPlan
                                                          ( commentPlanFingerprint
                                                          , normalizeCommentPlan
                                                          )
import           Language.Haskell.Brittany.Internal.ExactPrintCompat
                                                          ( Anns )
import qualified Language.Haskell.Brittany.Internal.ParseModule
                                                         as ParseModule
import           Language.Haskell.Brittany.Internal.SemanticFingerprint
                                                          ( compareSemanticSyntax
                                                          )
import           Language.Haskell.Brittany.Internal.SourceComment.Types
                                                          ( CanonicalComment(..)
                                                          , CommentBoundaryGap(..)
                                                          , CommentBoundaryId(..)
                                                          , CommentBoundaryPath(..)
                                                          , CommentPlan
                                                          , CommentRole
                                                          , SourceCommentSyntax
                                                          )
import qualified Language.Haskell.Brittany.Main          as Brittany
import qualified System.Directory                        as Directory
import qualified System.Exit                             as Exit
import qualified System.FilePath                         as FilePath
import qualified Test.Hspec                              as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "case alternative comments" $ do
  Hspec.it "keeps a comment after case-of and before the first alternative" $ do
    firstPass <- formatSource defaultConfig primaryInput
    firstPass `Hspec.shouldBe` primaryExpected
    assertStableAndEquivalent defaultConfig primaryInput firstPass

  Hspec.it "preserves edge-case comment runs at configured indents" $ do
    forM_ [2, 4] $ \indentAmount -> do
      let config = configWithIndent indentAmount
      firstPass <- formatSource config edgeInput
      forM_ commentAlternativePairs $ \(commentNeedle, alternativeNeedle) -> do
        commentIndent     <- indentationOf commentNeedle firstPass
        alternativeIndent <- indentationOf alternativeNeedle firstPass
        commentIndent `Hspec.shouldBe` alternativeIndent
      firstPass `Hspec.shouldContain` "{- second case note -}\n\n"
      assertCaseBoundaries firstPass
      assertStableAndEquivalent config edgeInput firstPass

  Hspec.it "leaves malformed case input byte-identical in inplace mode" $ do
    let fixture = fixturePath projectRoot invalidFixture
        output  = outputPath projectRoot invalidFixture
    expected <- readFile fixture
    Directory.copyFile fixture output
    Brittany.mainWith "brittany" (formatterArgs projectRoot output)
      `Hspec.shouldThrow` (== Exit.ExitFailure 60)
    readFile output `Hspec.shouldReturn` expected

assertStableAndEquivalent :: Config -> String -> String -> IO ()
assertStableAndEquivalent config original firstPass = do
  originalParsed <- parseSource "CaseInput.hs" original
  firstParsed    <- parseSource "CaseOutput.hs" firstPass
  compareSemanticSyntax originalParsed firstParsed
    `Hspec.shouldBe` Right Nothing
  originalFingerprint <- commentFingerprint "CaseInput.hs" original
  firstFingerprint    <- commentFingerprint "CaseOutput.hs" firstPass
  firstFingerprint `Hspec.shouldBe` originalFingerprint
  secondPass <- formatSource config firstPass
  thirdPass  <- formatSource config secondPass
  secondPass `Hspec.shouldBe` firstPass
  thirdPass `Hspec.shouldBe` firstPass

assertCaseBoundaries :: String -> IO ()
assertCaseBoundaries source = do
  (annotations, parsedSource) <- parseWithAnnotations "CaseBoundaries.hs" source
  plan <- normalizedPlan annotations
  let caseComments =
        [ canonicalCommentBoundary comment
        | comment <- canonicalCommentGraph parsedSource plan
        , canonicalCommentText comment `elem` (Text.pack <$> boundaryComments)
        ]
  caseComments `Hspec.shouldSatisfy` \boundaries ->
    length boundaries
      == length boundaryComments
      && all isCaseAlternativeBoundary boundaries
 where
  isCaseAlternativeBoundary = \case
    CommentBoundaryId (CaseAlternativeBoundaryPath _) BeforeBoundary -> True
    _ -> False

formatSource :: Config -> String -> IO String
formatSource config input = parsePrintModule config (Text.pack input) >>= \case
  Left errors ->
    Hspec.expectationFailure
        ("formatting returned " ++ show (length errors) ++ " errors")
      >> fail "formatting failed"
  Right output -> pure $ Text.unpack output

parseSource :: FilePath -> String -> IO GHC.ParsedSource
parseSource filename source = snd <$> parseWithAnnotations filename source

parseWithAnnotations :: FilePath -> String -> IO (Anns, GHC.ParsedSource)
parseWithAnnotations filename source = do
  parsed <- ParseModule.parseModule ["-haddock"]
                                    filename
                                    (const $ pure $ Right ())
                                    source
  case parsed of
    Left parseError -> Hspec.expectationFailure parseError >> fail parseError
    Right (annotations, parsedSource, ()) -> pure (annotations, parsedSource)

commentFingerprint
  :: FilePath
  -> String
  -> IO [(Text.Text, SourceCommentSyntax, CommentRole, String)]
commentFingerprint filename source = do
  (annotations, _) <- parseWithAnnotations filename source
  plan             <- normalizedPlan annotations
  pure $ commentPlanFingerprint plan

normalizedPlan :: Anns -> IO CommentPlan
normalizedPlan annotations = case normalizeCommentPlan annotations of
  Left  errors -> Hspec.expectationFailure (show errors) >> fail "invalid plan"
  Right plan   -> pure plan

indentationOf :: String -> String -> IO Int
indentationOf needle source =
  case List.find (List.isInfixOf needle) $ lines source of
    Nothing -> Hspec.expectationFailure ("missing line containing " ++ needle)
      >> fail "missing formatted line"
    Just line -> pure $ length $ takeWhile (== ' ') line

defaultConfig :: Config
defaultConfig =
  staticDefaultConfig
    { _conf_errorHandling =
        (_conf_errorHandling staticDefaultConfig)
          { _econf_failOnExactSourceFallback = Identity $ Last True
          }
    }

configWithIndent :: Int -> Config
configWithIndent indentAmount =
  defaultConfig
    { _conf_layout =
        (_conf_layout defaultConfig)
          { _lconfig_indentAmount = Identity $ Last indentAmount
          }
    }

fixturePath :: FilePath -> FilePath -> FilePath
fixturePath projectRoot fixtureName = FilePath.combine
  (FilePath.combine projectRoot "source/test-suite/fixtures")
  fixtureName

outputPath :: FilePath -> FilePath -> FilePath
outputPath projectRoot fixtureName =
  FilePath.combine (FilePath.combine projectRoot "output") fixtureName

formatterArgs :: FilePath -> FilePath -> [String]
formatterArgs projectRoot input =
  [ "--config-file"
  , FilePath.combine projectRoot "data/brittany.yaml"
  , "--no-user-config"
  , "--write-mode"
  , "inplace"
  , input
  ]

invalidFixture :: FilePath
invalidFixture = "CaseAlternativeCommentInvalid.hs"

boundaryComments :: [String]
boundaryComments =
  [ "-- first case note"
  , "{- second case note -}"
  , "-- inner case note"
  , "-- outer case note"
  , "-- multiline scrutinee note"
  , "-- self-hosted case note"
  ]

commentAlternativePairs :: [(String, String)]
commentAlternativePairs =
  [ ("-- first case note", "Positive positiveValue")
  , ("{- second case note -}", "Positive positiveValue")
  , ("-- inner case note", "Inner innerValue")
  , ("-- outer case note", "Zero ->")
  , ("-- multiline scrutinee note", "Selected selectedValue")
  , ("-- self-hosted case note", "EmptyLocalBinds _")
  ]

primaryInput :: String
primaryInput = unlines
  [ "module CaseComment where"
  , ""
  , "example binds = case binds of"
  , "  -- Keep this comment before the first alternative."
  , "  Just value -> value"
  , "  Nothing -> 0"
  ]

primaryExpected :: String
primaryExpected = List.intercalate
  "\n"
  [ "module CaseComment where"
  , ""
  , "example binds = case binds of"
  , "  -- Keep this comment before the first alternative."
  , "  Just value -> value"
  , "  Nothing -> 0"
  ]

edgeInput :: String
edgeInput = unlines
  [ "module CaseAlternativeCorpus where"
  , ""
  , "multiple input = case input of"
  , "  -- first case note"
  , "  {- second case note -}"
  , ""
  , "  Positive positiveValue | positiveValue > 0 -> positiveValue"
  , "  _ -> 0"
  , ""
  , "nested outer = case (case outer of"
  , "  -- inner case note"
  , "  Inner innerValue -> innerValue"
  , "  NoInner -> 0) of"
  , "  -- outer case note"
  , "  Zero -> False"
  , "  NonZero -> True"
  , ""
  , "multiline left right = case"
  , "  chooseValue"
  , "    left"
  , "    right"
  , "  of"
  , "  -- multiline scrutinee note"
  , "  Selected selectedValue -> selectedValue"
  , "  NotSelected -> 0"
  , ""
  , "layoutLocalBindsWithComments annotations binds = case binds of"
  , "  -- self-hosted case note"
  , "  EmptyLocalBinds _ -> pure []"
  , "  _ -> layoutLocalBinds annotations binds"
  ]
