{-# LANGUAGE LambdaCase #-}

module CommentBoundaryGraphSpec (spec) where

import qualified Data.Text                               as Text
import           Language.Haskell.Brittany                ( parsePrintModule
                                                          , staticDefaultConfig
                                                          )
import           Language.Haskell.Brittany.Internal.CommentBoundary
                                                          ( canonicalCommentGraph
                                                          )
import           Language.Haskell.Brittany.Internal.CommentPlan
                                                          ( normalizeCommentPlan
                                                          )
import qualified Language.Haskell.Brittany.Internal.ParseModule
                                                         as ParseModule
import           Language.Haskell.Brittany.Internal.SourceComment.Types
                                                          ( CanonicalComment(..)
                                                          , CommentBoundaryGap(..)
                                                          , CommentBoundaryId(..)
                                                          , CommentBoundaryPath(..)
                                                          )
import qualified Test.Hspec                              as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "canonical comment boundary graph" $ do
  Hspec.it "keeps an ASCII caret inside its declaration boundary run" $ do
    firstPass <- formatChecked asciiHaddockSource
    commentLines firstPass `Hspec.shouldBe` commentLines asciiHaddockSource
    assertThreePasses "AsciiHaddock.hs" asciiHaddockSource firstPass

  Hspec.it "preserves identical comments as distinct ordered entries" $ do
    firstPass <- formatChecked duplicateSource
    graph     <- parsedGraph "DuplicateComments.hs" firstPass
    filter ((== Text.pack "-- same boundary text") . canonicalCommentText) graph
      `Hspec.shouldSatisfy` ((== 2) . length)
    assertThreePasses "DuplicateComments.hs" duplicateSource firstPass

  Hspec.it "attaches a terminal constructor post-doc without deriving" $ do
    firstPass <- formatChecked terminalPostDocSource
    firstPass
      `Hspec.shouldContain` "  | Final String\n    -- ^ final constructor"
    assertThreePasses "TerminalPostDoc.hs" terminalPostDocSource firstPass

  Hspec.it "keeps delimiter edge comments on explicit boundary gaps" $ do
    firstPass <- formatChecked delimiterBoundarySource
    graph <- parsedGraph "DelimiterBoundary.hs" firstPass
    let delimiterGaps =
          [ gap
          | comment <- graph
          , canonicalCommentText comment `elem`
              (Text.pack <$> ["-- after open", "-- before close"])
          , CommentBoundaryId (DelimiterBoundaryPath _) gap <-
              [canonicalCommentBoundary comment]
          ]
    delimiterGaps `Hspec.shouldBe` [AfterOpenBoundary, BeforeCloseBoundary]
    assertThreePasses "DelimiterBoundary.hs" delimiterBoundarySource firstPass

  Hspec.it "does not attach a trailing comment to the preceding delimiter" $ do
    graph <- parsedGraph "TrailingDelimiterComment.hs" trailingCommentSource
    let trailingBoundaries =
          [ canonicalCommentBoundary comment
          | comment <- graph
          , canonicalCommentText comment == Text.pack "-- trailing"
          ]
    trailingBoundaries
      `Hspec.shouldBe` [CommentBoundaryId (DeclarationBoundaryPath 0) WithinBoundary]

  Hspec.it "rejects malformed input without inventing a boundary" $ do
    parsePrintModule staticDefaultConfig (Text.pack malformedSource) >>= \case
      Left _ -> pure ()
      Right output ->
        Hspec.expectationFailure
          $  "malformed input formatted as "
          ++ Text.unpack output
    malformedSource `Hspec.shouldBe` malformedSource

assertThreePasses :: FilePath -> String -> String -> Hspec.Expectation
assertThreePasses filename original firstPass = do
  secondPass <- formatChecked firstPass
  thirdPass  <- formatChecked secondPass
  secondPass `Hspec.shouldBe` firstPass
  thirdPass `Hspec.shouldBe` firstPass
  originalGraph <- parsedGraph filename original
  firstGraph    <- parsedGraph filename firstPass
  secondGraph   <- parsedGraph filename secondPass
  firstGraph `Hspec.shouldBe` originalGraph
  secondGraph `Hspec.shouldBe` originalGraph

formatChecked :: String -> IO String
formatChecked source =
  parsePrintModule staticDefaultConfig (Text.pack source) >>= \case
    Left errors ->
      Hspec.expectationFailure
          ("formatting returned " ++ show (length errors) ++ " errors")
        >> fail "format failed"
    Right output -> pure $ Text.unpack output

parsedGraph :: FilePath -> String -> IO [CanonicalComment]
parsedGraph filename source = do
  parsed <- ParseModule.parseModule ["-haddock"]
                                    filename
                                    (const $ pure $ Right ())
                                    source
  case parsed of
    Left parseError -> Hspec.expectationFailure parseError >> fail parseError
    Right (annotations, parsedSource, ()) ->
      case normalizeCommentPlan annotations of
        Left errors ->
          Hspec.expectationFailure (show errors) >> fail "invalid plan"
        Right plan -> pure $ canonicalCommentGraph parsedSource plan

commentLines :: String -> [Text.Text]
commentLines =
  filter (Text.isPrefixOf (Text.pack "--") . Text.stripStart)
    . Text.lines
    . Text.pack

asciiHaddockSource :: String
asciiHaddockSource = unlines
  [ "module AsciiHaddock where"
  , ""
  , "previous = 0"
  , ""
  , "-- | Function docs."
  , "-- > case input of"
  , "--        ^^^^^^^^^^^ ASCII pointer"
  , "-- Ordinary prose after the pointer."
  , "select :: Int -> Int"
  , "select value = value"
  ]

duplicateSource :: String
duplicateSource = unlines
  [ "module DuplicateComments where"
  , ""
  , "first = 1"
  , "-- same boundary text"
  , "second = 2"
  , "-- same boundary text"
  , "third = 3"
  ]

terminalPostDocSource :: String
terminalPostDocSource = unlines
  [ "module TerminalPostDoc where"
  , ""
  , "data Example"
  , "  = First String"
  , "    -- ^ first constructor"
  , "  | Final String"
  , "    -- ^ final constructor"
  , ""
  , "following = 1"
  ]

delimiterBoundarySource :: String
delimiterBoundarySource = unlines
  [ "module DelimiterBoundary where"
  , ""
  , "values ="
  , "  [ -- after open"
  , "    first"
  , "  , second"
  , "    -- before close"
  , "  ]"
  ]

trailingCommentSource :: String
trailingCommentSource = unlines
  [ "module TrailingDelimiterComment where"
  , ""
  , "render values = case values of"
  , "  [] -> output (pack \"\") -- trailing"
  , "  _ -> output values"
  ]

malformedSource :: String
malformedSource = unlines
  [ "module MalformedBoundary where"
  , "value = do -- retained on failure"
  , "  let ="
  ]
