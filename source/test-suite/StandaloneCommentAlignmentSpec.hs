{-# LANGUAGE LambdaCase #-}

module StandaloneCommentAlignmentSpec (spec) where

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
spec projectRoot = Hspec.describe "standalone comment alignment" $ do
  Hspec.it "aligns the complete DataDecl module's explanation with its case branch" $ do
    source <- readFile $ projectRoot
      </> "source/library/Language/Haskell/Brittany/Internal/Layouters/DataDecl.hs"
    output <- checkedSource 2 source
    branch <- uniqueLine "DataTypeCons _ []" output
    forM_ ["-- data MyData a b", "-- (zero constructors)"] $ \marker -> do
      comment <- uniqueLine marker output
      indentation comment `Hspec.shouldBe` indentation branch

  Hspec.it "aligns the complete Alt module's TODO with its record field" $ do
    source <- readFile $ projectRoot
      </> "source/library/Language/Haskell/Brittany/Internal/Transformations/Alt.hs"
    output <- checkedSource 2 source
    assertFollowingAlignment
      "-- TODO: i am not sure this is valid, in general." ", _acp_indent" output

  forM_ [40, 80, 100] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ [False, True] $ \nested -> do
      Hspec.it ("aligns standalone case explanations at width " ++ show columns
        ++ " and indent " ++ show indent
        ++ if nested then " in a nested case" else " in a case") $ do
        output <- checkedSourceAt columns indent $ moduleSource $ caseComments nested
        assertFollowingAlignment "-- branch explanation" "Just selected" output
        if nested
          then assertFollowingAlignment "-- inner explanation" "First" output
          else pure ()
      Hspec.it ("aligns a standalone record comment at width " ++ show columns
        ++ " and indent " ++ show indent
        ++ if nested then " in a nested update" else " in an update") $ do
        output <- checkedSourceAt columns indent $ moduleSource $ recordComments nested
        assertFollowingAlignment "-- second field explanation" ", secondField" output
        assertRecordOpeningAlignment "firstField" "-- second field explanation" output

  forM_ [40, 80, 100] $ \columns -> forM_ [2, 4] $ \indent -> do
    forM_ [False, True] $ \recordValue ->
      Hspec.it ("aligns comments in a nested record " ++ (if recordValue then "value" else "update")
        ++ " at width " ++ show columns ++ " and indent " ++ show indent) $ do
        output <- checkedSourceAt columns indent $ moduleSource
          [ "update source = " ++ if recordValue then "Outer" else "source"
          , "  { outerField = " ++ if recordValue then "Inner" else "source"
          , "      { firstField = 0"
          , "          -- second field explanation"
          , "      , secondField = 1"
          , "      }"
          , "  }"
          ]
        assertFollowingAlignment "-- second field explanation" ", secondField" output
        assertRecordOpeningAlignment "firstField" "-- second field explanation" output
    Hspec.it ("aligns later case-alternative comments at width " ++ show columns
      ++ " and indent " ++ show indent) $ do
      output <- checkedSourceAt columns indent $ moduleSource
        [ "choose value = case value of"
        , "  First -> False"
        , "  -- later explanation"
        , "  Second -> case value of"
        , "    First -> False"
        , "  -- nested later explanation"
        , "    Second -> True"
        ]
      assertFollowingAlignment "-- later explanation" "Second" output
      assertFollowingAlignment "-- nested later explanation" "Second" output
    Hspec.it ("aligns a standalone record block comment at width " ++ show columns
      ++ " and indent " ++ show indent) $ do
      output <- checkedSourceAt columns indent $ moduleSource
        [ "update source = source"
        , "  { firstField = 0"
        , "      {- second field explanation -}"
        , "  , secondField = 1"
        , "  }"
        ]
      assertFollowingAlignment "{- second field explanation -}" ", secondField" output
      assertRecordOpeningAlignment "firstField" "{- second field explanation -}" output
    Hspec.it ("retains a fitting uncommented record at width " ++ show columns
      ++ " and indent " ++ show indent) $ do
      output <- checkedSourceAt columns indent $ moduleSource
        ["value = Record { field = 1 }"]
      output `Hspec.shouldContain` "value = Record { field = 1 }"

  forM_ [2, 4] $ \indent -> do
    Hspec.it ("aligns deeper ordinary prose with its later case alternative at indent " ++ show indent) $ do
      output <- checkedSource indent $ moduleSource
        [ "choose value = case value of"
        , "  First -> True"
        , "        -- deeper branch explanation"
        , "  Second -> False"
        ]
      assertFollowingAlignment "-- deeper branch explanation" "Second" output
    Hspec.it ("preserves physical columns in a later case ASCII block at indent " ++ show indent) $ do
      let comments =
            [ "  -- branch diagram"
            , "    --   left -> right"
            , "  --        ^ pointer"
            ]
      output <- checkedSource indent $ moduleSource $
        ["choose value = case value of", "  First -> False"] ++ comments
          ++ ["  Second -> True"]
      sourceCommentLines output `Hspec.shouldBe` comments
    Hspec.it ("preserves the baseline record ASCII columns at indent " ++ show indent) $ do
      let comments =
            [ "-- +----+", "--   /\\", "-- +----+"
            , "-- Ordinary prose after the diagram."
            ]
      output <- checkedSource indent $ moduleSource $
        ["update source = source", "  { firstField = 0"]
          ++ map (replicate 6 ' ' ++) comments ++ ["  , secondField = 1", "  }"]
      sourceCommentLines output `Hspec.shouldBe` map (replicate 38 ' ' ++) comments
    Hspec.it ("normalizes ordinary record explanations with varied source columns at indent " ++ show indent) $ do
      let comments = ["-- Outer explanation", "-- Nested explanation", "-- Final explanation"]
      output <- checkedSource indent $ moduleSource $
        ["update source = source", "  { firstField = 0"]
          ++ zipWith (\column text -> replicate column ' ' ++ text) [6, 8, 6] comments
          ++ ["  , secondField = 1", "  }"]
      map (dropWhile (== ' ')) (sourceCommentLines output) `Hspec.shouldBe` comments
      forM_ comments $ \marker -> do
        assertFollowingAlignment marker ", secondField" output
        assertRecordOpeningAlignment "firstField" marker output
    Hspec.it ("keeps protected documentation before a later ordinary explanation at indent " ++ show indent) $ do
      output <- checkedSource indent $ moduleSource
        [ "choose value = case value of"
        , "  First -> True"
        , "  -- | Branch docs."
        , ""
        , "  -- branch explanation"
        , "  Second -> False"
        ]
      map (dropWhile (== ' ')) (sourceCommentLines output)
        `Hspec.shouldBe` ["-- | Branch docs.", "-- branch explanation"]
    Hspec.it ("preserves relative ASCII spacing within a case block at indent " ++ show indent) $ do
      let comments = ["-- branch diagram", "--   left -> right", "--        ^ pointer"]
      output <- checkedSource indent $ moduleSource $
        ["choose value = case value of"] ++ map ("  " ++) comments
          ++ ["    First -> True", "    Second -> False"]
      map (dropWhile (== ' '))
        (filter (List.isPrefixOf "--" . dropWhile (== ' ')) $ lines output)
        `Hspec.shouldBe` comments
    Hspec.it ("keeps duplicate case comments with their respective branches at indent " ++ show indent) $ do
      output <- checkedSource indent $ moduleSource
        [ "choose value = case value of"
        , "  -- repeated explanation"
        , "    First -> True"
        , "  -- repeated explanation"
        , "    Second -> False"
        ]
      assertRepeatedAlignment "-- repeated explanation" ["First", "Second"] output
    Hspec.it ("keeps duplicate record comments with their respective fields at indent " ++ show indent) $ do
      output <- checkedSource indent $ moduleSource
        [ "update value = value"
        , "  { firstField = 0"
        , "      -- repeated explanation"
        , "  , secondField = 1"
        , "      -- repeated explanation"
        , "  , thirdField = 2"
        , "  }"
        ]
      assertRepeatedAlignment "-- repeated explanation" [", secondField", ", thirdField"] output
      assertRecordOpeningAlignment "firstField" "-- repeated explanation" output

  forM_ [2, 4] $ \indent -> forM_ continuationContexts $ \(description, declarations) ->
    Hspec.it ("retains inline-seeded " ++ description ++ " continuations at indent " ++ show indent) $ do
      output <- checkedSource indent $ moduleSource declarations
      seed <- uniqueLine "-- seed note" output
      continuation <- uniqueLine "-- continuation note" output
      markerColumn "-- continuation note" continuation
        `Hspec.shouldBe` markerColumn "-- seed note" seed

  forM_ [2, 4] $ \indent -> forM_ [False, True] $ \recordContext ->
    forM_ ["-- | Protected B.", "-- > protected B", "--   left -> right"] $ \protected ->
      Hspec.it ("keeps ordinary/protected/ordinary comment order in "
        ++ (if recordContext then "a record" else "a case") ++ " at indent "
        ++ show indent ++ " around " ++ protected) $ do
        let comments = ["-- Ordinary A.", protected, "-- Ordinary C."]
            declarations = if recordContext
              then
                [ "update value = value"
                , "  { firstField = 0"
                , "      -- Ordinary A."
                , ""
                , "      " ++ protected
                , ""
                , "      -- Ordinary C."
                , "  , secondField = 1"
                , "  }"
                ]
              else
                [ "choose value = case value of"
                , "  First -> True"
                , "  -- Ordinary A."
                , ""
                , "  " ++ protected
                , ""
                , "  -- Ordinary C."
                , "  Second -> False"
                ]
        output <- checkedSource indent $ moduleSource declarations
        map (dropWhile (== ' ')) (sourceCommentLines output) `Hspec.shouldBe` comments
        assertFollowingAlignment "-- Ordinary C."
          (if recordContext then ", secondField" else "Second") output
        if recordContext
          then assertRecordOpeningAlignment "firstField" "-- Ordinary C." output
          else pure ()

  forM_ [2, 4] $ \indent -> do
    Hspec.it ("preserves record-field Haddock association at indent " ++ show indent) $ do
      output <- checkedSource indent $ moduleSource
        [ "data Example = Example"
        , "  { firstField :: Int"
        , "  -- | Documentation for secondField."
        , "  , secondField :: Bool"
        , "  }"
        ]
      documentation <- uniqueLine "-- | Documentation for secondField." output
      documentation `Hspec.shouldContain` "-- | Documentation for secondField."
      let afterDocumentation = drop 1 $ dropWhile (/= documentation) $ lines output
      afterDocumentation `Hspec.shouldSatisfy` any (List.isInfixOf ", secondField")
    Hspec.it ("preserves source-sensitive Haddock example and ASCII content at indent " ++ show indent) $ do
      let comments =
            [ "-- | Function docs."
            , "-- > case input of"
            , "--        ^^^^^^^^^^^ ASCII pointer"
            , "-- Ordinary prose after the pointer."
            ]
      output <- checkedSource indent $ moduleSource $
        ["previous = 0", ""] ++ comments ++ ["select :: Int -> Int", "select value = value"]
      filter (List.isPrefixOf "--") (lines output) `Hspec.shouldBe` comments

  forM_ malformedSources $ \(description, declarations) ->
    Hspec.it ("rejects " ++ description ++ " without replacing inplace input") $ do
      let source = moduleSource declarations
      directory <- Directory.getTemporaryDirectory
      Exception.bracket
        (do
          (path, handle) <- IO.openTempFile directory "brittany-standalone-comments-invalid.hs"
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

checkedSource :: Int -> String -> IO String
checkedSource = checkedSourceAt 80

checkedSourceAt :: Int -> Int -> String -> IO String
checkedSourceAt columns indent source = do
  let config = configWithLayout columns indent
  output <- formatChecked config source
  assertStableAndEquivalent config source output
  pure output

uniqueLine :: String -> String -> IO String
uniqueLine marker source = case filter (List.isInfixOf marker) $ lines source of
  [line] -> pure line
  matches -> Hspec.expectationFailure
    ("expected exactly one line containing " ++ show marker ++ ", found " ++ show matches)
    >> fail "missing or duplicated marker"

indentation :: String -> Int
indentation = length . takeWhile (== ' ')

sourceCommentLines :: String -> [String]
sourceCommentLines = filter (List.isPrefixOf "--" . dropWhile (== ' ')) . lines

markerColumn :: String -> String -> Int
markerColumn marker line = length $ fst $ breakAtMarker marker line
 where
  breakAtMarker needle input = case
    [(prefix, suffix) | (prefix, suffix) <- zip (List.inits input) (List.tails input)
                      , needle `List.isPrefixOf` suffix] of
      found : _ -> found
      [] -> (input, "")

assertFollowingAlignment :: String -> String -> String -> IO ()
assertFollowingAlignment commentMarker followingMarker output = do
  comment <- uniqueLine commentMarker output
  let followingLines = drop 1 $ dropWhile (/= comment) $ lines output
  case filter (List.isInfixOf followingMarker) followingLines of
    following : _ -> indentation comment `Hspec.shouldBe` indentation following
    [] -> Hspec.expectationFailure $ "missing following structural line: " ++ followingMarker

assertRepeatedAlignment :: String -> [String] -> String -> IO ()
assertRepeatedAlignment commentMarker followingMarkers output = do
  let comments = filter (List.isInfixOf commentMarker) $ lines output
  length comments `Hspec.shouldBe` length followingMarkers
  forM_ (zip comments followingMarkers) $ \(comment, followingMarker) -> do
    following <- uniqueLine followingMarker output
    indentation comment `Hspec.shouldBe` indentation following

assertRecordOpeningAlignment :: String -> String -> String -> IO ()
assertRecordOpeningAlignment firstFieldMarker commentMarker output = do
  firstField <- uniqueLine firstFieldMarker output
  let throughFirstField = takeWhile (/= firstField) (lines output) ++ [firstField]
      openingColumns = concatMap (List.elemIndices '{') throughFirstField
      comments = filter (List.isInfixOf commentMarker) $ lines output
  comments `Hspec.shouldSatisfy` (not . null)
  case reverse openingColumns of
    openingColumn : _ -> forM_ comments $ \comment ->
      indentation comment `Hspec.shouldBe` openingColumn
    [] -> Hspec.expectationFailure "missing record opening brace before the first field"

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
    parsed <- ParseModule.parseModule ["-haddock"] "StandaloneComments.hs"
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
moduleSource declarations = unlines $ ["module StandaloneComments where", ""] ++ declarations

caseComments :: Bool -> [String]
caseComments nested =
  [ "choose value = case value of"
  , "  -- branch explanation"
  ] ++ if nested
    then
      [ "    Just selected -> case selected of"
      , "    -- inner explanation"
      , "      First -> True"
      , "      Second -> False"
      , "    Nothing -> False"
      ]
    else ["    Just selected -> True", "    Nothing -> False"]

recordComments :: Bool -> [String]
recordComments nested =
  (if nested then ["outer value = Just $ update value", "  where", "    update source = source"]
    else ["update source = source"])
  ++ map (replicate (if nested then 4 else 0) ' ' ++)
    [ "  { firstField = 0"
    , "      -- second field explanation"
    , "  , secondField = 1"
    , "  }"
    ]

continuationContexts :: [(String, [String])]
continuationContexts =
  [ ("case branch",
      [ "choose value = case value of"
      , "  Just selected -> selected -- seed note"
      , "                            -- continuation note"
      , "  Nothing -> fallback"
      ])
  , ("record field",
      [ "update value = value"
      , "  { firstField = 0 -- seed note"
      , "                   -- continuation note"
      , "  , secondField = 1"
      , "  }"
      ])
  ]

malformedSources :: [(String, [String])]
malformedSources =
  [ ("an incomplete case branch",
      ["choose value = case value of", "  -- branch explanation", "  Just selected ->"])
  , ("an incomplete record update",
      ["update value = value { firstField = 0", "  -- field explanation", "  , secondField ="])
  ]
