{-# LANGUAGE LambdaCase #-}

module ListPatternLayoutSpec (spec) where

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
import Language.Haskell.Brittany.Internal.Config.Types (IndentPolicy(..))
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SemanticFingerprint
  ( compareSemanticSyntax
  )
import qualified Language.Haskell.Brittany.Main as Brittany
import ListPatternLayoutFixtures
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "multiline list pattern layout" $ do
  Hspec.it "retains the fitting prefix in the reported nested constructor" $ do
    output <- checkedSource 80 2 IndentPolicyFree $ reportedSource "ownerKey'"
    assertOuterAlignment "BDComment planned" 2 output
    output `Hspec.shouldContain` ", BDAnnotationPrior PriorCommentSource ownerKey'"
    constructor <- uniqueLine "BDAnnotationPrior" output
    argument <- uniqueLine "(BDAlt alternatives)" output
    nameColumn <- columnOf "BDAnnotationPrior" constructor
    leadingSpaces argument `Hspec.shouldBe` nameColumn + 2

  Hspec.it "reserves the closing bracket suffix at the exact child-width boundary" $ do
    output <- checkedSource 80 2 IndentPolicyFree $ reportedSource "ownerKey"
    assertOuterAlignment "BDComment planned" 2 output
    output `Hspec.shouldContain` "BDAnnotationPrior PriorCommentSource ownerKey"
    closing <- uniqueLine "] ->" output
    dropWhile (== ' ') closing `Hspec.shouldBe` "] ->"

  Hspec.it "aligns the maintained pattern in the complete CommentIRSpec module" $ do
    source <- readFile $ projectRoot </> "source/test-suite/CommentIRSpec.hs"
    output <- checkedModule (configWithLayout 80 2 IndentPolicyFree) source
    let occurrence = unlines $ takeWhile (not . List.isInfixOf "_ ->")
          $ dropWhile (not . List.isInfixOf "case unwrapBriDocNumbered lowered of") $ lines output
    assertOuterAlignment "BDComment planned" 2 occurrence
    occurrence `Hspec.shouldContain` ", BDAnnotationPrior PriorCommentSource ownerKey'"
    filter ((> 80) . length) (lines occurrence) `Hspec.shouldBe` []

  forM_ [40, 80, 100] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ policies $ \policy -> do
      Hspec.it ("aligns a list pattern and retains fitting prefixes" ++ layoutDescription columns indent policy) $ do
        output <- checkedSource columns indent policy $ moduleSource
          [ "example value = case value of"
          , "  Box"
          , "    [ First firstArgument secondArgument thirdArgument"
          , "    , Second fourthArgument fifthArgument sixthArgument"
          , "    ] -> done"
          ]
        assertOuterAlignment "First" 2 output
        unlines (map (unwords . words) $ lines output) `Hspec.shouldContain` "First firstArgument"
        unlines (map (unwords . words) $ lines output) `Hspec.shouldContain` "Second fourthArgument"
        assertConstructorContinuation indent policy "First"
          ["firstArgument", "secondArgument", "thirdArgument"] output
        assertConstructorContinuation indent policy "Second"
          ["fourthArgument", "fifthArgument", "sixthArgument"] output
      Hspec.it ("aligns nested constructor and list patterns" ++ layoutDescription columns indent policy) $ do
        output <- checkedSource columns indent policy $ moduleSource
          [ "example value = case value of"
          , "  Box"
          , "    [ Wrap [First one two three, Second four five six]"
          , "    , Wrap [Third seven eight nine, Fourth ten eleven twelve]"
          , "    ] -> done"
          ]
        assertOuterAlignment "Wrap" 2 output
      Hspec.it ("preserves compact empty singleton and fitting list patterns" ++ layoutDescription columns indent policy) $ do
        output <- checkedSource columns indent policy $ moduleSource
          [ "empty [] = done"
          , "singleton [one] = one"
          , "small [one, two] = one"
          , "nested [Just one] = one"
          ]
        forM_ ["empty []", "singleton [one]", "small [one, two]", "nested [Just one]"] $ \marker ->
          output `Hspec.shouldContain` marker
      Hspec.it ("retains short tuple binders" ++ layoutDescription columns indent policy) $ do
        output <- checkedSource columns indent policy $ moduleSource
          ["example = use $ \\(one, two) -> result one two"]
        output `Hspec.shouldContain` "\\(one, two) ->"
      Hspec.it ("keeps structural case heads within the column limit" ++ layoutDescription columns indent policy) $ do
        output <- checkedSource columns indent policy $ moduleSource
          [ "example value = case value of"
          , "  [First first second third, Second fourth fifth sixth] -> done"
          ]
        output `Hspec.shouldContain` "First"
        output `Hspec.shouldContain` "->"
        _ <- uniqueLine "done" output
        pure ()
      Hspec.it ("preserves list expression delimiter alignment" ++ layoutDescription columns indent policy) $ do
        output <- checkedSource columns indent policy $ moduleSource
          [ "example ="
          , "  [ firstSelectedExpressionValue"
          , "  , secondSelectedExpressionValue"
          , "  , thirdSelectedExpressionValue"
          , "  , fourthSelectedExpressionValue"
          , "  ]"
          ]
        assertOuterAlignment "firstSelectedExpressionValue" 4 output

  forM_ [2, 4] $ \indent -> forM_ commentCases $ \(description, entries) ->
    Hspec.it ("preserves " ++ description ++ " in a pattern at indent " ++ show indent) $ do
      output <- checkedSource 80 indent IndentPolicyFree $ moduleSource $
        ["example value = case value of", "  Box"]
          ++ map ("  " ++) entries ++ ["    ] -> done"]
      assertOuterAlignment "First" 3 output
      forM_ ["First", "Second", "Third"] $ \marker ->
        length (filter (List.isInfixOf marker) $ lines output) `Hspec.shouldBe` 1

  Hspec.it "expands an oversized singleton constructor pattern safely" $ do
    output <- checkedSource 40 2 IndentPolicyFree $ moduleSource
      [ "example value = case value of"
      , "  Box [LongConstructor firstArgument secondArgument thirdArgument] -> done"
      ]
    output `Hspec.shouldContain` "LongConstructor"
    forM_ ["firstArgument", "secondArgument", "thirdArgument"] $ \marker -> do
      _ <- uniqueLine marker output
      pure ()

  Hspec.it "retains fitting prefixes with many short constructor arguments" $ do
    output <- checkedSource 40 2 IndentPolicyFree $ moduleSource
      [ "example value = case value of"
      , "  Box [LongConstructor a b c d e f g h i j k l m n o p q, Other r s] -> done"
      ]
    assertOuterAlignment "LongConstructor" 2 output
    output `Hspec.shouldContain` "LongConstructor a b c"

  Hspec.it "preserves an indivisible constructor name wider than the target" $ do
    let name = "Constructor" ++ replicate 45 'X'
        source = moduleSource
          [ "example value = case value of"
          , "  Box [" ++ name ++ " one two, Other three] -> done"
          ]
    output <- checkedModule (configWithLayout 40 2 IndentPolicyFree) source
    _ <- uniqueLine name output
    pure ()

  Hspec.it "accounts for a parenthesized child's exact closing suffix width" $ do
    output <- checkedSource 40 2 IndentPolicyFree $ moduleSource
      [ "example value = case value of"
      , "  Box [First firstArgument, Second (Nested selectedArgumentXX)] -> done"
      ]
    assertOuterAlignment "First" 2 output
    child <- uniqueLine "Second (Nested selectedArgumentXX)" output
    length child `Hspec.shouldBe` 40

  forM_ [(53, 4), (57, 2)] $ \(argumentWidth, indent) ->
    Hspec.it ("reserves a parenthesized constructor's closing suffix at indent " ++ show indent) $ do
      let argument = "argument" ++ replicate (argumentWidth - 8) 'X'
      output <- checkedSource 80 indent IndentPolicyFree $ moduleSource
        [ "f value = case value of"
        , "  Box [First x, (Second " ++ argument ++ " finalArg)] -> done"
        , "  _ -> fallback"
        ]
      assertOuterAlignment "First" 2 output
      prefix <- uniqueLine argument output
      final <- uniqueLine "finalArg" output
      final `Hspec.shouldNotBe` prefix

  forM_ ["", "Box "] $ \constructor ->
    Hspec.it ("keeps a guarded " ++ constructor ++ "list pattern within the column limit") $ do
      output <- checkedSource 40 2 IndentPolicyFree $ moduleSource
        [ "example value = case value of"
        , "  " ++ constructor
            ++ "[First firstArgument secondArgument, Second thirdArgument fourthArgument]"
            ++ " | ready -> done"
        , "  _ -> fallback"
        ]
      output `Hspec.shouldContain` "First"
      output `Hspec.shouldContain` "Second"
      output `Hspec.shouldContain` "| ready"

  forM_ barePatternContexts $ \(description, declarations) ->
    Hspec.it ("preserves the layout boundary of " ++ description) $ do
      output <- checkedSource 40 2 IndentPolicyFree $ moduleSource declarations
      output `Hspec.shouldContain` "First"
      output `Hspec.shouldContain` "Second"

  forM_ malformedLists $ \(description, declarations) ->
    Hspec.it ("rejects " ++ description ++ " without replacing inplace input") $ do
      let source = moduleSource declarations
      directory <- Directory.getTemporaryDirectory
      Exception.bracket
        (do
          (path, handle) <- IO.openTempFile directory "brittany-list-pattern-invalid.hs"
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

uniqueLine :: String -> String -> IO String
uniqueLine marker output = case filter (List.isInfixOf marker) $ lines output of
  [line] -> pure line
  matches -> Hspec.expectationFailure ("expected one " ++ marker ++ " line, found " ++ show matches)
    >> fail "missing or repeated line"

columnOf :: String -> String -> IO Int
columnOf marker line = case List.findIndex (List.isPrefixOf marker) $ List.tails line of
  Just column -> pure column
  Nothing -> Hspec.expectationFailure ("missing marker " ++ marker) >> fail "missing marker"

assertConstructorContinuation :: Int -> IndentPolicy -> String -> [String] -> String -> IO ()
assertConstructorContinuation indent policy constructor arguments output = do
  header <- uniqueLine constructor output
  nameColumn <- columnOf constructor header
  let expectedColumn = case policy of
        IndentPolicyMultiple -> (nameColumn `div` indent + 1) * indent
        _ -> nameColumn + indent
  forM_ arguments $ \marker -> do
    argument <- uniqueLine marker output
    if argument == header then pure () else
      leadingSpaces argument `Hspec.shouldBe` expectedColumn

leadingSpaces :: String -> Int
leadingSpaces = length . takeWhile (== ' ')

checkedModule :: Config -> String -> IO String
checkedModule config source = do
  output <- formatChecked config source
  assertStableAndEquivalent config source output
  pure output

checkedSource :: Int -> Int -> IndentPolicy -> String -> IO String
checkedSource columns indent policy source = do
  output <- checkedModule (configWithLayout columns indent policy) source
  filter ((> columns) . length) (lines output) `Hspec.shouldBe` []
  pure output

assertOuterAlignment :: String -> Int -> String -> IO ()
assertOuterAlignment marker elementCount source = do
  markerOffset <- case List.findIndex (List.isPrefixOf marker) $ List.tails source of
    Just offset -> pure offset
    Nothing -> Hspec.expectationFailure ("missing list marker " ++ marker) >> fail "missing marker"
  opening <- case [offset | (offset, '[') <- punctuation source, offset < markerOffset] of
    [] -> Hspec.expectationFailure "missing outer opening bracket" >> fail "missing opening"
    offsets -> pure $ last offsets
  let delimiters = opening : selectDelimiters 1
        (dropWhile ((<= opening) . fst) $ punctuation source)
      column offset = length $ takeWhile (/= '\n') $ reverse $ take offset source
  length delimiters `Hspec.shouldBe` elementCount + 1
  map column delimiters `Hspec.shouldBe` replicate (elementCount + 1) (column opening)

selectDelimiters :: Int -> [(Int, Char)] -> [Int]
selectDelimiters _ [] = []
selectDelimiters depth ((offset, symbol) : rest) = case symbol of
  '[' -> selectDelimiters (depth + 1) rest
  ']' | depth == 1 -> [offset]
      | otherwise -> selectDelimiters (depth - 1) rest
  ',' | depth == 1 -> offset : selectDelimiters depth rest
  _ -> selectDelimiters depth rest

-- The fixtures contain nested lists, quoted punctuation and delimiter-like comments.
-- Only punctuation outside those strings/comments contributes to list structure.
punctuation :: String -> [(Int, Char)]
punctuation = normal 0
 where
  normal _ [] = []
  normal offset ('"' : rest) = quoted (offset + 1) rest
  normal offset ('-' : '-' : rest) = lineComment (offset + 2) rest
  normal offset ('{' : '-' : rest) = blockComment 1 (offset + 2) rest
  normal offset (char : rest)
    | char `elem` "[]," = (offset, char) : normal (offset + 1) rest
    | otherwise = normal (offset + 1) rest
  quoted _ [] = []
  quoted offset ('\\' : _ : rest) = quoted (offset + 2) rest
  quoted offset ('"' : rest) = normal (offset + 1) rest
  quoted offset (_ : rest) = quoted (offset + 1) rest
  lineComment _ [] = []
  lineComment offset ('\n' : rest) = normal (offset + 1) rest
  lineComment offset (_ : rest) = lineComment (offset + 1) rest
  blockComment :: Int -> Int -> String -> [(Int, Char)]
  blockComment _ _ [] = []
  blockComment depth offset ('{' : '-' : rest) = blockComment (depth + 1) (offset + 2) rest
  blockComment 1 offset ('-' : '}' : rest) = normal (offset + 2) rest
  blockComment depth offset ('-' : '}' : rest) = blockComment (depth - 1) (offset + 2) rest
  blockComment depth offset (_ : rest) = blockComment depth (offset + 1) rest

layoutDescription :: Int -> Int -> IndentPolicy -> String
layoutDescription columns indent policy = " at width " ++ show columns
  ++ ", indent " ++ show indent ++ ", " ++ show policy

policies :: [IndentPolicy]
policies = [IndentPolicyLeft, IndentPolicyMultiple, IndentPolicyFree]

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
    parsed <- ParseModule.parseModule ["-haddock"] "ListPatternLayout.hs"
      (const $ pure $ Right ()) source
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError >> fail parseError
      Right result -> pure result

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
