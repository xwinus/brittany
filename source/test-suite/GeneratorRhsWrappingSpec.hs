{-# LANGUAGE LambdaCase #-}

module GeneratorRhsWrappingSpec (spec) where

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
spec projectRoot = Hspec.describe "generator RHS wrapping" $ do
  Hspec.it "fits the reported generator in the complete CommentBoundaryGraphSpec" $ do
    source <- readFile $ projectRoot </> "source/test-suite/CommentBoundaryGraphSpec.hs"
    let config = configWithLayout 80 2
    output <- formatChecked config source
    let generatorLines = filter
          (List.isInfixOf "CommentBoundaryId (DelimiterBoundaryPath _) gap <-")
          $ lines output
    length generatorLines `Hspec.shouldBe` 1
    map length generatorLines `Hspec.shouldSatisfy` all (<= 80)
    generatorLines `Hspec.shouldSatisfy`
      all (not . List.isInfixOf "canonicalCommentBoundary")
    let rhsLines = filter
          (List.isInfixOf "[canonicalCommentBoundary comment]") $ lines output
    length rhsLines `Hspec.shouldBe` 1
    map leadingSpaces rhsLines `Hspec.shouldBe` [14]
    assertStableAndEquivalent config source output

  Hspec.it "preserves the surrounding base of inline pattern guards" $ do
    source <- readFile $ projectRoot </>
      "source/library/Language/Haskell/Brittany/Internal/Transformations/Columns.hs"
    let config = configWithLayout 80 2
    output <- formatChecked config source
    output `Hspec.shouldContain` "\n      _ <- List.last lines1, sig1 == sig2 ->"
    assertStableAndEquivalent config source output

  forM_ [False, True] $ \isDo ->
    Hspec.it ("breaks the reduced " ++ (if isDo then "do bind" else "generator")) $ do
      let binding = longPattern ++ " <- [canonicalCommentBoundary comment]"
          declarations = if isDo
            then ["example = do", "  " ++ binding, "  pure gap"]
            else ["example = [gap | " ++ binding ++ "]"]
      output <- checkWithinColumns 80 2 $ moduleSource declarations
      output `Hspec.shouldContain` "[canonicalCommentBoundary comment]"
      arrowLines output `Hspec.shouldSatisfy`
        all (not . List.isInfixOf "canonicalCommentBoundary")
      map leadingSpaces (filter (List.isInfixOf "canonicalCommentBoundary") $ lines output)
        `Hspec.shouldSatisfy` all (<= 8)

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ [0, 1] $ \overflow ->
      Hspec.it
        ("handles a bind " ++ (if overflow == 0 then "exactly at" else "one past")
          ++ " width " ++ show columns ++ " at indent " ++ show indent) $ do
          let literal = show $ replicate (columns - indent - length "p <- \"\"" + overflow) 'x'
          output <- checkWithinColumns columns indent $ moduleSource
            ["example = do", "  p <- " ++ literal, "  pure p"]
          if overflow == 0
            then do
              arrowLines output `Hspec.shouldSatisfy` any (List.isInfixOf literal)
              map length (arrowLines output) `Hspec.shouldBe` [columns]
            else arrowLines output `Hspec.shouldSatisfy`
              all (not . List.isInfixOf literal)
          output `Hspec.shouldContain` literal

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent ->
    Hspec.it
      ("fits a list generator at width " ++ show columns ++ " and indent " ++ show indent) $ do
        let literal = show $ replicate (columns - 4 * indent - 4) 'x'
        output <- checkWithinColumns columns indent $ moduleSource
          ["example = [p | ConstructorWithPayload p <- [" ++ literal ++ "]]"]
        output `Hspec.shouldContain` literal
        arrowLines output `Hspec.shouldSatisfy` all (not . List.isInfixOf literal)

  Hspec.it "fits nested comprehensions with multiple generators and guards" $ do
    output <- checkWithinColumns 80 2 $ moduleSource
      [ "example ="
      , "  [ [gap | " ++ longPattern ++ " <- [canonicalCommentBoundary comment], gap /= 0]"
      , "  | comment <- comments"
      , "  , enabled comment"
      , "  ]"
      ]
    output `Hspec.shouldContain` "canonicalCommentBoundary comment"
    output `Hspec.shouldContain` "enabled comment"

  forM_ ["ConstructorWithALongName first second", "(first : second : rest)"] $ \patternText ->
    Hspec.it ("fits multiple RHS elements after pattern " ++ patternText) $ do
      output <- checkWithinColumns 40 2 $ moduleSource
        [ "example = [first | " ++ patternText
            ++ " <- [makeValue first, makeValue second]]"
        ]
      output `Hspec.shouldContain` "makeValue first"
      output `Hspec.shouldContain` "makeValue second"

  Hspec.it "supports the shared MonadComprehensions path" $ do
    output <- checkWithinColumns 80 2 $ unlines
      [ "{-# LANGUAGE MonadComprehensions #-}"
      , "module GeneratorRhs where"
      , "example = [gap | " ++ longPattern ++ " <- [canonicalCommentBoundary comment]]"
      ]
    output `Hspec.shouldContain` "[canonicalCommentBoundary comment]"
    arrowLines output `Hspec.shouldSatisfy`
      all (not . List.isInfixOf "canonicalCommentBoundary")

  forM_
    [ ("comma", ["  , -- generator note", "    " ++ longPattern ++ " <-", "      [canonicalCommentBoundary comment]"])
    , ("arrow", ["  , " ++ longPattern ++ " <- -- generator note", "      [canonicalCommentBoundary comment]"])
    , ("list", ["  , " ++ longPattern ++ " <-", "      [ -- generator note", "        canonicalCommentBoundary comment]"])
    ] $ \(boundary, generator) ->
      Hspec.it ("preserves a comment at the generator " ++ boundary ++ " boundary") $ do
        output <- checkWithinColumns 80 2 $ moduleSource
          (["example =", "  [gap | comment <- comments"] ++ generator ++ ["  ]"])
        output `Hspec.shouldContain` "generator note"
        output `Hspec.shouldContain` "canonicalCommentBoundary comment"

  Hspec.it "retains fitting compact generators and ordinary do binds" $ do
    output <- checkWithinColumns 80 2 $ moduleSource
      ["example = [x | x <- xs]", "action = do", "  x <- getValue", "  pure x"]
    output `Hspec.shouldContain` "example = [ x | x <- xs ]"
    output `Hspec.shouldContain` "  x <- getValue"

  forM_ [False, True] $ \longPatternToken ->
    Hspec.it ("preserves an indivisible " ++ (if longPatternToken then "pattern" else "literal")) $ do
      let token = if longPatternToken then 'p' : replicate 90 'x' else show $ replicate 90 'x'
          binding = if longPatternToken then token ++ " <- xs" else "p <- " ++ token
          source = moduleSource ["example = [() | " ++ binding ++ "]"]
          config = configWithLayout 40 2
      output <- formatChecked config source
      output `Hspec.shouldContain` token
      filter ((> 40) . length) (lines output) `Hspec.shouldSatisfy`
        all (List.isInfixOf token)
      assertStableAndEquivalent config source output

  Hspec.it "rejects a malformed generator without replacing the inplace input" $ do
    let source = moduleSource ["example = [x | x <- ]"]
    directory <- Directory.getTemporaryDirectory
    Exception.bracket
      (do
        (path, handle) <- IO.openTempFile directory "brittany-generator-invalid.hs"
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
    parsed <- ParseModule.parseModule ["-haddock"] "GeneratorRhs.hs"
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

arrowLines :: String -> [String]
arrowLines = filter (List.isInfixOf "<-") . lines

leadingSpaces :: String -> Int
leadingSpaces = length . takeWhile (== ' ')

longPattern :: String
longPattern = "CommentBoundaryId (DelimiterBoundaryPath _) gap"

moduleSource :: [String] -> String
moduleSource declarations = unlines $ ["module GeneratorRhs where", ""] ++ declarations
