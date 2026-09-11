{-# LANGUAGE LambdaCase #-}

module UnfittableInfixRhsSpec (spec) where

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
spec projectRoot = Hspec.describe "unfittable infix RHS layout" $ do
  Hspec.it "avoids operator-prefix overflow in the complete LocalTrailingCommentSpec" $ do
    source <- readFile $ projectRoot </> "source/test-suite/LocalTrailingCommentSpec.hs"
    output <- checkedSource 80 2 source
    assertUnprefixedLiteral 8 "`Hspec.shouldContain`" (show reportedMessage) output

  Hspec.it "keeps the reported indivisible string intact on a normal continuation" $ do
    let literal = show reportedMessage
    output <- checkedSource 80 2 $ assertionSource "`Hspec.shouldContain`" literal
    assertUnprefixedLiteral 6 "`Hspec.shouldContain`" literal output

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent -> do
    forM_ ["`shouldContain`", "`Hspec.shouldContain`", "<++++++++++++>"] $ \operator ->
      forM_ (literalCases columns) $ \(description, literal) ->
        Hspec.it ("avoids prefix growth for " ++ description ++ " after " ++ operator
          ++ layoutDescription columns indent) $ do
          output <- checkedSource columns indent $ assertionSource operator literal
          assertUnprefixedLiteral (3 * indent) operator literal output
          assertOnlyLiteralOverflow columns [literal] output

    Hspec.it ("keeps a parenthesized oversized literal intact" ++ layoutDescription columns indent) $ do
      let literal = show $ replicate (columns + 8) 'p'
      output <- checkedSource columns indent $
        assertionSource "`shouldContain`" $ "(" ++ literal ++ ")"
      assertUnprefixedLiteral (3 * indent + 1) "`shouldContain`" literal output
      assertOnlyLiteralOverflow columns [literal] output

    Hspec.it ("reduces avoidable width throughout a flattened literal chain" ++ layoutDescription columns indent) $ do
      let first = show $ replicate (columns + 8) 'a'
          second = show $ replicate (columns + 9) 'b'
          operator = "<++++++++++++>"
      output <- checkedSource columns indent $ moduleSource
        ["example = do", "  result " ++ operator ++ " " ++ first ++ " " ++ operator ++ " " ++ second]
      forM_ [first, second] $ \literal ->
        assertUnprefixedLiteral (3 * indent) operator literal output
      assertOnlyLiteralOverflow columns [first, second] output

    Hspec.it ("only breaks a path join when its prefix exceeds regular indentation" ++ layoutDescription columns indent) $ do
      let literal = show $ "directory/" ++ replicate columns 'p' ++ "/file.hs"
      output <- checkedSource columns indent $ assertionSource "</>" literal
      if indent == 2
        then assertUnprefixedLiteral (3 * indent) "</>" literal output
        else output `Hspec.shouldContain` ("</> " ++ literal)
      assertOnlyLiteralOverflow columns [literal] output

    Hspec.it ("retains fitting continuations and compact assertions" ++ layoutDescription columns indent) $ do
      let literal = show $ replicate (columns - 3 * indent - 4) 'x'
      output <- checkedSource columns indent $ assertionSource "`Hspec.shouldContain`" literal
      assertUnprefixedLiteral (3 * indent) "`Hspec.shouldContain`" literal output
      filter ((> columns) . length) (lines output) `Hspec.shouldBe` []
      compact <- checkedSource columns indent $ assertionSource "`has`" "\"small\""
      compact `Hspec.shouldContain` "result `has` \"small\""

    Hspec.it ("preserves an unsupported negative-literal operand" ++ layoutDescription columns indent) $ do
      let literal = replicate (columns + 8) '7'
      output <- checkedSource columns indent $ assertionSource "`shouldContain`" $ "(-" ++ literal ++ ")"
      output `Hspec.shouldContain` literal
      assertOnlyLiteralOverflow columns [literal] output

    forM_ ["-- RHS boundary", "{- RHS boundary -}"] $ \comment ->
      Hspec.it ("preserves the operator boundary comment " ++ comment ++ layoutDescription columns indent) $ do
        let literal = show $ replicate (columns + 8) 'c'
            operator = "`Hspec.shouldContain`"
        output <- checkedSource columns indent $ moduleSource
          ["example = do", "  result " ++ operator ++ " " ++ comment, "    " ++ literal]
        output `Hspec.shouldContain` (operator ++ " " ++ comment)
        assertUnprefixedLiteral (3 * indent) operator literal output

  forM_ [40, 80] $ \columns -> forM_ [("$", 2), ("$", 4), ("<>", 4)] $ \(operator, indent) ->
    Hspec.it ("retains an operator prefix no wider than regular indentation: " ++ operator
      ++ layoutDescription columns indent) $ do
      let literal = show $ replicate (columns + 8) 's'
      output <- checkedSource columns indent $ assertionSource operator literal
      output `Hspec.shouldContain` (operator ++ " " ++ literal)
      assertOnlyLiteralOverflow columns [literal] output

  forM_ blockCases $ \(description, declarations) ->
    Hspec.it ("preserves block-relative indentation for " ++ description) $ do
      output <- checkedSource 40 2 $ moduleSource declarations
      filter ((> 40) . length) (lines output) `Hspec.shouldBe` []
      body <- literalLine "perform firstValue" output
      indentation body `Hspec.shouldSatisfy` (<= 6)

  forM_ [2, 4] $ \indent -> forM_ [False, True] $ \chain ->
    Hspec.it ("retains the existing oversized list fallback at indent " ++ show indent
      ++ if chain then " in a chain" else " in a direct application") $ do
      let literal = show $ replicate 90 'l'
          operator = "`appendLists`"
          prefix = if chain then "prefix " ++ operator ++ " []" else "prefix"
      output <- checkedSource 80 indent $ moduleSource
        [ "example = do"
        , "  result `shouldBe` (" ++ prefix ++ " " ++ operator
          ++ " [" ++ literal ++ ", \"small\"])"
        ]
      output `Hspec.shouldContain` (operator ++ " [ " ++ literal)
      assertOnlyLiteralOverflow 80 [literal] output

  forM_ malformedApplications $ \(description, declarations) ->
    Hspec.it ("rejects " ++ description ++ " without replacing inplace input") $ do
      let source = moduleSource declarations
      directory <- Directory.getTemporaryDirectory
      Exception.bracket
        (do
          (path, handle) <- IO.openTempFile directory "brittany-unfittable-infix-invalid.hs"
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
  assertStableAndEquivalent config source output
  pure output

assertUnprefixedLiteral :: Int -> String -> String -> String -> IO ()
assertUnprefixedLiteral maximumIndent operator literal output = do
  line <- literalLine literal output
  line `Hspec.shouldNotContain` operator
  indentation line `Hspec.shouldSatisfy` (<= maximumIndent)
  indentation line `Hspec.shouldSatisfy` (> 0)

assertOnlyLiteralOverflow :: Int -> [String] -> String -> IO ()
assertOnlyLiteralOverflow columns literals output = do
  let overflow = filter ((> columns) . length) $ lines output
  length overflow `Hspec.shouldBe` length literals
  forM_ overflow $ \line ->
    line `Hspec.shouldSatisfy` \row -> any (`List.isInfixOf` row) literals

literalLine :: String -> String -> IO String
literalLine literal output = case filter (List.isInfixOf literal) $ lines output of
  [line] -> pure line
  matches -> Hspec.expectationFailure
    ("expected one intact literal line, found " ++ show matches)
    >> fail "missing or duplicated literal"

indentation :: String -> Int
indentation = length . takeWhile (== ' ')

layoutDescription :: Int -> Int -> String
layoutDescription columns indent = " at width " ++ show columns ++ " and indent " ++ show indent

literalCases :: Int -> [(String, String)]
literalCases columns =
  [ ("a string", show $ replicate (columns + 8) 'x')
  , ("an integer", replicate (columns + 8) '1')
  , ("a primitive string", show (replicate (columns + 8) 'x') ++ "#")
  , ("a primitive integer", replicate (columns + 8) '1' ++ "#")
  ]

blockCases :: [(String, [String])]
blockCases =
  [ ("a do RHS",
      [ "example = runSelectedAction $ do"
      , "  perform firstValue"
      , "  perform secondValue"
      ])
  , ("a lambda-case RHS",
      [ "example = runSelectedAction $ \\case"
      , "  First -> perform firstValue"
      , "  Second -> perform secondValue"
      ])
  ]

malformedApplications :: [(String, [String])]
malformedApplications =
  [ ("a missing infix RHS", ["example = result `shouldContain`"])
  , ("an unterminated string RHS", ["example = result `shouldContain` \"unterminated"])
  ]

assertionSource :: String -> String -> String
assertionSource operator literal = moduleSource
  ["example = do", "  result " ++ operator ++ " " ++ literal]

moduleSource :: [String] -> String
moduleSource declarations = unlines $
  ["{-# LANGUAGE MagicHash, LambdaCase #-}", "module UnfittableInfixRhs where", ""] ++ declarations

reportedMessage :: String
reportedMessage = "-- position (e.g., \"then\" keyword for then-expression comments), so that"

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
    parsed <- ParseModule.parseModule ["-haddock"] "UnfittableInfixRhs.hs"
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
