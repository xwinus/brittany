module PreprocessorSpec (spec) where

import qualified Data.Text.IO as Text.IO
import Language.Haskell.Brittany
  ( BrittanyError(..)
  , parsePrintModule
  , staticDefaultConfig
  )
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "CPP diagnostics" $ do
  cppFailureExample projectRoot
    "explains how to handle ordinary CPP input"
    "CompatibilityCppUnsupported.hs"
  cppFailureExample projectRoot
    "rejects include directives before preprocessing"
    "CompatibilityCppEdge.hs"
  cppFailureExample projectRoot
    "reports the CPP policy before malformed directives"
    "CompatibilityCppInvalid.hs"

cppFailureExample :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
cppFailureExample projectRoot description fixtureName =
  Hspec.it description $ do
    input <- Text.IO.readFile $ FilePath.combine
      (FilePath.combine projectRoot "source/test-suite/fixtures")
      fixtureName
    result <- parsePrintModule staticDefaultConfig input
    case result of
      Left [ErrorInput message] ->
        message `Hspec.shouldBe`
          "CPP is unsupported. Preprocess the input before running brittany or remove -XCPP."
      Left errors -> Hspec.expectationFailure
        $ "expected one CPP input error, got " ++ show (length errors)
      Right{} -> Hspec.expectationFailure "expected CPP input to be rejected"
