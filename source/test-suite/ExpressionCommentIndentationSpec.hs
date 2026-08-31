{-# LANGUAGE LambdaCase #-}

module ExpressionCommentIndentationSpec
  ( spec
  ) where

import Control.Monad (forM_)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import Data.Semigroup (Last(..))
import qualified Data.Text as Text
import qualified GHC
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
import Language.Haskell.Brittany.Internal.SourceComment.Types
  ( CommentPlan
  , CommentRole
  , SourceCommentSyntax
  )
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "own-line expression comment indentation" $ do
  Hspec.it "rebases a far-column variable comment to the branch body" $ do
    firstPass <- formatSource defaultConfig variableInput
    firstPass `Hspec.shouldBe` variableExpected
    assertStableAndEquivalent defaultConfig variableInput firstPass

  Hspec.it
    "rebases application, infix, and nested branches at configured indents"
    $ forM_ [2, 4] $ \indentAmount -> do
        let config = configWithWidthAndIndent 50 indentAmount
        firstPass <- formatSource config expressionCorpus
        forM_ expressionPairs $ \(commentNeedle, expressionNeedle) -> do
          commentIndent <- indentationOf commentNeedle firstPass
          expressionIndent <- indentationOf expressionNeedle firstPass
          commentIndent `Hspec.shouldBe` expressionIndent
        commentLines firstPass
          `Hspec.shouldSatisfy` all ((<= 50) . length)
        firstPass `Hspec.shouldContain` "-- delimiter note"
        assertStableAndEquivalent config expressionCorpus firstPass

  Hspec.it "reports malformed expression input without rewriting it" $ do
    parsePrintModule defaultConfig (Text.pack malformedInput) >>= \case
      Left _ -> pure ()
      Right output -> Hspec.expectationFailure
        $ "malformed input formatted as: " ++ Text.unpack output
    malformedInput `Hspec.shouldBe` malformedSourceSnapshot

assertStableAndEquivalent :: Config -> String -> String -> IO ()
assertStableAndEquivalent config original firstPass = do
  assertEquivalent "ExpressionInput.hs" original "ExpressionOutput.hs" firstPass
  originalFingerprint <- commentFingerprint "ExpressionInput.hs" original
  firstFingerprint <- commentFingerprint "ExpressionOutput.hs" firstPass
  firstFingerprint `Hspec.shouldBe` originalFingerprint
  secondPass <- formatSource config firstPass
  thirdPass <- formatSource config secondPass
  secondPass `Hspec.shouldBe` firstPass
  thirdPass `Hspec.shouldBe` firstPass

formatSource :: Config -> String -> IO String
formatSource config input = parsePrintModule config (Text.pack input) >>= \case
  Left errors ->
    Hspec.expectationFailure
        ("formatting returned " ++ show (length errors) ++ " errors")
      >> fail "formatting failed"
  Right output -> pure $ Text.unpack output

assertEquivalent :: FilePath -> String -> FilePath -> String -> IO ()
assertEquivalent inputPath input outputPath output = do
  inputParsed <- parseSource inputPath input
  outputParsed <- parseSource outputPath output
  compareSemanticSyntax inputParsed outputParsed `Hspec.shouldBe` Right Nothing

parseSource :: FilePath -> String -> IO GHC.ParsedSource
parseSource path source = do
  parsed <- ParseModule.parseModule
    ["-haddock"]
    path
    (const $ pure $ Right ())
    source
  case parsed of
    Left parseError -> Hspec.expectationFailure parseError >> fail parseError
    Right (_, parsedSource, ()) -> pure parsedSource

commentFingerprint
  :: FilePath
  -> String
  -> IO [(Text.Text, SourceCommentSyntax, CommentRole, String)]
commentFingerprint filename source = do
  plan <- parsedCommentPlan filename source
  pure $ commentPlanFingerprint plan

parsedCommentPlan :: FilePath -> String -> IO CommentPlan
parsedCommentPlan filename source = do
  parsed <- ParseModule.parseModule
    ["-haddock"]
    filename
    (const $ pure $ Right ())
    source
  case parsed of
    Left parseError -> Hspec.expectationFailure parseError >> fail parseError
    Right (annotations, _, ()) -> case normalizeCommentPlan annotations of
      Left errors -> Hspec.expectationFailure (show errors) >> fail "invalid plan"
      Right plan -> pure plan

indentationOf :: String -> String -> IO Int
indentationOf needle source = case List.find (List.isInfixOf needle) $ lines source of
  Nothing -> Hspec.expectationFailure
    ("missing formatted line containing " ++ show needle)
    >> fail "missing formatted line"
  Just line -> pure $ length $ takeWhile (== ' ') line

commentLines :: String -> [String]
commentLines = filter (List.isPrefixOf "--" . dropWhile (== ' ')) . lines

defaultConfig :: Config
defaultConfig =
  staticDefaultConfig
    { _conf_errorHandling =
        (_conf_errorHandling staticDefaultConfig)
          { _econf_failOnExactSourceFallback = Identity $ Last True
          }
    }

configWithWidthAndIndent :: Int -> Int -> Config
configWithWidthAndIndent width indentAmount =
  defaultConfig
    { _conf_layout =
        (_conf_layout defaultConfig)
          { _lconfig_cols = Identity $ Last width
          , _lconfig_indentAmount = Identity $ Last indentAmount
          }
    }

expressionPairs :: [(String, String)]
expressionPairs =
  [ ("-- application branch", "applyValue first")
  , ("-- infix branch", "leftValue + rightValue")
  , ("-- nested if branch", "firstValue")
  , ("-- nested case branch", "consume value")
  ]

variableInput :: String
variableInput = unlines
  [ "module ExpressionComment where"
  , ""
  , "choose condition ="
  , "  if condition"
  , "    then"
  , farComment "-- branch note"
  , "      branchValue"
  , "    else fallbackValue"
  ]

variableExpected :: String
variableExpected = List.intercalate "\n"
  [ "module ExpressionComment where"
  , ""
  , "choose condition = if condition"
  , "  then"
  , "       -- branch note"
  , "       branchValue"
  , "  else fallbackValue"
  ]

expressionCorpus :: String
expressionCorpus = unlines
  [ "module ExpressionCommentCorpus where"
  , ""
  , "application condition ="
  , "  if condition"
  , "    then"
  , farComment "-- application branch"
  , "      applyValue first second"
  , "    else fallbackValue"
  , ""
  , "infixBranch condition ="
  , "  if condition"
  , "    then"
  , farComment "-- infix branch"
  , "      leftValue + rightValue"
  , "    else fallbackValue"
  , ""
  , "nested condition other ="
  , "  if condition"
  , "    then"
  , "      if other"
  , "        then"
  , farComment "-- nested if branch"
  , "          firstValue"
  , "        else secondValue"
  , "    else"
  , "      case other of"
  , "        Just value ->"
  , farComment "-- nested case branch"
  , "          consume value"
  , "        Nothing -> fallbackValue"
  , ""
  , "delimited ="
  , "  [ firstValue"
  , farComment "-- delimiter note"
  , "  , secondValue"
  , "  ]"
  ]

farComment :: String -> String
farComment contents = replicate 60 ' ' ++ contents

malformedInput :: String
malformedInput = unlines
  [ "module MalformedExpressionComment where"
  , "value condition = if condition"
  , "  then"
  , farComment "-- retained on failure"
  , "  else"
  ]

malformedSourceSnapshot :: String
malformedSourceSnapshot = malformedInput
