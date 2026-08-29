{-# LANGUAGE LambdaCase #-}

module StructuralCommentSpec (spec) where

import qualified Data.Text as Text
import Language.Haskell.Brittany
  ( parsePrintModule
  , staticDefaultConfig
  )
import Language.Haskell.Brittany.Internal.CommentPlan
  ( commentPlanFingerprint
  , commentPlanStructuralFingerprint
  , normalizeCommentPlan
  )
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SourceComment.Types
  ( CommentAnchor
  , CommentLineRelation
  , CommentPlan
  , CommentRole
  , SourceCommentSyntax
  )
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "structurally anchored comments" $ do
  Hspec.it "canonicalizes inline token comments without growing whitespace" $ do
    firstPass <- formatChecked inlineSource
    firstPass `Hspec.shouldContain` "do -- after do"
    firstPass `Hspec.shouldContain` "let -- after let"
    firstPass `Hspec.shouldContain` "= -- after equals"
    firstPass `Hspec.shouldContain` "= -- after function equals"
    firstPass `Hspec.shouldContain` "= -- disabled tuple binding"
    firstPass `Hspec.shouldContain` "-- after application"
    firstPass `Hspec.shouldContain` "\\value -> -- after arrow"
    firstPass `Hspec.shouldContain` "Just value -> -- after case arrow"
    assertStable "InlineComments.hs" inlineSource firstPass

  Hspec.it "keeps comments between function equations on one boundary" $ do
    firstPass <- formatChecked equationSource
    commentLines firstPass `Hspec.shouldBe` commentLines equationSource
    assertStable "EquationComments.hs" equationSource firstPass

  Hspec.it "keeps own-line comments after a lambda arrow before its body" $ do
    firstPass <- formatChecked lambdaOwnLineSource
    commentLines firstPass `Hspec.shouldBe` commentLines lambdaOwnLineSource
    firstPass `Hspec.shouldContain` "\\input ->\n  -- first lambda note"
    assertStable "LambdaOwnLineComments.hs" lambdaOwnLineSource firstPass

  Hspec.it "anchors comments before list-comprehension results before the bar" $ do
    firstPass <- formatChecked listComprehensionSource
    commentLines firstPass `Hspec.shouldBe` [Text.pack "-- result note"]
    firstPass `Hspec.shouldContain` "-- result note\n    value\n  |"
    assertStable "ListComprehensionComments.hs" listComprehensionSource firstPass

  Hspec.it "preserves consecutive, block, Haddock, and nested comments" $ do
    firstPass <- formatChecked mixedSource
    commentLines firstPass `Hspec.shouldBe` commentLines mixedSource
    assertStable "MixedComments.hs" mixedSource firstPass

  Hspec.it "reports malformed input without changing the supplied source" $ do
    let original = malformedSource
    parsePrintModule staticDefaultConfig (Text.pack original) >>= \case
      Left _ -> pure ()
      Right output -> Hspec.expectationFailure
        $ "malformed input formatted as: " ++ Text.unpack output
    original `Hspec.shouldBe` malformedSource

assertStable :: FilePath -> String -> String -> Hspec.Expectation
assertStable filename original firstPass = do
  secondPass <- formatChecked firstPass
  secondPass `Hspec.shouldBe` firstPass
  originalFingerprint <- commentFingerprint filename original
  formattedFingerprint <- commentFingerprint filename firstPass
  formattedFingerprint `Hspec.shouldBe` originalFingerprint
  firstStructuralFingerprint <- structuralFingerprint filename firstPass
  secondStructuralFingerprint <- structuralFingerprint filename secondPass
  secondStructuralFingerprint `Hspec.shouldBe` firstStructuralFingerprint

formatChecked :: String -> IO String
formatChecked source =
  parsePrintModule staticDefaultConfig (Text.pack source) >>= \case
    Left errors -> Hspec.expectationFailure
      ("formatting returned " ++ show (length errors) ++ " errors")
      >> fail "formatting failed"
    Right output -> pure $ Text.unpack output

commentFingerprint
  :: FilePath
  -> String
  -> IO [(Text.Text, SourceCommentSyntax, CommentRole, String)]
commentFingerprint filename source = do
  plan <- parsedCommentPlan filename source
  pure $ commentPlanFingerprint plan

structuralFingerprint
  :: FilePath
  -> String
  -> IO
       [ ( Text.Text
         , SourceCommentSyntax
         , CommentRole
         , CommentAnchor
         , CommentLineRelation
         , String
         )
       ]
structuralFingerprint filename source = do
  plan <- parsedCommentPlan filename source
  pure $ commentPlanStructuralFingerprint plan

parsedCommentPlan :: FilePath -> String -> IO CommentPlan
parsedCommentPlan filename source = do
  parsed <- ParseModule.parseModule ["-haddock"] filename
    (const $ pure $ Right ()) source
  case parsed of
    Left parseError -> Hspec.expectationFailure parseError >> fail parseError
    Right (annotations, _, ()) -> case normalizeCommentPlan annotations of
      Left errors -> Hspec.expectationFailure (show errors) >> fail "invalid plan"
      Right plan -> pure plan

commentLines :: String -> [Text.Text]
commentLines = fmap Text.strip
  . filter hasComment
  . Text.lines
  . Text.pack
 where
  commentMarkers = Text.pack <$> ["--", "{-"]
  hasComment line = any (`Text.isInfixOf` line) commentMarkers

inlineSource :: String
inlineSource = unlines
  [ "module InlineComments where"
  , ""
  , "action input = do   -- after do"
  , "  let -- after let"
  , "    result =   -- after equals"
  , "      input"
  , "  pure result   -- after application"
  , ""
  , "identity value =   -- after function equals"
  , "  value"
  , ""
  , "positions values ="
  , "  let"
  , "    (_, result) =   -- disabled tuple binding"
  , "      (0, values)"
  , "  in result"
  , ""
  , "apply = \\value ->   -- after arrow"
  , "  value"
  , ""
  , "unwrap input = case input of"
  , "  Just value ->   -- after case arrow"
  , "    value"
  , "  Nothing -> 0"
  , ""
  , "unwrapLong input = case input of"
  , "  VeryLongConstructorName first second third fourth fifth sixth ->   -- after wrapped case arrow"
  , "    first"
  ]

equationSource :: String
equationSource = unlines
  [ "module EquationComments where"
  , ""
  , "choose [] = 0 -- terminal case"
  , "-- The non-empty case keeps the head."
  , "-- It deliberately ignores the tail."
  , "choose (value : _) = value"
  ]

lambdaOwnLineSource :: String
lambdaOwnLineSource = unlines
  [ "module LambdaOwnLineComments where"
  , ""
  , "apply = \\input ->"
  , "  -- first lambda note"
  , "  -- second lambda note"
  , "  case input of"
  , "    Just value -> value"
  , "    Nothing -> 0"
  ]

listComprehensionSource :: String
listComprehensionSource = unlines
  [ "module ListComprehensionComments where"
  , ""
  , "select values ="
  , "  [ -- result note"
  , "    value"
  , "  | value <- values"
  , "  ]"
  ]

mixedSource :: String
mixedSource = unlines
  [ "module MixedComments where"
  , ""
  , "data Box = Box"
  , "  { field :: Int"
  , "             -- ^ retained Haddock"
  , "  }"
  , ""
  , "value input ="
  , "  let"
  , "    -- first nested note"
  , "    -- second nested note"
  , "    result = input {- retained block -}"
  , "  in result"
  ]

malformedSource :: String
malformedSource = unlines
  [ "module MalformedComments where"
  , "value = do -- retained on failure"
  , "  let ="
  ]
