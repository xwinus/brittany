{-# LANGUAGE LambdaCase #-}

module LocalTrailingCommentSpec (spec) where

import qualified Control.Exception as Exception
import Control.Monad (forM_)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Semigroup (Last(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified GHC.Data.FastString as FastString
import qualified GHC.Types.SrcLoc as SrcLoc
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
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as EP
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SemanticFingerprint
  ( compareSemanticSyntax
  )
import Language.Haskell.Brittany.Internal.SourceComment.Types
  ( CommentPlanError(..)
  )
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "local trailing comments" $ do
  Hspec.it "keeps the issue #190 branch comment and the following Haddock apart" $ do
    output <- checkFormatting 80 2 $ localSource shortBranch documentedHelper
    output `Hspec.shouldBe` List.intercalate "\n"
      [ "module LocalTrailingComment where"
      , ""
      , "outer input ="
      , "  classify input"
      , " where"
      , "  classify value = case value of"
      , "    Just item -> ([item], [])"
      , "    Nothing   -> ([], [])  -- final branch"
      , ""
      , "  -- | Build a child annotation."
      , "  buildChildAnn value = value"
      ]

  Hspec.it "retains the self-hosted classifyByKeywords trailing comment" $ do
    output <- checkFormatting 80 2 selfHostedSource
    assertAttached "-- no else → all go to then" "Nothing" output
    assertBlankBefore "-- | Build a child annotation with redistributed comments" output
    output `Hspec.shouldContain`
      "-- position (e.g., \"then\" keyword for then-expression comments), so that"

  forM_ [2, 4] $ \indent -> forM_ [40, 80] $ \columns ->
    Hspec.it
      ("keeps a multiline branch comment at width " ++ show columns
        ++ " and indent " ++ show indent) $ do
        output <- checkFormatting columns indent $
          localSource longBranch documentedHelper
        assertAttached "-- final branch" "Nothing" output
        branchIndex <- lineIndex "Nothing" output
        commentIndex <- lineIndex "-- final branch" output
        commentIndex `Hspec.shouldSatisfy` (> branchIndex)
        assertBlankBefore "-- | Build a child annotation." output

  forM_ helperVariants $ \(description, helper) ->
    Hspec.it ("preserves branch ownership before " ++ description) $ do
      output <- checkFormatting 80 2 $ localSource shortBranch helper
      assertAttached "-- final branch" "Nothing" output
      case filter (List.isInfixOf "--") helper of
        comment : _ -> assertBlankBefore (dropWhile (== ' ') comment) output
        [] -> assertBlankBefore "buildChildAnn value" output

  Hspec.it "preserves blank lines between distinct leading comment blocks" $ do
    let helper =
          [ "    -- A separate implementation note."
          , ""
          , "    -- Another independent note."
          , ""
          , "    -- | Build a child annotation."
          , "    buildChildAnn value = value"
          ]
    output <- checkFormatting 80 2 $ localSource shortBranch helper
    assertAttached "-- final branch" "Nothing" output
    forM_
      [ "-- A separate implementation note."
      , "-- Another independent note."
      , "-- | Build a child annotation."
      ] $ \comment -> assertBlankBefore comment output

  Hspec.it "leaves an adjacent leading comment with the following helper" $ do
    let source = unlines $
          take 7 (lines $ localSource shortBranch []) ++
          [ "    -- Documentation for buildChildAnn."
          , "    buildChildAnn value = value"
          ]
    output <- checkFormatting 80 2 source
    assertAttached "-- final branch" "Nothing" output
    output `Hspec.shouldContain`
      "\n  -- Documentation for buildChildAnn.\n  buildChildAnn value = value"
    output `Hspec.shouldNotContain` "-- final branch\n\n"

  Hspec.it "preserves one blank line after a trailing block comment" $ do
    output <- checkFormatting 80 2 $ localSource
      "      Nothing -> ([], [])  {- final branch -}" documentedHelper
    assertAttached "{- final branch -}" "Nothing" output
    assertBlankBefore "-- | Build a child annotation." output
    output `Hspec.shouldNotContain` "\n\n\n"

  Hspec.it "keeps a branch comment in a let declaration group" $ do
    let source = unlines
          [ "module LocalTrailingComment where"
          , ""
          , "outer input ="
          , "  let classify value = case value of"
          , "        Just item -> ([item], [])"
          , "        Nothing -> ([], [])  -- final branch"
          , ""
          , "      -- | Build a child annotation."
          , "      buildChildAnn value = value"
          , "  in classify input"
          ]
    output <- checkFormatting 80 2 source
    assertAttached "-- final branch" "Nothing" output
    assertBlankBefore "-- | Build a child annotation." output

  Hspec.it "rejects malformed local declarations without replacing inplace input" $ do
    let source = localSource shortBranch ["    buildChildAnn value = ("]
    directory <- Directory.getTemporaryDirectory
    Exception.bracket
      (do
        (path, handle) <- IO.openTempFile directory "brittany-local-comment-invalid.hs"
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

  Hspec.it "rejects duplicate local comment ownership before rendering" $ do
    let comment = EP.Comment Nothing (sourceSpan 7 28 7 43) "-- final branch"
        annotation = EP.Ann
          { EP.annCapturedSpan = Nothing
          , EP.annSortKey = Nothing
          , EP.annsDP = []
          , EP.annFollowingComments = [(comment, EP.DP (0, 2))]
          , EP.annPriorComments = []
          , EP.annEntryDelta = EP.DP (0, 0)
          }
        annotations = Map.fromList
          [ (EP.AnnKey [sourceSpan 5 5 7 26] $ EP.CN "FunBind", annotation)
          , (EP.AnnKey [sourceSpan 10 5 10 32] $ EP.CN "FunBind", annotation)
          ]
    normalizeCommentPlan annotations `Hspec.shouldSatisfy` \case
      Left [AmbiguousCommentOwnership _ owners] -> length owners == 2
      _ -> False

checkFormatting :: Int -> Int -> String -> IO String
checkFormatting columns indent source = do
  let config = configWithLayout columns indent
  output <- formatChecked config source
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
    parsed <- ParseModule.parseModule ["-haddock"] "LocalTrailingComment.hs"
      (const $ pure $ Right ()) source
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError >> fail parseError
      Right result -> pure result

assertAttached :: String -> String -> String -> IO ()
assertAttached comment branch source = do
  commentIndex <- lineIndex comment source
  branchIndex <- lineIndex branch source
  helperIndex <- lineIndex "buildChildAnn value" source
  let sourceLines = lines source
      commentLine = sourceLines !! commentIndex
      commentPrefix = takeWhile (/= '-') commentLine
      helperIndent = length $ takeWhile (== ' ') $ sourceLines !! helperIndex
      commentIndent = length $ takeWhile (== ' ') commentLine
  commentIndex `Hspec.shouldSatisfy` (>= branchIndex)
  commentIndex `Hspec.shouldSatisfy` (< helperIndex)
  (any (/= ' ') commentPrefix || commentIndent > helperIndent)
    `Hspec.shouldBe` True

assertBlankBefore :: String -> String -> IO ()
assertBlankBefore needle source = do
  index <- lineIndex needle source
  index `Hspec.shouldSatisfy` (> 0)
  (lines source !! (index - 1)) `Hspec.shouldBe` ""

lineIndex :: String -> String -> IO Int
lineIndex needle source = case
    [index | (index, line) <- zip [0 ..] $ lines source, needle `List.isInfixOf` line] of
  [index] -> pure index
  matches -> Hspec.expectationFailure
    ("expected one line containing " ++ show needle ++ ", found " ++ show matches)
    >> fail "missing or duplicated marker"

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

sourceSpan :: Int -> Int -> Int -> Int -> SrcLoc.SrcSpan
sourceSpan startLine startColumn endLine endColumn =
  EP.realSpanToSrcSpan $ SrcLoc.mkRealSrcSpan
    (SrcLoc.mkRealSrcLoc file startLine startColumn)
    (SrcLoc.mkRealSrcLoc file endLine endColumn)
 where
  file = FastString.fsLit "AmbiguousLocalComment.hs"

localSource :: String -> [String] -> String
localSource branch helper = unlines $
  [ "module LocalTrailingComment where"
  , ""
  , "outer input = classify input"
  , "  where"
  , "    classify value = case value of"
  , "      Just item -> ([item], [])"
  , branch
  , ""
  ] ++ helper

shortBranch :: String
shortBranch = "      Nothing -> ([], [])  -- final branch"

longBranch :: String
longBranch = "      Nothing -> (makeValue firstArgument secondArgument thirdArgument, "
  ++ "anotherValue fourthArgument fifthArgument sixthArgument)  -- final branch"

documentedHelper :: [String]
documentedHelper =
  [ "    -- | Build a child annotation."
  , "    buildChildAnn value = value"
  ]

helperVariants :: [(String, [String])]
helperVariants =
  [ ("an ordinary leading comment",
      ["    -- Build a child annotation.", "    buildChildAnn value = value"])
  , ("a documented type signature",
      [ "    -- | Build a child annotation."
      , "    buildChildAnn :: value -> value"
      , "    buildChildAnn value = value"
      ])
  , ("an undocumented helper", ["    buildChildAnn value = value"])
  ]

selfHostedSource :: String
selfHostedSource = unlines
  [ "module LocalTrailingComment where"
  , ""
  , "import qualified Data.List as List"
  , ""
  , "outer thenPos elsePos coms = classifyByKeywords thenPos elsePos coms"
  , "  where"
  , "    -- | Classify inner comments: before elsePos → then-expression,"
  , "    -- at/after elsePos → else-expression"
  , "    classifyByKeywords"
  , "      :: Maybe (Int, Int) -> Maybe (Int, Int)"
  , "      -> [((Int, Int), (String, RealSrcSpan))]"
  , "      -> ([((Int, Int), (String, RealSrcSpan))], [((Int, Int), (String, RealSrcSpan))])"
  , "    classifyByKeywords _thenPos elsePos coms = case elsePos of"
  , "      Just ep -> List.partition (\\((line, _), _) -> line < fst ep) coms"
  , "      Nothing -> (coms, [])  -- no else → all go to then"
  , ""
  , "    -- | Build a child annotation with redistributed comments as prior comments."
  , "    -- The DP for each comment is computed relative to the preceding keyword"
  , "    -- position (e.g., \"then\" keyword for then-expression comments), so that"
  , "    -- the comment gets placed on a new line at the correct column."
  , "    buildChildAnn value = value"
  ]
