{-# LANGUAGE LambdaCase #-}

module TrailingCommentContinuationSpec (spec) where

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
spec = Hspec.describe "trailing comment continuations" $ do
  Hspec.it "aligns every continuation in the issue #186 reproducer" $ do
    let source = continuationSource 3 followingDeclaration
    output <- formatChecked (configWithIndent 2) source
    output `Hspec.shouldBe` List.intercalate "\n"
      (declarationLines ++ map (replicate 39 ' ' ++) continuationComments
        ++ followingDeclaration)
    assertStableAndEquivalent (configWithIndent 2) source output

  Hspec.it "keeps the self-hosted BackendUtils comment run together" $ do
    output <- formatChecked (configWithIndent 2) selfHostedSource
    assertCommentAlignment "-- this always sets to" selfHostedComments output
    assertStableAndEquivalent (configWithIndent 2) selfHostedSource output

  forM_ [2, 4] $ \indent -> forM_ [1, 3] $ \count ->
    Hspec.it
      ("aligns " ++ show count ++ " continuation lines at indent " ++ show indent)
      $ do
        let suffix = if count == 1
              then followingDeclaration
              else "" : "next :: value -> value" : ["next value = value"]
            source = continuationSource count suffix
            config = configWithIndent indent
        output <- formatChecked config source
        assertCommentAlignment "-- this always sets to"
          (take count continuationComments) output
        assertStableAndEquivalent config source output

  Hspec.it "aligns a trailing run at EOF without a following declaration" $ do
    let source = continuationSource 3 []
        config = configWithIndent 4
    output <- formatChecked config source
    assertCommentAlignment "-- this always sets to" continuationComments output
    assertStableAndEquivalent config source output

  Hspec.it "leaves separate ordinary and Haddock leading comments at the next declaration" $ do
    let source = continuationSource 1
          [ ""
          , "-- A separate note about next."
          , "-- | Documentation for next."
          , "next :: value -> value"
          , "next value = value"
          ]
        config = configWithIndent 2
    output <- formatChecked config source
    assertCommentAlignment "-- this always sets to"
      (take 1 continuationComments) output
    output `Hspec.shouldContain`
      "\n\n-- A separate note about next.\n-- | Documentation for next.\nnext ::"
    assertStableAndEquivalent config source output

  Hspec.it "preserves the source column of the first merged prior comment" $ do
    let source = unlines
          [ "module SeparateLeadingComment where"
          , ""
          , "first = 1"
          , ""
          , "       -- separate leading note"
          , "next = 2"
          ]
        config = configWithIndent 2
    output <- formatChecked config source
    output `Hspec.shouldContain` "\n       -- separate leading note\nnext = 2"
    assertStableAndEquivalent config source output

  Hspec.it "rejects malformed input without replacing its inplace source" $ do
    directory <- Directory.getTemporaryDirectory
    Exception.bracket
      (do
        (path, handle) <- IO.openTempFile directory "brittany-trailing-invalid.hs"
        IO.hPutStr handle malformedSource
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
        TextIO.readFile path `Hspec.shouldReturn` Text.pack malformedSource

  Hspec.it "rejects an ambiguous continuation owner before rendering" $ do
    let comment = EP.Comment Nothing (sourceSpan 3 39 3 61) "-- shared continuation"
        annotation = EP.Ann
          { EP.annCapturedSpan = Nothing
          , EP.annSortKey = Nothing
          , EP.annsDP = []
          , EP.annFollowingComments = []
          , EP.annPriorComments = [(comment, EP.DP (1, 38))]
          , EP.annEntryDelta = EP.DP (0, 0)
          }
        annotations = Map.fromList
          [ (EP.AnnKey [sourceSpan 2 1 2 8] $ EP.CN "Previous", annotation)
          , (EP.AnnKey [sourceSpan 5 1 5 8] $ EP.CN "Next", annotation)
          ]
    normalizeCommentPlan annotations `Hspec.shouldSatisfy` \case
      Left [AmbiguousCommentOwnership _ owners] -> length owners == 2
      _ -> False

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
    parsed <- ParseModule.parseModule ["-haddock"] "TrailingContinuation.hs"
      (const $ pure $ Right ()) source
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError >> fail parseError
      Right result -> pure result

assertCommentAlignment :: String -> [String] -> String -> IO ()
assertCommentAlignment seed continuations source = do
  seedColumn <- columnOf seed source
  seedColumn `Hspec.shouldSatisfy` (> 0)
  forM_ continuations $ \comment -> do
    continuationColumn <- columnOf comment source
    continuationColumn `Hspec.shouldBe` seedColumn
 where
  columnOf needle input = case
      [ length prefix
      | line <- lines input
      , (prefix, suffix) <- zip (List.inits line) (List.tails line)
      , needle `List.isPrefixOf` suffix
      ] of
    [column] -> pure column
    matches -> Hspec.expectationFailure
      ("expected exactly one " ++ show needle ++ ", found " ++ show matches)
      >> fail "missing or duplicated comment"

configWithIndent :: Int -> Config
configWithIndent indent = staticDefaultConfig
  { _conf_layout = (_conf_layout staticDefaultConfig)
      { _lconfig_cols = Identity $ Last 80
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
  file = FastString.fsLit "AmbiguousContinuation.hs"

continuationSource :: Int -> [String] -> String
continuationSource count suffix = unlines
  (declarationLines
    ++ map (replicate 38 ' ' ++) (take count continuationComments)
    ++ suffix)

declarationLines :: [String]
declarationLines =
  [ "module TrailingContinuation where"
  , ""
  , "update state diff = do"
  , "  when (diff > 0) $ do"
  , "    mSet $ state { value = Just diff } -- this always sets to"
  ]

continuationComments :: [String]
continuationComments =
  [ "-- at least (Just 1), so we will not"
  , "-- overwrite an old value in any"
  , "-- bad way."
  ]

followingDeclaration :: [String]
followingDeclaration = ["", "next value = value"]

selfHostedSource :: String
selfHostedSource = unlines $
  [ "module BackendUtilsComment where"
  , ""
  , "layoutWriteEnsureAbsoluteN n = do"
  , "  state <- mGet"
  , "  let"
  , "    diff = case (_lstate_commentCol state, _lstate_curYOrAddNewline state) of"
  , "      (Just c, _) -> n - c"
  , "      (Nothing, Left i) -> n - i"
  , "      (Nothing, Right{}) -> n"
  , "  traceLocal (\"layoutWriteEnsureAbsoluteN\", n, diff)"
  , "  when (diff > 0) $ do"
  , "    mSet $ state { _lstate_addSepSpace = Just diff } -- this always sets to"
  ] ++ map (replicate 44 ' ' ++) selfHostedComments ++
  [ ""
  , "layoutBaseYPushInternal :: Int -> Int"
  , "layoutBaseYPushInternal i = i"
  ]

selfHostedComments :: [String]
selfHostedComments =
  [ "-- at least (Just 1), so we won't"
  , "-- overwrite any old value in any"
  , "-- bad way."
  ]

malformedSource :: String
malformedSource = continuationSource 3 ["", "next value = ("]
