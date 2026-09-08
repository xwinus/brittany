{-# LANGUAGE LambdaCase #-}

module ConstructorCommentContinuationSpec (spec) where

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
spec = Hspec.describe "constructor comment continuations" $ do
  Hspec.it "aligns the ordinary continuation in the issue #187 reproducer" $ do
    output <- formatChecked defaultConfig minimalSource
    output `Hspec.shouldBe` List.intercalate "\n"
      [ "module ConstructorComment where"
      , ""
      , "data IndentPolicy"
      , "  = IndentPolicyLeft  -- never create a new indentation at more"
      , replicate 22 ' ' ++ "-- than old indentation + amount"
      , "  | IndentPolicyFree  -- can create new indentations wherever"
      ]
    assertStableAndEquivalent defaultConfig minimalSource output

  Hspec.it "keeps both self-hosted IndentPolicy runs aligned before deriving" $ do
    output <- formatChecked defaultConfig selfHostedSource
    assertCommentAlignment "-- never create a new indentation at more"
      ["-- than old indentation + amount"] output
    assertCommentAlignment "-- can create indentations only"
      ["-- at any n * amount."] output
    output `Hspec.shouldContain` "\n  deriving (Eq, Show)"
    assertStableAndEquivalent defaultConfig selfHostedSource output

  Hspec.it "aligns ordinary continuations between GADT constructors" $ do
    let source = unlines
          [ "{-# LANGUAGE GADTs #-}"
          , "module GadtContinuation where"
          , ""
          , "data Choice where"
          , "  First :: Int -> Choice -- first constructor note"
          , replicate 25 ' ' ++ "-- first continuation"
          , replicate 25 ' ' ++ "-- second continuation"
          , "  Second :: Choice"
          ]
    output <- formatChecked defaultConfig source
    assertCommentAlignment "-- first constructor note"
      ["-- first continuation", "-- second continuation"] output
    assertStableAndEquivalent defaultConfig source output

  Hspec.it "keeps a final constructor run before the next declaration" $ do
    let source = unlines
          [ "module FinalConstructorBeforeDeclaration where"
          , ""
          , "data Choice = Only -- final constructor note"
          , replicate 20 ' ' ++ "-- final continuation"
          , ""
          , "next = Only"
          ]
    output <- formatChecked defaultConfig source
    assertCommentAlignment "-- final constructor note"
      ["-- final continuation"] output
    output `Hspec.shouldContain` "\n\nnext = Only"
    assertStableAndEquivalent defaultConfig source output

  Hspec.it "aligns a final constructor run at EOF" $ do
    let source = unlines
          [ "module FinalConstructor where"
          , ""
          , "data Choice = Only -- final constructor note"
          , replicate 20 ' ' ++ "-- final continuation"
          ]
    output <- formatChecked defaultConfig source
    assertCommentAlignment "-- final constructor note"
      ["-- final continuation"] output
    assertStableAndEquivalent defaultConfig source output

  forM_ [2, 4] $ \indent -> forM_ [44, 80] $ \columns ->
    Hspec.it
      ("keeps a run together at indent " ++ show indent
        ++ " and width " ++ show columns) $ do
        let config = configWithLayout indent columns
        output <- formatChecked config widthPressureSource
        assertCommentAlignment "-- constructor note"
          ["-- continuation one", "-- continuation two"] output
        assertStableAndEquivalent config widthPressureSource output

  Hspec.it "keeps separated ordinary and Haddock comments with the next constructor" $ do
    let source = unlines
          [ "module SeparateConstructorComments where"
          , ""
          , "data Choice"
          , "  = First -- first constructor note"
          , "          -- continuation of first"
          , ""
          , "  -- A separate ordinary note."
          , "  -- | Documentation for Second."
          , "  | Second"
          , "  -- ^ Second post-documentation."
          , "  deriving (Eq, Show)"
          ]
    output <- formatChecked defaultConfig source
    assertCommentAlignment "-- first constructor note"
      ["-- continuation of first"] output
    output `Hspec.shouldContain`
      "  -- A separate ordinary note.\n  -- | Documentation for Second.\n  | Second"
    assertStableAndEquivalent defaultConfig source output

  Hspec.it "anchors each constructor boundary to its own trailing seed" $ do
    let source = unlines
          [ "module DistinctConstructorRuns where"
          , ""
          , "data Choice"
          , "  = First -- first constructor note"
          , "          -- first continuation"
          , "  | SecondConstructor -- second constructor note"
          , "                      -- second continuation"
          , "  | Third"
          ]
    output <- formatChecked defaultConfig source
    assertCommentAlignment "-- first constructor note"
      ["-- first continuation"] output
    assertCommentAlignment "-- second constructor note"
      ["-- second continuation"] output
    assertStableAndEquivalent defaultConfig source output

  Hspec.it "stops a constructor run at a blank source line" $ do
    let source = unlines
          [ "module BlankConstructorComment where"
          , ""
          , "data Choice"
          , "  = First -- first constructor note"
          , "          -- first continuation"
          , ""
          , "          -- Separate note for Second."
          , "  | Second"
          ]
    output <- formatChecked defaultConfig source
    assertCommentAlignment "-- first constructor note"
      ["-- first continuation"] output
    output `Hspec.shouldContain` "\n  -- Separate note for Second.\n  | Second"
    assertStableAndEquivalent defaultConfig source output

  Hspec.it "stops a constructor run when continuation source columns change" $ do
    let source = unlines
          [ "module ChangedConstructorCommentColumn where"
          , ""
          , "data Choice"
          , "  = First -- first constructor note"
          , "          -- first continuation"
          , "            -- Separate note for Second."
          , "  | Second"
          ]
    output <- formatChecked defaultConfig source
    assertCommentAlignment "-- first constructor note"
      ["-- first continuation"] output
    output `Hspec.shouldContain` "\n  -- Separate note for Second.\n  | Second"
    assertStableAndEquivalent defaultConfig source output

  Hspec.it "does not absorb an adjacent dedented leading comment" $ do
    let source = unlines
          [ "module LeadingConstructorComment where"
          , ""
          , "data Choice"
          , "  = First -- first constructor note"
          , "  -- This note introduces Second."
          , "  | Second"
          ]
    output <- formatChecked defaultConfig source
    output `Hspec.shouldContain`
      "\n  -- This note introduces Second.\n  | Second"
    assertStableAndEquivalent defaultConfig source output

  Hspec.it "rejects malformed constructors without replacing their inplace source" $ do
    directory <- Directory.getTemporaryDirectory
    Exception.bracket
      (do
        (path, handle) <- IO.openTempFile directory "brittany-constructor-invalid.hs"
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

  Hspec.it "rejects an ambiguous constructor continuation owner before rendering" $ do
    let comment = EP.Comment Nothing (sourceSpan 3 20 3 42) "-- shared continuation"
        annotation = EP.Ann
          { EP.annCapturedSpan = Nothing
          , EP.annSortKey = Nothing
          , EP.annsDP = []
          , EP.annFollowingComments = []
          , EP.annPriorComments = [(comment, EP.DP (1, 19))]
          , EP.annEntryDelta = EP.DP (0, 0)
          }
        annotations = Map.fromList
          [ (EP.AnnKey [sourceSpan 2 3 2 8] $ EP.CN "ConDeclH98", annotation)
          , (EP.AnnKey [sourceSpan 4 3 4 9] $ EP.CN "ConDeclH98", annotation)
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
    parsed <- ParseModule.parseModule ["-haddock"] "ConstructorContinuation.hs"
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

defaultConfig :: Config
defaultConfig = configWithLayout 2 80

configWithLayout :: Int -> Int -> Config
configWithLayout indent columns = staticDefaultConfig
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
  file = FastString.fsLit "AmbiguousConstructorContinuation.hs"

minimalSource :: String
minimalSource = unlines
  [ "module ConstructorComment where"
  , ""
  , "data IndentPolicy = IndentPolicyLeft -- never create a new indentation at more"
  , replicate 37 ' ' ++ "-- than old indentation + amount"
  , "                  | IndentPolicyFree -- can create new indentations wherever"
  ]

selfHostedSource :: String
selfHostedSource = unlines
  [ "module IndentPolicyComment where"
  , ""
  , "data IndentPolicy = IndentPolicyLeft -- never create a new indentation at more"
  , replicate 37 ' ' ++ "-- than old indentation + amount"
  , "                  | IndentPolicyFree -- can create new indentations wherever"
  , "                  | IndentPolicyMultiple -- can create indentations only"
  , replicate 41 ' ' ++ "-- at any n * amount."
  , "  deriving (Eq, Show)"
  ]

widthPressureSource :: String
widthPressureSource = unlines
  [ "module ConstructorWidth where"
  , ""
  , "data Choice = FirstConstructorWithLongName Int Bool -- constructor note"
  , replicate 52 ' ' ++ "-- continuation one"
  , replicate 52 ' ' ++ "-- continuation two"
  , "            | SecondConstructor"
  , "  deriving (Eq, Show)"
  ]

malformedSource :: String
malformedSource = unlines
  [ "module MalformedConstructor where"
  , ""
  , "data Choice = First -- first constructor note"
  , "                    -- continuation of first"
  , "            | Second ("
  ]
