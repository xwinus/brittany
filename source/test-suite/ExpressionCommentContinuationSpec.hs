{-# LANGUAGE LambdaCase #-}

module ExpressionCommentContinuationSpec (spec) where

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
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "expression comment continuations" $ do
  forM_ maintainedRuns $ \(path, runs) ->
    Hspec.it ("aligns self-hosted comment runs in " ++ path) $ do
      source <- readFile $ projectRoot </> "source/library" </> path
      output <- formatChecked defaultConfig source
      forM_ runs $ \(seed, comments) -> assertCommentAlignment seed comments output
      if path == internalPath "Types.hs"
        then do
          declaration <- uniqueLine "-- like a \"last\" of indLevel. Used for" output
          declaration `Hspec.shouldContain` "_lstate_indLevelLinger"
          declaration `Hspec.shouldContain` ":: Int"
          length declaration `Hspec.shouldSatisfy` (<= 80)
        else pure ()
      assertStableAndEquivalent defaultConfig source output

  forM_ [2, 4] $ \indent -> forM_ [40, 80] $ \columns ->
    forM_ expressionContexts $ \(context, source) ->
      Hspec.it ("aligns " ++ context ++ " at indent " ++ show indent
        ++ " and width " ++ show columns) $ do
        let config = configWithLayout indent columns
        output <- formatChecked config source
        assertCommentAlignment "-- seed note"
          ["-- continuation one", "-- continuation two"] output
        assertStableAndEquivalent config source output

  forM_ [2, 4] $ \indent -> forM_ [40, 80] $ \columns ->
    forM_ [False, True] $ \overflows ->
      Hspec.it ((if overflows then "breaks a field one column over "
        else "keeps a field exactly at ") ++ show columns
        ++ " columns with indent " ++ show indent) $ do
        let prefix = replicate indent ' ' ++ "{ field :: Int -- seed "
            compact = prefix ++ replicate
              (columns - length prefix + if overflows then 1 else 0) 'x'
            source = moduleSource
              (["data Example = Example", compact]
                ++ [replicate (length $ takeWhile (/= '-') compact) ' '
                    ++ "-- continuation", replicate indent ' ' ++ "}"])
            config = configWithLayout indent columns
        output <- formatChecked config source
        fieldLine <- uniqueLine "{ field" output
        if overflows
          then fieldLine `Hspec.shouldNotContain` ":: Int"
          else do
            fieldLine `Hspec.shouldContain` ":: Int -- seed "
            length fieldLine `Hspec.shouldBe` columns
        assertCommentAlignment "-- seed " ["-- continuation"] output
        assertStableAndEquivalent config source output

  forM_ [2, 4] $ \indent -> forM_ [False, True] $ \subsequent ->
    Hspec.it ("keeps a commented long " ++ (if subsequent then "subsequent" else "first")
      ++ " field type multiline with indent " ++ show indent) $ do
      let prefix = if subsequent
            then ["  { initial :: Bool"]
            else []
          field = (if subsequent then "  , " else "  { ")
            ++ "field :: LongArgumentTypeName -> LongResultTypeName"
          source = moduleSource
            (["data Example = Example"] ++ prefix
              ++ commented field ["-- seed note", "-- continuation"] ++ ["  }"])
          config = configWithLayout indent 50
      output <- formatChecked config source
      fieldLine <- uniqueLine "field" output
      fieldLine `Hspec.shouldNotContain` "::"
      output `Hspec.shouldContain` "-> LongResultTypeName"
      maximum (map length $ lines output) `Hspec.shouldSatisfy` (<= 50)
      assertCommentAlignment "-- seed note" ["-- continuation"] output
      assertStableAndEquivalent config source output

  forM_ [2, 4] $ \indent ->
    Hspec.it ("keeps fitting commented fields within width despite longer siblings at indent "
      ++ show indent) $ do
      let prefix = replicate indent ' ' ++ ", field :: Int -- seed "
          compact = prefix ++ replicate (80 - length prefix) 'x'
          source = moduleSource
            [ "data Example = Example"
            , replicate indent ' ' ++ "{ considerablyLongerSiblingFieldName :: Bool"
            , compact
            , replicate (length $ takeWhile (/= '-') compact) ' '
                ++ "-- continuation"
            , replicate indent ' ' ++ "}"]
          config = configWithLayout indent 80
      output <- formatChecked config source
      fieldLine <- uniqueLine "-- seed " output
      fieldLine `Hspec.shouldContain` "field :: Int"
      maximum (map length $ lines output) `Hspec.shouldSatisfy` (<= 80)
      assertCommentAlignment "-- seed " ["-- continuation"] output
      assertStableAndEquivalent config source output

  Hspec.it "preserves indentation inside a continuation comment's text" $ do
    let source = moduleSource
          (["data Example = Example"]
            ++ commented "  { field :: Int"
              ["-- seed note", "--   nested detail", "-- final detail"]
            ++ ["  }"])
    output <- formatChecked defaultConfig source
    assertCommentAlignment "-- seed note"
      ["--   nested detail", "-- final detail"] output
    assertStableAndEquivalent defaultConfig source output

  Hspec.it "preserves increased marker indentation and its return to the run baseline" $ do
    let source = moduleSource
          [ "data Example = Example"
          , "  { field :: Int -- seed note"
          , "                   -- first continuation"
          , "                     -- nested continuation"
          , "                   -- final continuation"
          , "  }"]
    output <- formatChecked defaultConfig source
    assertCommentAlignment "-- seed note"
      ["-- first continuation", "-- final continuation"] output
    firstLine <- uniqueLine "-- first continuation" output
    nestedLine <- uniqueLine "-- nested continuation" output
    length (takeWhile (== ' ') nestedLine)
      `Hspec.shouldBe` (length (takeWhile (== ' ') firstLine) + 2)
    assertStableAndEquivalent defaultConfig source output

  Hspec.it "keeps blank-separated leading and Haddock comments at the next field" $ do
    let source = moduleSource
          (["data Example = Example"]
            ++ commented "  { field :: Int" ["-- seed note", "-- continuation"]
            ++ [ ""
               , "  -- Separate note for next."
               , "  -- | Documentation for next."
               , "  , next :: Bool"
               , "  }"])
    output <- formatChecked defaultConfig source
    assertCommentAlignment "-- seed note" ["-- continuation"] output
    output `Hspec.shouldContain`
      "-- Separate note for next.\n  -- | Documentation for next.\n  , next"
    assertStableAndEquivalent defaultConfig source output

  Hspec.it "does not absorb a dedented leading comment after a local equation" $ do
    let source = moduleSource
          [ "example = do"
          , "  let first = 1 -- seed note"
          , "      -- This note introduces next."
          , "      next = 2"
          , "  pure (first, next)"
          ]
    output <- formatChecked defaultConfig source
    leading <- uniqueLine "-- This note introduces next." output
    binding <- uniqueLine "next = 2" output
    length (takeWhile (== ' ') leading)
      `Hspec.shouldBe` length (takeWhile (== ' ') binding)
    assertStableAndEquivalent defaultConfig source output

  Hspec.it "preserves an intervening block comment and following separate note" $ do
    let source = moduleSource
          (["data Example = Example"]
            ++ commented "  { field :: Int" ["-- seed note", "-- continuation"]
            ++ [ "  {- block note -}"
               , "  -- Separate note for next."
               , "  , next :: Bool"
               , "  }"])
    output <- formatChecked defaultConfig source
    assertCommentAlignment "-- seed note" ["-- continuation"] output
    blockLine <- uniqueLine "{- block note -}" output
    separateLine <- uniqueLine "-- Separate note for next." output
    takeWhile (== ' ') blockLine `Hspec.shouldBe` "  "
    takeWhile (== ' ') separateLine `Hspec.shouldBe` "  "
    assertStableAndEquivalent defaultConfig source output

  Hspec.it "rejects malformed input without replacing its inplace source" $ do
    let source = moduleSource
          ["data Example = Example { field :: Int -- seed note"
          , "                                    -- continuation"
          , ", next :: ("]
    directory <- Directory.getTemporaryDirectory
    Exception.bracket
      (do
        (path, handle) <- IO.openTempFile directory "brittany-expression-invalid.hs"
        IO.hPutStr handle source
        IO.hClose handle
        pure path)
      Directory.removeFile
      $ \path -> do
        Brittany.mainWith "brittany"
          ["--no-user-config", "--write-mode", "inplace", "--werror"
          , "--fail-on-fallback", path]
          `Hspec.shouldThrow` (== Exit.ExitFailure 60)
        TextIO.readFile path `Hspec.shouldReturn` Text.pack source

  Hspec.it "rejects ambiguous expression continuation ownership before rendering" $ do
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
          [(EP.AnnKey [sourceSpan 2 3 2 8] $ EP.CN "HsVar", annotation)
          , (EP.AnnKey [sourceSpan 4 3 4 9] $ EP.CN "HsVar", annotation)]
    normalizeCommentPlan annotations `Hspec.shouldSatisfy` \case
      Left [AmbiguousCommentOwnership _ owners] -> length owners == 2
      _ -> False

uniqueLine :: String -> String -> IO String
uniqueLine needle source = case filter (List.isInfixOf needle) $ lines source of
  [line] -> pure line
  matches -> Hspec.expectationFailure
    ("expected one line containing " ++ show needle ++ ", found " ++ show matches)
    >> fail "missing or duplicated line"

defaultConfig :: Config
defaultConfig = configWithLayout 2 80

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
    parsed <- ParseModule.parseModule ["-haddock"] "ExpressionContinuation.hs"
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
  file = FastString.fsLit "AmbiguousContinuation.hs"

moduleSource :: [String] -> String
moduleSource body = unlines $ ["module ExpressionContinuation where", ""] ++ body

commented :: String -> [String] -> [String]
commented line comments = case comments of
  [] -> [line]
  seed : continuations -> (line ++ " " ++ seed)
    : map (replicate (length line + 1) ' ' ++) continuations

expressionContexts :: [(String, String)]
expressionContexts = map (fmap moduleSource)
  [ ("nested do", ["example = do", "  let"]
      ++ commented "    parser = do" runComments
      ++ ["      action", "      pure value", "  parser"])
  , ("bind after arrow", ["example = do"]
      ++ commented "  value <-" runComments
      ++ ["    action", "  pure value"])
  , ("record value", ["example = Record"]
      ++ commented "  { field = Right 0" runComments
      ++ ["  , next = Nothing", "  }"])
  , ("local equation", ["example values = aggregate values", "  where"]
      ++ commented "    aggregate [] = 0" runComments
      ++ ["    aggregate xs = sum xs"])
  , ("case alternative", ["example value = case value of"]
      ++ commented "  Nothing -> error \"empty value\"" runComments
      ++ ["  Just x -> x"])
  , ("operator layout", ["example =", "  [ first value"]
      ++ commented "  , setSpacing" runComments
      ++ ["      $ addBase", "      $ sequenceDocs [firstDoc, secondDoc]", "  ]"])
  ]
 where
  runComments = ["-- seed note", "-- continuation one", "-- continuation two"]

internalPath :: FilePath -> FilePath
internalPath name = "Language/Haskell/Brittany/Internal" </> name

maintainedRuns :: [(FilePath, [(String, [String])])]
maintainedRuns =
  [ ("Language/Haskell/Brittany/Internal.hs",
      [ ("-- we will (mis?)use butcher here to parse the inline config", ["-- line."])
      , ("-- important that we dont use left",
          ["-- here because moveToAnn stuff", "-- of the first node needs to do"
          , "-- its thing properly."])
      ])
  , (internalPath "Backend.hs",
      [("-- this probably cannot happen the way we call",
        ["-- this function, because _cbs_map only ever", "-- contains nonempty Seqs."])])
  , (internalPath "Types.hs",
      [("-- like a \"last\" of indLevel. Used for",
        ["-- properly treating cases where comments"
        , "-- on the first indented element have an"
        , "-- annotation offset relative to the last"
        , "-- non-indented element, which is confusing."])])
  , (internalPath "Layouters/Expr.hs",
      [("-- this is most likely superfluous because",
        ["-- this is a sequence of a one-line and a par-space"
        , "-- anyways, so it is _always_ par-spaced."])])
  , (internalPath "Layouters/Pattern.hs",
      [("-- at the moment, we don't support splitting patterns into",
        ["-- multiple lines. but we cannot enforce pasting everything"
        , "-- into one line either, because the type signature will ignore"
        , "-- this if we overflow sufficiently."
        , "-- In order to prevent syntactically invalid results in such"
        , "-- cases, we need the AddBaseY here."
        , "-- This can all change when patterns get multiline support."])])
  , (internalPath "Config/Types.hs",
      [("-- use some special indentation for \",\"",
        ["-- when creating zero-indentation", "-- multi-line list literals."])])
  , (internalPath "Transformations/Alt.hs",
      [("-- returning BDEmpty instead is a",
        ["-- possibility, but i will prefer a"
        , "-- fail-early approach; BDEmpty does not"
        , "-- make sense semantically for Alt[]."])])
  ]
