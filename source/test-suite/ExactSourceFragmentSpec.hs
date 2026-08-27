module ExactSourceFragmentSpec (spec) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified GHC.Data.FastString as FastString
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.Backend
  ( consumeExactSourceFragment
  )
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as EP
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SourceComment.Types
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "exact-source comment ownership" $ do
  formattingExample projectRoot
    "preserves a Headroom-style final result post-doc"
    "ExactSourceFragmentInput.hs"
  formattingExample projectRoot
    "keeps identical block post-docs at their distinct source positions"
    "ExactSourceFragmentEdge.hs"

  Hspec.it "consumes only the comment key named by a valid fragment" $ do
    let (fragment, annotations, outsideComment) = fragmentFixture
    case consumeExactSourceFragment fragment annotations of
      Left fragmentError -> Hspec.expectationFailure fragmentError
      Right remaining -> do
        let comments = concatMap EP.annPriorComments $ Map.elems remaining
        comments `Hspec.shouldBe` [(outsideComment, EP.DP (1, 0))]

  Hspec.it "distinguishes equal comment text by source identity" $ do
    let (fragment, _, outsideComment) = fragmentFixture
        insideKeys = fragmentCommentKeys fragment
        outsideKey = SourceCommentKey $ EP.commentIdentifier outsideComment
    Set.size insideKeys `Hspec.shouldBe` 1
    outsideKey `Set.member` insideKeys `Hspec.shouldBe` False

  Hspec.it "rejects a comment key outside the fragment source range" $ do
    let (fragment, annotations, outsideComment) = fragmentFixture
        invalid = fragment
          { fragmentCommentKeys = Set.singleton
              $ SourceCommentKey
              $ EP.commentIdentifier outsideComment
          }
    consumeExactSourceFragment invalid annotations
      `Hspec.shouldSatisfy` isOutsideRangeError

formattingExample :: FilePath -> String -> FilePath -> Hspec.SpecWith ()
formattingExample projectRoot description fixtureName = Hspec.it description $ do
  let input = fixturePath projectRoot fixtureName
      output = FilePath.combine (FilePath.combine projectRoot "output") fixtureName
      args =
        [ "--config-file"
        , FilePath.combine projectRoot "data/brittany.yaml"
        , "--no-user-config"
        , "--columns"
        , "80"
        , "--indent"
        , "2"
        , "--write-mode"
        , "inplace"
        , output
        ]
  expected <- readFile input
  Directory.copyFile input output
  Brittany.mainWith "brittany" args
  firstPass <- readFile output
  firstPass `Hspec.shouldBe` expected
  commentLines firstPass `Hspec.shouldBe` commentLines expected
  parsed <- ParseModule.parseModule ["-haddock"] output
    (const $ pure $ Right ()) firstPass
  case parsed of
    Left parseError -> Hspec.expectationFailure parseError
    Right _ -> pure ()
  Brittany.mainWith "brittany" args
  readFile output `Hspec.shouldReturn` firstPass

fragmentFixture :: (ExactSourceFragment, EP.Anns, EP.Comment)
fragmentFixture = (fragment, Map.singleton annotationKey annotation, outside)
 where
  insideSpan = sourceSpan "FragmentFixture.hs" 3 1 3 16
  outsideSpan = sourceSpan "FragmentFixture.hs" 7 1 7 16
  annotationSpan = sourceSpan "FragmentFixture.hs" 2 1 4 10
  inside = EP.Comment Nothing insideSpan "-- same comment"
  outside = EP.Comment Nothing outsideSpan "-- same comment"
  annotationKey = EP.AnnKey [annotationSpan] $ EP.CN "fixture"
  annotation = EP.Ann
    { EP.annCapturedSpan = Nothing
    , EP.annSortKey = Nothing
    , EP.annsDP = []
    , EP.annFollowingComments = []
    , EP.annPriorComments = [(inside, EP.DP (0, 0)), (outside, EP.DP (1, 0))]
    , EP.annEntryDelta = EP.DP (0, 0)
    }
  fragment = ExactSourceFragment
    { fragmentText = Text.pack "fixture\n-- same comment\nvalue"
    , fragmentRange = SourceRange "FragmentFixture.hs" 2 1 4 10
    , fragmentAnnotationKeys = Set.singleton annotationKey
    , fragmentCommentKeys = Set.singleton $ SourceCommentKey insideSpan
    , fragmentAbsoluteColumn = Nothing
    }

sourceSpan :: FilePath -> Int -> Int -> Int -> Int -> SrcLoc.SrcSpan
sourceSpan file startLine startColumn endLine endColumn =
  EP.realSpanToSrcSpan $ SrcLoc.mkRealSrcSpan
    (SrcLoc.mkRealSrcLoc (FastString.mkFastString file) startLine startColumn)
    (SrcLoc.mkRealSrcLoc (FastString.mkFastString file) endLine endColumn)

commentLines :: String -> [String]
commentLines = filter isComment . lines
 where
  isComment line = any (`List.isPrefixOf` dropWhile (== ' ') line)
    ["--", "{-^"]

isOutsideRangeError :: Either String EP.Anns -> Bool
isOutsideRangeError = either
  (List.isInfixOf "outside its range")
  (const False)

fixturePath :: FilePath -> FilePath -> FilePath
fixturePath projectRoot fixtureName = FilePath.combine
  (FilePath.combine projectRoot "source/test-suite/fixtures")
  fixtureName
