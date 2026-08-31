module ExactSourceFragmentSpec (spec) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified GHC.Data.FastString as FastString
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.Backend
  ( consumeExactSourceFragment
  , reserveSourceFragmentComments
  )
import Language.Haskell.Brittany.Internal.ExactSource
  ( validateOpaqueSourceFragment
  )
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as EP
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Types (BriDoc(..))
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

  Hspec.it "reserves comments rendered by nested source fragments" $ do
    let (fragment, annotations, outsideComment) = fragmentFixture
        annotationKey = fst $ Map.findMin annotations
        document = BDSeq
          [ BDLit $ Text.pack "prefix"
          , BDExternal annotationKey False $ SourceFragment fragment
          ]
        remaining = reserveSourceFragmentComments document annotations
        comments = concatMap EP.annPriorComments $ Map.elems remaining
    comments `Hspec.shouldBe` [(outsideComment, EP.DP (1, 0))]

  Hspec.it "keeps annotations when a document has no source fragment" $ do
    let (_, annotations, _) = fragmentFixture
    reserveSourceFragmentComments (BDLit $ Text.pack "native") annotations
      `Hspec.shouldBe` annotations

  Hspec.it "ignores source fragments in inactive alternatives" $ do
    let (fragment, annotations, _) = fragmentFixture
        annotationKey = fst $ Map.findMin annotations
        inactive = BDExternal annotationKey False $ SourceFragment fragment
        document = BDAlt [BDLit $ Text.pack "selected", inactive]
    reserveSourceFragmentComments document annotations
      `Hspec.shouldBe` annotations

  Hspec.it "does not reserve a different equal-text comment" $ do
    let (fragment, annotations, outsideComment) = fragmentFixture
        annotationKey = fst $ Map.findMin annotations
        document = BDExternal annotationKey False $ SourceFragment fragment
        remaining = reserveSourceFragmentComments document annotations
        outsideKey = SourceCommentKey $ EP.commentIdentifier outsideComment
        remainingKeys = Set.fromList
          [ SourceCommentKey $ EP.commentIdentifier comment
          | annotation <- Map.elems remaining
          , (comment, _) <- EP.annPriorComments annotation
          ]
    outsideKey `Set.member` remainingKeys `Hspec.shouldBe` True

  Hspec.it "rejects a comment key outside the fragment source range" $ do
    let (fragment, annotations, outsideComment) = fragmentFixture
        invalid = fragment
          { fragmentCommentKeys = Set.singleton
              $ SourceCommentKey
              $ EP.commentIdentifier outsideComment
          }
    consumeExactSourceFragment invalid annotations
      `Hspec.shouldSatisfy` isOutsideRangeError

  Hspec.it "rejects opaque fragments with externally owned comments" $ do
    let (fragment, _, _) = fragmentFixture
        commentKey = Set.findMin $ fragmentCommentKeys fragment
        commentSpan = realSourceSpan "FragmentFixture.hs" 3 1 3 16
        externalOwner = EP.AnnKey
          [sourceSpan "FragmentFixture.hs" 1 1 8 1]
          $ EP.CN "external-owner"
        commentPlan = CommentPlan
          { commentPlanSources = Map.singleton commentKey SourceComment
            { sourceCommentKey = commentKey
            , sourceCommentText = Text.pack "-- same comment"
            , sourceCommentSpan = commentSpan
            , sourceCommentSyntax = LineComment
            }
          , commentPlanPlacements = Map.singleton commentKey CommentPlacement
            { placementOwner = NodeId externalOwner
            , placementRole = LeadingOrdinary
            , placementAnchor = BeforeNode
            , placementLineRelation = CommentOwnLine
            , placementRelativeOrder = 0
            }
          , commentPlanBoundaries = Map.empty
          }
    validateOpaqueSourceFragment fragment commentPlan
      `Hspec.shouldSatisfy` isOwnershipError

  Hspec.it "rejects opaque fragments without a comment placement" $ do
    let (fragment, _, _) = fragmentFixture
        commentPlan = CommentPlan Map.empty Map.empty Map.empty
    validateOpaqueSourceFragment fragment commentPlan
      `Hspec.shouldSatisfy` isOwnershipError

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
    , fragmentRebaseContinuation = True
    }

sourceSpan :: FilePath -> Int -> Int -> Int -> Int -> SrcLoc.SrcSpan
sourceSpan file startLine startColumn endLine endColumn =
  EP.realSpanToSrcSpan
    $ realSourceSpan file startLine startColumn endLine endColumn

realSourceSpan
  :: FilePath -> Int -> Int -> Int -> Int -> SrcLoc.RealSrcSpan
realSourceSpan file startLine startColumn endLine endColumn =
  SrcLoc.mkRealSrcSpan
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

isOwnershipError :: Either String () -> Bool
isOwnershipError = either
  (List.isInfixOf "without local ownership")
  (const False)

fixturePath :: FilePath -> FilePath -> FilePath
fixturePath projectRoot fixtureName = FilePath.combine
  (FilePath.combine projectRoot "source/test-suite/fixtures")
  fixtureName
