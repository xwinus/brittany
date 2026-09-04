module AltCommentSpec (spec) where

import qualified Data.Text as Text
import qualified GHC.Data.FastString as FastString
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as EP
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Transformations.Alt.Comments
import Language.Haskell.Brittany.Internal.Types
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "alternative layout comment checks" $ do
  Hspec.it "detects a line comment in a shared alternative document" $ do
    let shared = lineCommentDocument 1
        document = (0, BDFAlt [shared, shared])
    containsLineComment document `Hspec.shouldBe` True

  Hspec.it "requires a break when content follows an inline line comment" $ do
    sequenceRequiresCommentLineBreak
      True
      [lineCommentDocument 1, literalDocument 2]
      `Hspec.shouldBe` True

  Hspec.it "ignores empty tails and block comments" $ do
    sequenceRequiresCommentLineBreak
      True
      [lineCommentDocument 1, (2, BDFSeparator), (3, BDFEmpty)]
      `Hspec.shouldBe` False
    let blockSequence =
          (4, BDFSeq [blockCommentDocument 5, literalDocument 6])
    containsLineComment blockSequence `Hspec.shouldBe` False
    sequenceRequiresCommentLineBreak
      (containsLineComment blockSequence)
      [blockCommentDocument 5, literalDocument 6]
      `Hspec.shouldBe` False

lineCommentDocument :: Int -> BriDocNumbered
lineCommentDocument nodeId = (nodeId, BDFComment $ plannedComment LineComment)

blockCommentDocument :: Int -> BriDocNumbered
blockCommentDocument nodeId = (nodeId, BDFComment $ plannedComment BlockComment)

literalDocument :: Int -> BriDocNumbered
literalDocument nodeId = (nodeId, BDFLit $ Text.pack "value")

plannedComment :: SourceCommentSyntax -> PlannedComment
plannedComment syntax = PlannedComment
  { plannedCommentSource = SourceComment
      { sourceCommentKey = SourceCommentKey $ EP.realSpanToSrcSpan commentSpan
      , sourceCommentText = Text.pack $ case syntax of
          LineComment -> "-- comment"
          BlockComment -> "{- comment -}"
      , sourceCommentSpan = commentSpan
      , sourceCommentSyntax = syntax
      }
  , plannedCommentPlacement = CommentPlacement
      { placementOwner = NodeId ownerKey
      , placementRole = TrailingSameLine
      , placementAnchor = AfterNode
      , placementLineRelation = InlineComment
      , placementRelativeOrder = 0
      }
  , plannedCommentBoundary = CommentBoundaryId
      { commentBoundaryPath = ExpressionBoundaryPath 0
      , commentBoundaryGap = WithinBoundary
      }
  , plannedCommentIndentPolicy = OwnerRelativeIndent
  , plannedCommentLineDelta = 0
  , plannedCommentColumnDelta = 1
  }

ownerKey :: EP.AnnKey
ownerKey = EP.AnnKey [EP.realSpanToSrcSpan ownerSpan] $ EP.CN "HsVar"

ownerSpan :: SrcLoc.RealSrcSpan
ownerSpan = realSpan 1 1 1 6

commentSpan :: SrcLoc.RealSrcSpan
commentSpan = realSpan 1 7 1 17

realSpan :: Int -> Int -> Int -> Int -> SrcLoc.RealSrcSpan
realSpan startLine startColumn endLine endColumn = SrcLoc.mkRealSrcSpan
  (SrcLoc.mkRealSrcLoc fileName startLine startColumn)
  (SrcLoc.mkRealSrcLoc fileName endLine endColumn)
 where
  fileName = FastString.mkFastString "AltCommentSpec.hs"
