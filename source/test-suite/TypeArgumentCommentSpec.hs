module TypeArgumentCommentSpec (spec) where

import qualified Data.Text as Text
import qualified GHC.Data.FastString as FastString
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as EP
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Transformations.Floating
  ( transformSimplifyFloating )
import Language.Haskell.Brittany.Internal.Types
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "type argument comment floating" $ do
  Hspec.it "removes a type-continuation base from an own-line argument post-doc" $ do
    let planned = plannedPostDoc SignatureArgument CommentOwnLine
    case transformSimplifyFloating $ BDAddBaseY (BrIndentSpecial 3) $ BDComment planned of
      BDComment result -> result `Hspec.shouldBe` planned
      _ -> Hspec.expectationFailure "own-line argument post-doc retained the type continuation base"
  Hspec.it "retains the base of an inline argument post-doc" $
    assertRetained SignatureArgument InlineComment
  Hspec.it "retains the base of an own-line record-field post-doc" $
    assertRetained RecordField CommentOwnLine

assertRetained :: CommentedNode -> CommentLineRelation -> IO ()
assertRetained owner relation = do
  let planned = plannedPostDoc owner relation
  case transformSimplifyFloating $ BDAddBaseY (BrIndentSpecial 3) $ BDComment planned of
    BDAddBaseY (BrIndentSpecial 3) (BDComment result) -> result `Hspec.shouldBe` planned
    _ -> Hspec.expectationFailure "an unrelated comment lost its indentation base"

plannedPostDoc :: CommentedNode -> CommentLineRelation -> PlannedComment
plannedPostDoc owner relation = PlannedComment
  { plannedCommentSource = SourceComment
      { sourceCommentKey = SourceCommentKey $ EP.realSpanToSrcSpan commentSpan
      , sourceCommentText = Text.pack "-- ^ Argument documentation."
      , sourceCommentSpan = commentSpan
      , sourceCommentSyntax = LineComment
      }
  , plannedCommentPlacement = CommentPlacement
      { placementOwner = NodeId $ EP.AnnKey [EP.realSpanToSrcSpan commentSpan] $ EP.CN "HsTyVar"
      , placementRole = HaddockPostDoc owner
      , placementAnchor = AfterNode
      , placementLineRelation = relation
      , placementRelativeOrder = 0
      }
  , plannedCommentBoundary = CommentBoundaryId (DeclarationBoundaryPath 0) WithinBoundary
  , plannedCommentIndentPolicy = SourceColumnIndent
  , plannedCommentLineDelta = if relation == CommentOwnLine then 1 else 0
  , plannedCommentColumnDelta = 2
  }
 where
  commentSpan = SrcLoc.mkRealSrcSpan
    (SrcLoc.mkRealSrcLoc file 3 3)
    (SrcLoc.mkRealSrcLoc file 3 31)
  file = FastString.mkFastString "TypeArgumentComments.hs"
