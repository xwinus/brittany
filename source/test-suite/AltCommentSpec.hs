module AltCommentSpec (spec) where

import qualified Data.Text as Text
import qualified GHC.Data.FastString as FastString
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as EP
import Language.Haskell.Brittany.Internal.Delimiter.Comments
  ( extractBoundaryComments )
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Transformations.Alt.Comments
import Language.Haskell.Brittany.Internal.Transformations.Floating
  ( transformSimplifyFloating )
import Language.Haskell.Brittany.Internal.Transformations.Indent
  ( transformSimplifyIndent )
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

  Hspec.it "requires a break after an own-line expression comment with a follower" $ do
    let comment = (1, BDFComment $ ownLineComment LineComment)
        documents = [comment, (2, BDFSeparator), literalDocument 3]
        document = (0, BDFSeq documents)
    containsLineComment document `Hspec.shouldBe` True
    containsCommentLineBreak document `Hspec.shouldBe` True
    sequenceRequiresCommentLineBreak
      (containsCommentLineBreak document) documents `Hspec.shouldBe` True

  Hspec.it "detects an own-line block boundary while requiring following content" $ do
    let comment = (1, BDFComment $ ownLineComment BlockComment)
        documents = [comment, literalDocument 2]
        document = (0, BDFSeq documents)
    containsLineComment document `Hspec.shouldBe` False
    containsCommentLineBreak document `Hspec.shouldBe` True
    sequenceRequiresCommentLineBreak
      (containsCommentLineBreak document) documents `Hspec.shouldBe` True
    sequenceRequiresCommentLineBreak
      (containsCommentLineBreak comment)
      [comment, (3, BDFSeparator), (4, BDFEmpty)] `Hspec.shouldBe` False

  Hspec.it "excludes leading, container-relative, inline block and terminal boundaries" $ do
    let own = ownLineComment BlockComment
        placement = plannedCommentPlacement own
        excluded =
          [ own { plannedCommentPlacement = placement { placementAnchor = BeforeNode } }
          , own { plannedCommentIndentPolicy = ContainerRelativeIndent }
          , own { plannedCommentPlacement = placement { placementLineRelation = InlineComment } }
          , own { plannedCommentLineDelta = 0 }
          ]
        requiresBreak planned =
          let documents = [(1, BDFComment planned), literalDocument 2]
              document = (0, BDFSeq documents)
          in sequenceRequiresCommentLineBreak
              (containsCommentLineBreak document) documents
        terminal = (1, BDFComment $ ownLineComment LineComment)
    map requiresBreak excluded `Hspec.shouldBe` replicate 4 False
    sequenceRequiresCommentLineBreak
      (containsCommentLineBreak terminal) [terminal] `Hspec.shouldBe` False

  Hspec.it "carries the base to a sequence comment without shifting the prior paragraph" $ do
    let prior = BDPar BrIndentNone
          (BDLit $ Text.pack "earlier") (BDLit $ Text.pack "paragraph")
        comment = BDComment $ ownLineComment LineComment
        argument = BDLit $ Text.pack "argument"
        document = BDAddBaseY (BrIndentSpecial 2) $ BDSeq
          [prior, comment, BDSeparator, argument]
        expected = BDSeq
          [prior, BDAddBaseY (BrIndentSpecial 2) comment, BDSeparator, argument]
    assertTransformsTo document expected

  Hspec.it "carries the base only to the nested block boundary in preceding columns" $ do
    let function = BDLit $ Text.pack "function"
        comment = BDComment $ ownLineComment BlockComment
        argument = BDLit $ Text.pack "argument"
        signature = ColApp $ Text.pack "application"
        document = BDAddBaseY (BrIndentSpecial 4) $ BDCols signature
          [BDSeq [function, comment], argument]
        expected = BDCols signature
          [BDSeq [function, BDAddBaseY (BrIndentSpecial 4) comment], argument]
    assertTransformsTo document expected

  Hspec.it "still simplifies structural bases for uncommented sequences and columns" $ do
    let children = [BDLit $ Text.pack "function", BDLit $ Text.pack "argument"]
        sequenceBody = BDSeq children
        columnBody = BDCols (ColApp $ Text.pack "application") children
    assertTransformsTo (BDAddBaseY (BrIndentSpecial 2) sequenceBody) sequenceBody
    assertTransformsTo (BDAddBaseY (BrIndentSpecial 4) columnBody) columnBody

  Hspec.it "does not attach expression continuation bases to type or pattern comments" $ do
    let checkOwner constructor = do
          let planned = ownLineComment BlockComment
              placement = plannedCommentPlacement planned
              owner = NodeId $ EP.AnnKey [EP.realSpanToSrcSpan ownerSpan] $ EP.CN constructor
              comment = planned { plannedCommentPlacement = placement { placementOwner = owner } }
              body = BDSeq [BDComment comment, BDLit $ Text.pack "following"]
              numbered = (0, BDFSeq [(1, BDFComment comment), literalDocument 2])
          assertTransformsTo (BDAddBaseY (BrIndentSpecial 2) body) body
          containsCommentLineBreak numbered `Hspec.shouldBe` False
    mapM_ checkOwner ["HsTyVar", "ConPat"]

  Hspec.it "ignores transparent empty tails but still detects a real follower" $ do
    let comment = (1, BDFComment $ ownLineComment LineComment)
        emptyTail = (2, BDFAddBaseY BrIndentRegular
          (3, BDFAnnotationRest ownerKey
            (4, BDFNonBottomSpacing False
              (5, BDFSeq [(6, BDFSeparator), (7, BDFEmpty)]))))
        prefix = (8, BDFSeq [comment, emptyTail])
        realFollower = (9, BDFEnsureIndent BrIndentRegular $ literalDocument 10)
    sequenceRequiresCommentLineBreak
      (containsCommentLineBreak prefix) [comment, emptyTail]
      `Hspec.shouldBe` False
    sequenceRequiresCommentLineBreak
      (containsCommentLineBreak prefix) [prefix, realFollower]
      `Hspec.shouldBe` True

  Hspec.it "requires following code rather than another own-line comment" $ do
    let first = (1, BDFComment $ ownLineComment LineComment)
        second = (2, BDFComment $ ownLineComment BlockComment)
        documents = [first, (3, BDFSeparator), second]
        document = (0, BDFSeq documents)
    sequenceRequiresCommentLineBreak
      (containsCommentLineBreak document) documents `Hspec.shouldBe` False
    sequenceRequiresCommentLineBreak
      (containsCommentLineBreak document) (documents ++ [literalDocument 4])
      `Hspec.shouldBe` True

  Hspec.it "extracts a matching delimiter boundary comment" $ do
    let comment = lineCommentDocumentAt 1 BeforeCloseBoundary
        literal = literalDocument 2
        document = (0, BDFSeq [comment, literal])
        (comments, remaining) = extractBoundaryComments
          BeforeCloseBoundary document
    ( (unwrapBriDocNumbered <$> comments, unwrapBriDocNumbered remaining)
        == ( [unwrapBriDocNumbered comment]
           , BDSeq [BDEmpty, BDLit $ Text.pack "value"]
           )
      ) `Hspec.shouldBe` True

  Hspec.it "deduplicates a comment in a shared BriDoc subtree" $ do
    let comment = lineCommentDocumentAt 2 BeforeCloseBoundary
        shared = (1, BDFSeq [comment, literalDocument 3])
        document = (0, BDFAlt [shared, shared])
        (comments, remaining) = extractBoundaryComments
          BeforeCloseBoundary document
        transformedShared = BDSeq [BDEmpty, BDLit $ Text.pack "value"]
    ( (unwrapBriDocNumbered <$> comments, unwrapBriDocNumbered remaining)
        == ( [unwrapBriDocNumbered comment]
           , BDAlt [transformedShared, transformedShared]
           )
      ) `Hspec.shouldBe` True

  Hspec.it "preserves an empty malformed alternative" $ do
    let document = (0, BDFAlt [])
        (comments, remaining) = extractBoundaryComments
          BeforeCloseBoundary document
    (null comments && unwrapBriDocNumbered remaining == BDAlt [])
      `Hspec.shouldBe` True

assertTransformsTo :: BriDoc -> BriDoc -> IO ()
assertTransformsTo document expected = mapM_ check
  [ ("floating", transformSimplifyFloating)
  , ("indent", transformSimplifyIndent)
  , ("floating followed by indent", transformSimplifyIndent . transformSimplifyFloating)
  ]
 where
  check (name, transform) =
    (name, transform document == expected) `Hspec.shouldBe` (name, True)

lineCommentDocument :: Int -> BriDocNumbered
lineCommentDocument nodeId = lineCommentDocumentAt nodeId WithinBoundary

lineCommentDocumentAt :: Int -> CommentBoundaryGap -> BriDocNumbered
lineCommentDocumentAt nodeId gap =
  (nodeId, BDFComment $ plannedCommentAt LineComment gap)

blockCommentDocument :: Int -> BriDocNumbered
blockCommentDocument nodeId = (nodeId, BDFComment $ plannedComment BlockComment)

literalDocument :: Int -> BriDocNumbered
literalDocument nodeId = (nodeId, BDFLit $ Text.pack "value")

ownLineComment :: SourceCommentSyntax -> PlannedComment
ownLineComment syntax =
  let planned = plannedComment syntax
      placement = plannedCommentPlacement planned
  in planned
      { plannedCommentPlacement = placement { placementLineRelation = CommentOwnLine }
      , plannedCommentIndentPolicy = SourceColumnIndent
      , plannedCommentLineDelta = 1
      }

plannedComment :: SourceCommentSyntax -> PlannedComment
plannedComment syntax = plannedCommentAt syntax WithinBoundary

plannedCommentAt :: SourceCommentSyntax -> CommentBoundaryGap -> PlannedComment
plannedCommentAt syntax gap = PlannedComment
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
      , commentBoundaryGap = gap
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
