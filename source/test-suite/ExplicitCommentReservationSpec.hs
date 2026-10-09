module ExplicitCommentReservationSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Generics.Uniplate.Direct as Uniplate
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified GHC.Data.FastString as FastString
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.CommentIR
  ( CommentIRError(..), lowerPlannedComments, validatePlannedCommentNodes )
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as EP
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Types
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "explicit comment reservation" $ do
  Hspec.it "reserves explicit placement before an earlier coarse following annotation" $ do
    output <- lowered followingAnnotations followingDocument
    assertOneExplicit output
  Hspec.it "reserves explicit placement before an earlier prior annotation" $ do
    output <- lowered priorAnnotations priorDocument
    assertOneExplicit output
  Hspec.it "retains explicit placement in every equal-coverage alternative" $ do
    output <- lowered followingAnnotations
      (20, BDFAlt [followingDocument, (21, BDFLines [followingDocument])])
    case output of
      BDAlt alternatives -> do
        length alternatives `Hspec.shouldBe` 2
        mapM_ assertOneExplicit alternatives
      _ -> Hspec.expectationFailure "expected both layout alternatives to survive"
  Hspec.it "rejects genuine duplicate explicit emissions" $ do
    let document = (30, BDFSeq [(31, BDFComment explicitComment), (32, BDFComment explicitComment)])
        result = lowerPlannedComments followingAnnotations completePlan document
          >>= validatePlannedCommentNodes . unwrapBriDocNumbered
    result `Hspec.shouldBe` Left [DuplicatePlannedComment commentKey]
  Hspec.it "prunes an alternative that omits the explicit comment" $ do
    let document = (40, BDFAlt [(41, BDFComment explicitComment), (42, BDFLit $ Text.pack "missing")])
    output <- lowered followingAnnotations document
    case output of
      BDAlt [retained] -> assertOneExplicit retained
      _ -> Hspec.expectationFailure "expected only the complete alternative to survive"
  forM_ [False, True] $ \explicitFirst ->
    Hspec.it ("reserves comments across annotation-only alternatives, explicit first = " ++ show explicitFirst) $ do
      let annotationOnly = (51, BDFAnnotationRest ownerKey (52, BDFLit $ Text.pack "body"))
          explicit = (53, BDFComment explicitComment)
          alternatives = if explicitFirst then [explicit, annotationOnly] else [annotationOnly, explicit]
          document = (50, BDFSeq
            [ (54, BDFAnnotationRest ownerKey (55, BDFLit $ Text.pack "prepare"))
            , (56, BDFAlt alternatives)
            ])
      output <- lowered followingAnnotations document
      assertOneExplicit output
      let branchCounts = [length alternatives' | BDAlt alternatives' <- Uniplate.universe output]
      branchCounts `Hspec.shouldBe` [1]
  forM_ [False, True] $ \explicitFirst ->
    Hspec.it ("keeps reservation states distinct through a shared cached node, explicit first = " ++ show explicitFirst) $ do
      let shared = (70, BDFSeq [(71, BDFLit $ Text.pack "binding")])
          annotationOnly = (72, BDFSeq
            [ (73, BDFAnnotationRest ownerKey (74, BDFLit $ Text.pack "body"))
            , shared
            ])
          explicit = (75, BDFSeq [(76, BDFComment explicitComment), shared])
          alternatives = if explicitFirst then [explicit, annotationOnly] else [annotationOnly, explicit]
          document = (77, BDFSeq
            [ (78, BDFAnnotationRest ownerKey (79, BDFLit $ Text.pack "prepare"))
            , (80, BDFAlt alternatives)
            ])
      output <- lowered followingAnnotations document
      assertOneExplicit output
      let branchCounts = [length alternatives' | BDAlt alternatives' <- Uniplate.universe output]
      branchCounts `Hspec.shouldBe` [1]
  Hspec.it "rejects incomparable explicit reservations across two canonical keys" $ do
    let other = secondComment
        otherKey = sourceCommentKey $ plannedCommentSource other
        otherRaw = EP.Comment Nothing (EP.realSpanToSrcSpan $ sourceCommentSpan $ plannedCommentSource other)
          "-- Second explanation."
        annotations = Map.singleton ownerKey emptyAnnotation
          { EP.annFollowingComments = [(comment, EP.DP (1, 4)), (otherRaw, EP.DP (1, 4))] }
        plan = completePlan
          { commentPlanSources = Map.insert otherKey (plannedCommentSource other) $ commentPlanSources completePlan
          , commentPlanPlacements = Map.insert otherKey (plannedCommentPlacement other) $ commentPlanPlacements completePlan
          , commentPlanBoundaries = Map.insert otherKey boundary $ commentPlanBoundaries completePlan
          }
        document = (60, BDFSeq
          [ (61, BDFAnnotationRest ownerKey (62, BDFLit $ Text.pack "prepare"))
          , (63, BDFAlt [(64, BDFComment explicitComment), (65, BDFComment other)])
          ])
    case lowerPlannedComments annotations plan document of
      Left [AlternativeCommentMismatch _] -> pure ()
      Left errors -> Hspec.expectationFailure $ "unexpected error: " ++ show errors
      Right _ -> Hspec.expectationFailure "no alternative emits both required canonical comments"

lowered :: EP.Anns -> BriDocNumbered -> IO BriDoc
lowered annotations document = case lowerPlannedComments annotations completePlan document of
  Left errors -> Hspec.expectationFailure (show errors) >> fail "comment lowering failed"
  Right result -> pure $ unwrapBriDocNumbered result

assertOneExplicit :: BriDoc -> IO ()
assertOneExplicit document = do
  let comments = [planned | BDComment planned <- Uniplate.universe document]
  comments `Hspec.shouldBe` [explicitComment]
  validatePlannedCommentNodes document `Hspec.shouldBe` Right ()

followingDocument :: BriDocNumbered
followingDocument = (0, BDFSeq
  [ (1, BDFAnnotationRest ownerKey (2, BDFLit $ Text.pack "prepare"))
  , (3, BDFComment explicitComment)
  , (4, BDFLit $ Text.pack "binding")
  ])

priorDocument :: BriDocNumbered
priorDocument = (5, BDFSeq
  [ (6, BDFAnnotationPrior PriorCommentSource ownerKey (7, BDFLit $ Text.pack "prepare"))
  , (8, BDFComment explicitComment)
  , (9, BDFLit $ Text.pack "binding")
  ])

explicitComment :: PlannedComment
explicitComment = PlannedComment
  { plannedCommentSource = sourceComment
  , plannedCommentPlacement = placement
  , plannedCommentBoundary = boundary
  , plannedCommentIndentPolicy = RenderedAnchorIndent
  , plannedCommentLineDelta = 1
  , plannedCommentColumnDelta = 0
  }

secondComment :: PlannedComment
secondComment = explicitComment
  { plannedCommentSource = sourceComment
      { sourceCommentKey = SourceCommentKey $ EP.realSpanToSrcSpan span'
      , sourceCommentSpan = span'
      , sourceCommentText = Text.pack "-- Second explanation."
      }
  , plannedCommentPlacement = placement { placementRelativeOrder = 1 }
  }
 where
  span' = SrcLoc.mkRealSrcSpan
    (SrcLoc.mkRealSrcLoc file 5 5)
    (SrcLoc.mkRealSrcLoc file 5 27)
  file = FastString.mkFastString "ExplicitCommentReservation.hs"

completePlan :: CommentPlan
completePlan = CommentPlan
  { commentPlanSources = Map.singleton commentKey sourceComment
  , commentPlanPlacements = Map.singleton commentKey placement
  , commentPlanBoundaries = Map.singleton commentKey boundary
  }

sourceComment :: SourceComment
sourceComment = SourceComment
  { sourceCommentKey = commentKey
  , sourceCommentText = Text.pack "-- Local explanation."
  , sourceCommentSpan = commentSpan
  , sourceCommentSyntax = LineComment
  }

placement :: CommentPlacement
placement = CommentPlacement
  { placementOwner = NodeId ownerKey
  , placementRole = LeadingOrdinary
  , placementAnchor = BeforeNode
  , placementLineRelation = CommentOwnLine
  , placementRelativeOrder = 0
  }

boundary :: CommentBoundaryId
boundary = CommentBoundaryId (DeclarationBoundaryPath 0) WithinBoundary

followingAnnotations :: EP.Anns
followingAnnotations = Map.singleton ownerKey emptyAnnotation
  { EP.annFollowingComments = [(comment, EP.DP (1, 4))] }

priorAnnotations :: EP.Anns
priorAnnotations = Map.singleton ownerKey emptyAnnotation
  { EP.annPriorComments = [(comment, EP.DP (1, 4))] }

emptyAnnotation :: EP.Annotation
emptyAnnotation = EP.Ann
  { EP.annCapturedSpan = Nothing
  , EP.annSortKey = Nothing
  , EP.annsDP = []
  , EP.annFollowingComments = []
  , EP.annPriorComments = []
  , EP.annEntryDelta = EP.DP (0, 0)
  }

ownerKey :: EP.AnnKey
ownerKey = EP.AnnKey [EP.realSpanToSrcSpan commentSpan] $ EP.CN "ValueDecl"

commentKey :: SourceCommentKey
commentKey = SourceCommentKey $ EP.realSpanToSrcSpan commentSpan

comment :: EP.Comment
comment = EP.Comment Nothing (EP.realSpanToSrcSpan commentSpan) "-- Local explanation."

commentSpan :: SrcLoc.RealSrcSpan
commentSpan = SrcLoc.mkRealSrcSpan
  (SrcLoc.mkRealSrcLoc file 4 5)
  (SrcLoc.mkRealSrcLoc file 4 26)
 where
  file = FastString.mkFastString "ExplicitCommentReservation.hs"
