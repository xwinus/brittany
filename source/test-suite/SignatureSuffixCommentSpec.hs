{-# LANGUAGE DataKinds #-}

module SignatureSuffixCommentSpec (spec) where

import Control.Monad (forM_)
import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import Data.Functor.Identity (runIdentity)
import qualified Data.Generics.Uniplate.Direct as Uniplate
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified GHC.Data.FastString as FastString
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany (staticDefaultConfig)
import Language.Haskell.Brittany.Internal.CommentIR
  ( lowerPlannedComments, validatePlannedCommentNodes )
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as EP
import Language.Haskell.Brittany.Internal.Layouters.Decl.SignatureComments
  ( appendSignatureSuffix )
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Types
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "signature suffix annotation ordering" $ do
  Hspec.it "keeps an explicit suffix after unclaimed rest comments" $
    assertUnclaimedOrder RestWrapper
  Hspec.it "keeps an explicit suffix after unclaimed keyword comments" $
    assertUnclaimedOrder KeywordWrapper
  Hspec.it "places the suffix on the last row through empty or fully claimed wrappers" $
    forM_ [RestWrapper, KeywordWrapper] $ \wrapper -> forM_ [False, True] $ \claimed -> do
      let annotations = annotationFor wrapper $ if claimed then [explicitComment] else []
          plan = planFor [explicitComment]
      document <- appendChecked annotations plan $ wrapped wrapper
      case wrappedRows $ unwrapBriDocNumbered document of
        Just [BDLit first, BDSeq [BDLit final, BDComment suffix]] -> do
          first `Hspec.shouldBe` Text.pack "Input"
          final `Hspec.shouldBe` Text.pack "Result"
          sourceCommentKey (plannedCommentSource suffix)
            `Hspec.shouldBe` sourceCommentKey (plannedCommentSource explicitComment)
        _ -> Hspec.expectationFailure "suffix did not reach only the final type row"
      assertLoweredComments annotations plan document ["-- explicit final"]

assertUnclaimedOrder :: Wrapper -> IO ()
assertUnclaimedOrder wrapper = do
  let annotations = annotationFor wrapper [nativeComment]
      plan = planFor [nativeComment, explicitComment]
  document <- appendChecked annotations plan $ wrapped wrapper
  case unwrapBriDocNumbered document of
    BDSeq [original, BDComment suffix] -> do
      case wrappedRows original of
        Just [BDLit first, BDLit final] -> do
          first `Hspec.shouldBe` Text.pack "Input"
          final `Hspec.shouldBe` Text.pack "Result"
        _ -> Hspec.expectationFailure "annotation child changed despite an unclaimed comment"
      sourceCommentKey (plannedCommentSource suffix)
        `Hspec.shouldBe` sourceCommentKey (plannedCommentSource explicitComment)
    _ -> Hspec.expectationFailure "explicit suffix moved inside an unclaimed comment wrapper"
  assertLoweredComments annotations plan document ["-- native rest", "-- explicit final"]

appendChecked :: EP.Anns -> CommentPlan -> BriDocNumbered -> IO BriDocNumbered
appendChecked annotations plan body = do
  let ((document, errors), _) = result
  length errors `Hspec.shouldBe` 0
  pure document
 where
  result :: ((BriDocNumbered, [BrittanyError]), Seq String)
  result = runIdentity $ MultiRWSS.runMultiRWSTNil
    $ MultiRWSS.withMultiWriterAW
    $ MultiRWSS.withMultiWriterAW
    $ MultiRWSS.withMultiStateA (NodeAllocIndex 100)
    $ MultiRWSS.withMultiReader plan
    $ MultiRWSS.withMultiReader (OriginalSource Text.empty)
    $ MultiRWSS.withMultiReader annotations
    $ MultiRWSS.withMultiReader staticDefaultConfig
    $ appendSignatureSuffix
        (Set.singleton $ sourceCommentKey $ plannedCommentSource explicitComment)
        (pure body)
        (pure (5, BDFComment explicitComment))

assertLoweredComments :: EP.Anns -> CommentPlan -> BriDocNumbered -> [String] -> IO ()
assertLoweredComments annotations plan document expected =
  case lowerPlannedComments annotations plan document of
    Left errors -> Hspec.expectationFailure $ show errors
    Right lowered -> do
      let output = unwrapBriDocNumbered lowered
          comments = [comment | BDComment comment <- Uniplate.universe output]
      map (Text.unpack . sourceCommentText . plannedCommentSource) comments
        `Hspec.shouldBe` expected
      validatePlannedCommentNodes output `Hspec.shouldBe` Right ()

data Wrapper = RestWrapper | KeywordWrapper

wrapped :: Wrapper -> BriDocNumbered
wrapped wrapper = (4, case wrapper of
  RestWrapper -> BDFAnnotationRest ownerKey body
  KeywordWrapper -> BDFAnnotationKW ownerKey Nothing body)
 where
  body = (1, BDFLines [(2, BDFLit $ Text.pack "Input"), (3, BDFLit $ Text.pack "Result")])

wrappedRows :: BriDoc -> Maybe [BriDoc]
wrappedRows (BDAnnotationRest _ (BDLines rows)) = Just rows
wrappedRows (BDAnnotationKW _ _ (BDLines rows)) = Just rows
wrappedRows _ = Nothing

annotationFor :: Wrapper -> [PlannedComment] -> EP.Anns
annotationFor wrapper comments = Map.singleton ownerKey $ case wrapper of
  RestWrapper -> emptyAnnotation { EP.annFollowingComments = entries }
  KeywordWrapper -> emptyAnnotation { EP.annsDP = [(EP.AnnComment comment, delta) | (comment, delta) <- entries] }
 where
  entries = [(rawComment comment, EP.DP (0, 1)) | comment <- comments]
  rawComment planned = EP.Comment Nothing
    (EP.realSpanToSrcSpan $ sourceCommentSpan $ plannedCommentSource planned)
    (Text.unpack $ sourceCommentText $ plannedCommentSource planned)

emptyAnnotation :: EP.Annotation
emptyAnnotation = EP.Ann Nothing Nothing [] [] [] (EP.DP (0, 0))

planFor :: [PlannedComment] -> CommentPlan
planFor comments = CommentPlan
  { commentPlanSources = Map.fromList [(key comment, plannedCommentSource comment) | comment <- comments]
  , commentPlanPlacements = Map.fromList [(key comment, plannedCommentPlacement comment) | comment <- comments]
  , commentPlanBoundaries = Map.fromList [(key comment, plannedCommentBoundary comment) | comment <- comments]
  }
 where
  key = sourceCommentKey . plannedCommentSource

nativeComment, explicitComment :: PlannedComment
nativeComment = plannedComment 4 "-- native rest"
explicitComment = plannedComment 5 "-- explicit final"

plannedComment :: Int -> String -> PlannedComment
plannedComment line text = PlannedComment
  { plannedCommentSource = SourceComment
      { sourceCommentKey = SourceCommentKey $ EP.realSpanToSrcSpan span'
      , sourceCommentText = Text.pack text
      , sourceCommentSpan = span'
      , sourceCommentSyntax = LineComment
      }
  , plannedCommentPlacement = CommentPlacement
      { placementOwner = NodeId ownerKey
      , placementRole = TrailingSameLine
      , placementAnchor = AfterNode
      , placementLineRelation = InlineComment
      , placementRelativeOrder = line - 4
      }
  , plannedCommentBoundary = CommentBoundaryId (DeclarationBoundaryPath 0) WithinBoundary
  , plannedCommentIndentPolicy = RenderedAnchorIndent
  , plannedCommentLineDelta = 0
  , plannedCommentColumnDelta = 1
  }
 where
  span' = SrcLoc.mkRealSrcSpan
    (SrcLoc.mkRealSrcLoc file line 8)
    (SrcLoc.mkRealSrcLoc file line $ 8 + length text)
  file = FastString.mkFastString "SignatureSuffixComment.hs"

ownerKey :: EP.AnnKey
ownerKey = EP.AnnKey [EP.realSpanToSrcSpan span'] $ EP.CN "HsFunTy"
 where
  span' = SrcLoc.mkRealSrcSpan
    (SrcLoc.mkRealSrcLoc file 2 3)
    (SrcLoc.mkRealSrcLoc file 4 7)
  file = FastString.mkFastString "SignatureSuffixComment.hs"
