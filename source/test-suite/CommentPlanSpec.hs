{-# LANGUAGE LambdaCase #-}

module CommentPlanSpec (spec) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified GHC.Data.FastString as FastString
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.CommentPlan
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as EP
import Language.Haskell.Brittany.Internal.ParseModule (parseModule)
import Language.Haskell.Brittany.Internal.SourceComment.Types
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "normalized source comment ownership" $ do
  Hspec.it "assigns expected declaration comment roles exactly once" $ do
    plan <- parsePlan "CommentPlanExpected.hs" expectedSource
    assertCompleteUniquePlan plan
    let roles = placementRole <$> Map.elems (commentPlanPlacements plan)
    mapM_ (\role -> roles `Hspec.shouldContain` [role])
      [ LeadingDoc
      , HaddockPostDoc SignatureArgument
      , HaddockPostDoc SignatureResult
      , HaddockPostDoc RecordField
      ]
    commentPlanFingerprint plan `Hspec.shouldSatisfy` any
      (\(commentText, _, role, _) ->
        commentText == Text.pack "-- deriving note"
          && role == BetweenChildren DerivingClause
      )

  Hspec.it "keeps edge-case identity and ordering deterministic" $ do
    firstPlan <- parsePlan "CommentPlanEdge.hs" edgeSource
    secondPlan <- parsePlan "CommentPlanEdge.hs" edgeSource
    firstPlan `Hspec.shouldBe` secondPlan
    assertCompleteUniquePlan firstPlan
    let equalTextKeys =
          [ sourceCommentKey sourceComment
          | sourceComment <- Map.elems $ commentPlanSources firstPlan
          , sourceCommentText sourceComment == Text.pack "-- same text"
          ]
        syntaxes = sourceCommentSyntax
          <$> Map.elems (commentPlanSources firstPlan)
        roles = placementRole <$> Map.elems (commentPlanPlacements firstPlan)
    Set.size (Set.fromList equalTextKeys) `Hspec.shouldBe` 2
    syntaxes `Hspec.shouldContain` [LineComment, BlockComment]
    roles `Hspec.shouldContain` [SectionComment]
    let pragma = sourceCommentAt "CommentPlanEdge.hs" 7 "{-# INLINE value #-}"
    case normalizeCommentPlan
      (Map.singleton (nodeKeyAt "SigD" 8) $ annotationWithPrior pragma) of
      Left planErrors -> Hspec.expectationFailure $ show planErrors
      Right pragmaPlan ->
        (placementRole <$> Map.elems (commentPlanPlacements pragmaPlan))
          `Hspec.shouldBe` [PragmaComment]

  Hspec.it "rejects duplicate ownership with a structured error" $ do
    let comment = sourceCommentAt "Ambiguous.hs" 3 "-- shared"
        annotations = Map.fromList
          [ (nodeKeyAt "First" 2, annotationWithPrior comment)
          , (nodeKeyAt "Second" 4, annotationWithPrior comment)
          ]
    case normalizeCommentPlan annotations of
      Left [AmbiguousCommentOwnership key owners] -> do
        key `Hspec.shouldBe` SourceCommentKey (EP.commentIdentifier comment)
        length owners `Hspec.shouldBe` 2
      result -> Hspec.expectationFailure
        $ "expected ambiguous ownership error, got " ++ show result

  Hspec.it "canonicalizes duplicate transport entries for one placement" $ do
    let comment = sourceCommentAt "Duplicate.hs" 3 "-- shared"
        annotation = (annotationWithPrior comment)
          { EP.annPriorComments =
              [(comment, EP.DP (1, 0)), (comment, EP.DP (1, 0))]
          }
    case normalizeCommentPlan $ Map.singleton (nodeKeyAt "Owner" 2) annotation of
      Left planErrors -> Hspec.expectationFailure $ show planErrors
      Right plan -> do
        Map.size (commentPlanSources plan) `Hspec.shouldBe` 1
        Map.size (commentPlanPlacements plan) `Hspec.shouldBe` 1

  Hspec.it "orders comments by source position rather than annotation map order" $ do
    let first = sourceCommentAt "Ordering.hs" 3 "-- first"
        second = sourceCommentAt "Ordering.hs" 4 "-- second"
        annotations = Map.fromList
          [ (nodeKeyAt "LateOwner" 10, annotationWithPrior first)
          , (nodeKeyAt "EarlyOwner" 2, annotationWithPrior second)
          ]
    case normalizeCommentPlan annotations of
      Left planErrors -> Hspec.expectationFailure $ show planErrors
      Right plan -> commentPlanCommentTexts plan `Hspec.shouldBe`
        [Text.pack "-- first", Text.pack "-- second"]

  Hspec.it "rejects conflicting roles for the same owner and source key" $ do
    let comment = sourceCommentAt "Placement.hs" 3 "-- shared"
        annotation = (annotationWithPrior comment)
          { EP.annFollowingComments = [(comment, EP.DP (1, 0))]
          }
    normalizeCommentPlan
      (Map.singleton (nodeKeyAt "Owner" 2) annotation)
      `Hspec.shouldSatisfy` \case
        Left [AmbiguousCommentPlacement _ roles] -> length roles == 2
        _ -> False

  Hspec.it "rejects conflicting line relations for one structural placement" $ do
    let comment = sourceCommentAt "PlacementLine.hs" 3 "-- shared"
        annotation = (annotationWithPrior comment)
          { EP.annPriorComments =
              [(comment, EP.DP (0, 1)), (comment, EP.DP (1, 0))]
          }
    normalizeCommentPlan
      (Map.singleton (nodeKeyAt "Owner" 2) annotation)
      `Hspec.shouldSatisfy` \case
        Left [AmbiguousCommentPlacement _ placements] ->
          length placements == 2
        _ -> False

  Hspec.it "rejects source comments without a real source span" $ do
    let comment = EP.Comment Nothing SrcLoc.noSrcSpan "-- invalid"
        annotations = Map.singleton
          (nodeKeyAt "Owner" 2)
          (annotationWithPrior comment)
    normalizeCommentPlan annotations `Hspec.shouldSatisfy` \case
      Left [InvalidSourceCommentSpan "-- invalid" _] -> True
      _ -> False

assertCompleteUniquePlan :: CommentPlan -> Hspec.Expectation
assertCompleteUniquePlan plan = do
  Map.keysSet (commentPlanSources plan) `Hspec.shouldBe`
    Map.keysSet (commentPlanPlacements plan)
  let orders = List.sort
        $ placementRelativeOrder <$> Map.elems (commentPlanPlacements plan)
  orders `Hspec.shouldBe` [0 .. Map.size (commentPlanPlacements plan) - 1]

parsePlan :: FilePath -> String -> IO CommentPlan
parsePlan filename source = do
  parsed <- parseModule ["-haddock"] filename
    (const $ pure $ Right ()) source
  case parsed of
    Left parseError -> fail parseError
    Right (annotations, _, ()) -> case normalizeCommentPlan annotations of
      Left planErrors -> fail $ show planErrors
      Right plan -> pure plan

annotationWithPrior :: EP.Comment -> EP.Annotation
annotationWithPrior comment = EP.Ann
  { EP.annCapturedSpan = Nothing
  , EP.annSortKey = Nothing
  , EP.annsDP = []
  , EP.annFollowingComments = []
  , EP.annPriorComments = [(comment, EP.DP (1, 0))]
  , EP.annEntryDelta = EP.DP (0, 0)
  }

sourceCommentAt :: FilePath -> Int -> String -> EP.Comment
sourceCommentAt filename line contents = EP.Comment Nothing
  (sourceSpan filename line 1 line (length contents + 1))
  contents

nodeKeyAt :: String -> Int -> EP.AnnKey
nodeKeyAt constructorName line = EP.AnnKey
  [sourceSpan "Ambiguous.hs" line 1 line 8]
  (EP.CN constructorName)

sourceSpan :: FilePath -> Int -> Int -> Int -> Int -> SrcLoc.SrcSpan
sourceSpan filename startLine startColumn endLine endColumn =
  EP.realSpanToSrcSpan $ SrcLoc.mkRealSrcSpan
    (SrcLoc.mkRealSrcLoc file startLine startColumn)
    (SrcLoc.mkRealSrcLoc file endLine endColumn)
 where
  file = FastString.mkFastString filename

expectedSource :: String
expectedSource = unlines
  [ "module CommentPlanExpected where"
  , ""
  , "-- | Converts a value."
  , "convert"
  , "  :: Int"
  , "  -- ^ input"
  , "  -> Bool"
  , "  -- ^ result"
  , "convert = (> 0)"
  , ""
  , "data Record = Record"
  , "  { field :: Int"
  , "             -- ^ field"
  , "  }"
  , "  deriving -- deriving note"
  , "           (Eq)"
  , ""
  , "following :: Int"
  , "following = 1"
  ]

edgeSource :: String
edgeSource = unlines
  [ "module CommentPlanEdge"
  , "  ( -- * Public API"
  , "    value"
  , "  )"
  , "where"
  , ""
  , "{-# INLINE value #-}"
  , "class Example a where"
  , "  method :: a -> a"
  , "  method item ="
  , "    let values ="
  , "          [ item -- same text"
  , "          , item"
  , "          ]"
  , "    in head values -- same text"
  , ""
  , "{- block comment -}"
  , "value :: Int"
  , "value = 1"
  ]
