{-# LANGUAGE LambdaCase #-}

module CommentIRSpec (spec) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified GHC.Data.FastString as FastString
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany
  ( parsePrintModule
  , staticDefaultConfig
  )
import Language.Haskell.Brittany.Internal.CommentIR
  ( CommentIRError(..)
  , lowerPlannedComments
  , planComment
  , plannedCommentKeys
  )
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as EP
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Types
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "first-class planned comments" $ do
  Hspec.it "lowers one comment with canonical metadata before alternatives" $ do
    case lowerPlannedComments annotations completePlan annotatedAlternative of
      Left errors -> Hspec.expectationFailure $ show errors
      Right lowered -> do
        plannedCommentKeys (unwrapBriDocNumbered lowered)
          `Hspec.shouldBe` [commentKey]
        case unwrapBriDocNumbered lowered of
          BDSeq
            [ BDComment planned
            , BDAnnotationPrior PriorCommentSource ownerKey'
                (BDAlt alternatives)
            ] -> do
              ownerKey' `Hspec.shouldBe` ownerKey
              length alternatives `Hspec.shouldBe` 2
              sourceCommentKey (plannedCommentSource planned)
                `Hspec.shouldBe` commentKey
              placementRole (plannedCommentPlacement planned)
                `Hspec.shouldBe` LeadingOrdinary
              plannedCommentBoundary planned `Hspec.shouldBe` commentBoundary
              plannedCommentIndentPolicy planned
                `Hspec.shouldBe` SourceColumnIndent
          _ -> Hspec.expectationFailure "unexpected lowered document shape"

  Hspec.it "rebases an own-line expression comment to its rendered anchor" $ do
    case planComment expressionPlan (comment, EP.DP (1, farSourceIndent)) of
      Left planError -> Hspec.expectationFailure $ show planError
      Right planned -> do
        plannedCommentIndentPolicy planned
          `Hspec.shouldBe` RenderedAnchorIndent
        plannedCommentColumnDelta planned `Hspec.shouldBe` 0

  Hspec.it "retains source-column placement for source directives" $ do
    case planComment directivePlan (comment, EP.DP (1, farSourceIndent)) of
      Left planError -> Hspec.expectationFailure $ show planError
      Right planned -> do
        plannedCommentIndentPolicy planned `Hspec.shouldBe` SourceColumnIndent
        plannedCommentColumnDelta planned `Hspec.shouldBe` farSourceIndent

  Hspec.it "keeps delimiter comments relative to their container" $ do
    case planComment delimiterPlan (comment, EP.DP (1, farSourceIndent)) of
      Left planError -> Hspec.expectationFailure $ show planError
      Right planned -> do
        plannedCommentIndentPolicy planned
          `Hspec.shouldBe` ContainerRelativeIndent
        plannedCommentColumnDelta planned `Hspec.shouldBe` farSourceIndent

  Hspec.it
    "rebases a far-column final record-field comment and remains stable"
    $ do
        firstPass <- formatChecked farColumnRecordSource
        secondPass <- formatChecked firstPass
        thirdPass <- formatChecked secondPass
        secondPass `Hspec.shouldBe` firstPass
        thirdPass `Hspec.shouldBe` firstPass
        commentIndent <- indentationOf "-- , disabledField = 3" firstPass
        siblingIndent <- indentationOf ", secondField = 2" firstPass
        commentIndent `Hspec.shouldBe` siblingIndent
        commentIndent `Hspec.shouldNotBe` farSourceIndent

  Hspec.it "rejects a planned comment without a canonical boundary" $ do
    case lowerPlannedComments
      annotations missingBoundaryPlan annotatedAlternative of
      Left [MissingCommentBoundary key] -> key `Hspec.shouldBe` commentKey
      Left errors -> Hspec.expectationFailure
        $ "unexpected lowering errors: " ++ show errors
      Right _ -> Hspec.expectationFailure "missing boundary was accepted"

formatChecked :: String -> IO String
formatChecked source =
  parsePrintModule staticDefaultConfig (Text.pack source) >>= \case
    Left errors ->
      Hspec.expectationFailure
          ("formatting returned " ++ show (length errors) ++ " errors")
        >> fail "formatting failed"
    Right output -> pure $ Text.unpack output

indentationOf :: String -> String -> IO Int
indentationOf needle source = case List.find (List.isInfixOf needle) $ lines source of
  Nothing -> Hspec.expectationFailure
    ("missing formatted line containing " ++ show needle)
    >> fail "missing formatted line"
  Just line -> pure $ length $ takeWhile (== ' ') line

completePlan :: CommentPlan
completePlan = basePlan
  { commentPlanBoundaries = Map.singleton commentKey commentBoundary
  }

expressionPlan :: CommentPlan
expressionPlan = planFor sourceComment expressionPlacement expressionBoundary

directivePlan :: CommentPlan
directivePlan = planFor
  (sourceComment { sourceCommentText = Text.pack "#if FLAG" })
  expressionPlacement
  commentBoundary

delimiterPlan :: CommentPlan
delimiterPlan = planFor sourceComment expressionPlacement CommentBoundaryId
  { commentBoundaryPath = DelimiterBoundaryPath 0
  , commentBoundaryGap = WithinBoundary
  }

expressionBoundary :: CommentBoundaryId
expressionBoundary = CommentBoundaryId
  { commentBoundaryPath = ExpressionBoundaryPath 0
  , commentBoundaryGap = WithinBoundary
  }

planFor
  :: SourceComment
  -> CommentPlacement
  -> CommentBoundaryId
  -> CommentPlan
planFor plannedSource placement boundary = CommentPlan
  { commentPlanSources = Map.singleton commentKey plannedSource
  , commentPlanPlacements = Map.singleton commentKey placement
  , commentPlanBoundaries = Map.singleton commentKey boundary
  }

missingBoundaryPlan :: CommentPlan
missingBoundaryPlan = basePlan { commentPlanBoundaries = Map.empty }

basePlan :: CommentPlan
basePlan = CommentPlan
  { commentPlanSources = Map.singleton commentKey sourceComment
  , commentPlanPlacements = Map.singleton commentKey commentPlacement
  , commentPlanBoundaries = Map.empty
  }

sourceComment :: SourceComment
sourceComment = SourceComment
  { sourceCommentKey = commentKey
  , sourceCommentText = Text.pack "-- disabled field"
  , sourceCommentSpan = commentRealSpan
  , sourceCommentSyntax = LineComment
  }

commentPlacement :: CommentPlacement
commentPlacement = CommentPlacement
  { placementOwner = NodeId ownerKey
  , placementRole = LeadingOrdinary
  , placementAnchor = BeforeNode
  , placementLineRelation = CommentOwnLine
  , placementRelativeOrder = 0
  }

expressionPlacement :: CommentPlacement
expressionPlacement = commentPlacement
  { placementOwner = NodeId expressionOwnerKey
  }

commentBoundary :: CommentBoundaryId
commentBoundary = CommentBoundaryId
  { commentBoundaryPath = DeclarationBoundaryPath 0
  , commentBoundaryGap = WithinBoundary
  }

annotations :: EP.Anns
annotations = Map.singleton ownerKey EP.Ann
  { EP.annCapturedSpan = Nothing
  , EP.annSortKey = Nothing
  , EP.annsDP = []
  , EP.annFollowingComments = []
  , EP.annPriorComments = [(comment, EP.DP (1, farSourceIndent))]
  , EP.annEntryDelta = EP.DP (0, 0)
  }

annotatedAlternative :: BriDocNumbered
annotatedAlternative =
  ( 0
  , BDFAnnotationPrior
      PriorCommentSource
      ownerKey
      ( 1
      , BDFAlt
          [ (2, BDFLit $ Text.pack "compact")
          , (3, BDFLines [(4, BDFLit $ Text.pack "multiline")])
          ]
      )
  )

ownerKey :: EP.AnnKey
ownerKey = EP.AnnKey [ownerSourceSpan] $ EP.CN "ValueDecl"

expressionOwnerKey :: EP.AnnKey
expressionOwnerKey = EP.AnnKey [ownerSourceSpan] $ EP.CN "HsVar"

comment :: EP.Comment
comment = EP.Comment Nothing commentSourceSpan "-- disabled field"

commentKey :: SourceCommentKey
commentKey = SourceCommentKey commentSourceSpan

ownerSourceSpan :: SrcLoc.SrcSpan
ownerSourceSpan = sourceSpan 2 1 2 8

commentSourceSpan :: SrcLoc.SrcSpan
commentSourceSpan = EP.realSpanToSrcSpan commentRealSpan

commentRealSpan :: SrcLoc.RealSrcSpan
commentRealSpan = realSpan 3 (farSourceIndent + 1) 3 62

sourceSpan :: Int -> Int -> Int -> Int -> SrcLoc.SrcSpan
sourceSpan startLine startColumn endLine endColumn =
  EP.realSpanToSrcSpan $ realSpan startLine startColumn endLine endColumn

realSpan :: Int -> Int -> Int -> Int -> SrcLoc.RealSrcSpan
realSpan startLine startColumn endLine endColumn = SrcLoc.mkRealSrcSpan
  (SrcLoc.mkRealSrcLoc testFile startLine startColumn)
  (SrcLoc.mkRealSrcLoc testFile endLine endColumn)
 where
  testFile = FastString.mkFastString "CommentIRSpec.hs"

farSourceIndent :: Int
farSourceIndent = 52

farColumnRecordSource :: String
farColumnRecordSource = unlines
  [ "module FarColumnRecord where"
  , ""
  , "data Settings = Settings"
  , "  { firstField :: Int"
  , "  , secondField :: Int"
  , "  }"
  , ""
  , "value ="
  , "  Settings"
  , "    { firstField = 1"
  , "    , secondField = 2"
  , replicate farSourceIndent ' ' ++ "-- , disabledField = 3"
  , "    }"
  ]
