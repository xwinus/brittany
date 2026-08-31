{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.SourceComment.Types where

import qualified Data.Data
import GHC (RealSrcSpan, SrcSpan)
import Language.Haskell.Brittany.Internal.ExactPrintCompat
  ( AnnKey
  , stripBufSpan
  )
import Language.Haskell.Brittany.Internal.Prelude

newtype SourceCommentKey = SourceCommentKey SrcSpan
  deriving (Data.Data.Data, Show)

instance Eq SourceCommentKey where
  left == right = compare left right == EQ

instance Ord SourceCommentKey where
  compare (SourceCommentKey left) (SourceCommentKey right) =
    compare (show $ stripBufSpan left) (show $ stripBufSpan right)

newtype NodeId = NodeId AnnKey
  deriving (Data.Data.Data, Eq, Ord, Show)

data SourceCommentSyntax
  = LineComment
  | BlockComment
  deriving (Data.Data.Data, Eq, Ord, Show)

data CommentedNode
  = DataConstructor
  | SignatureArgument
  | SignatureResult
  | RecordField
  deriving (Data.Data.Data, Eq, Ord, Show)

data CommentBoundary
  = DerivingClause
  | TypeOperator
  | ListChildren
  deriving (Data.Data.Data, Eq, Ord, Show)

data CommentBoundaryPath
  = ModuleBoundaryPath
  | ImportBoundaryPath String Int
  | DeclarationBoundaryPath Int
  | ConstructorBoundaryPath Int Int
  | ExpressionBoundaryPath Int
  | DelimiterBoundaryPath Int
  deriving (Data.Data.Data, Eq, Ord, Show)

data CommentBoundaryGap
  = BeforeBoundary
  | AfterOpenBoundary
  | WithinBoundary
  | BetweenBoundary
  | BeforeCloseBoundary
  | AfterLastBoundary
  deriving (Data.Data.Data, Eq, Ord, Show)

data CommentBoundaryId = CommentBoundaryId
  { commentBoundaryPath :: CommentBoundaryPath
  , commentBoundaryGap :: CommentBoundaryGap
  }
  deriving (Data.Data.Data, Eq, Ord, Show)

data CanonicalComment = CanonicalComment
  { canonicalCommentBoundary :: CommentBoundaryId
  , canonicalCommentText :: Text
  , canonicalCommentSyntax :: SourceCommentSyntax
  , canonicalCommentRole :: CommentRole
  }
  deriving (Data.Data.Data, Eq, Ord, Show)

data CommentRole
  = LeadingDoc
  | LeadingOrdinary
  | TrailingSameLine
  | HaddockPostDoc CommentedNode
  | BetweenChildren CommentBoundary
  | SectionComment
  | PragmaComment
  | Unattached
  deriving (Data.Data.Data, Eq, Ord, Show)

data CommentAnchor
  = BeforeNode
  | AfterNode
  | WithinNode
  deriving (Data.Data.Data, Eq, Ord, Show)

data CommentLineRelation
  = InlineComment
  | CommentOwnLine
  deriving (Data.Data.Data, Eq, Ord, Show)

data SourceComment = SourceComment
  { sourceCommentKey :: SourceCommentKey
  , sourceCommentText :: Text
  , sourceCommentSpan :: RealSrcSpan
  , sourceCommentSyntax :: SourceCommentSyntax
  }
  deriving (Data.Data.Data, Eq, Ord, Show)

data CommentPlacement = CommentPlacement
  { placementOwner :: NodeId
  , placementRole :: CommentRole
  , placementAnchor :: CommentAnchor
  , placementLineRelation :: CommentLineRelation
  , placementRelativeOrder :: Int
  }
  deriving (Data.Data.Data, Eq, Ord, Show)

data CommentIndentPolicy
  = OwnerRelativeIndent
  | RenderedAnchorIndent
  | ContainerRelativeIndent
  | TokenRelativeIndent
  | SourceColumnIndent
  deriving (Data.Data.Data, Eq, Ord, Show)

data PlannedComment = PlannedComment
  { plannedCommentSource :: SourceComment
  , plannedCommentPlacement :: CommentPlacement
  , plannedCommentBoundary :: CommentBoundaryId
  , plannedCommentIndentPolicy :: CommentIndentPolicy
  , plannedCommentLineDelta :: Int
  , plannedCommentColumnDelta :: Int
  }
  deriving (Data.Data.Data, Eq, Ord, Show)

data CommentPlan = CommentPlan
  { commentPlanSources :: Map SourceCommentKey SourceComment
  , commentPlanPlacements :: Map SourceCommentKey CommentPlacement
  , commentPlanBoundaries :: Map SourceCommentKey CommentBoundaryId
  }
  deriving (Data.Data.Data, Eq, Show)

data CommentPlanError
  = AmbiguousCommentOwnership SourceCommentKey [NodeId]
  | AmbiguousCommentPlacement
      SourceCommentKey
      [(CommentRole, CommentAnchor, CommentLineRelation)]
  | InvalidSourceCommentSpan String SrcSpan
  deriving (Data.Data.Data, Eq, Show)

data SourceRange = SourceRange
  { sourceRangeFile :: String
  , sourceRangeStartLine :: Int
  , sourceRangeStartColumn :: Int
  , sourceRangeEndLine :: Int
  , sourceRangeEndColumn :: Int
  }
  deriving (Data.Data.Data, Eq, Ord, Show)

data ExactSourceFragment = ExactSourceFragment
  { fragmentText :: Text
  , fragmentRange :: SourceRange
  , fragmentAnnotationKeys :: Set AnnKey
  , fragmentCommentKeys :: Set SourceCommentKey
  , fragmentAbsoluteColumn :: Maybe Int
  , fragmentRebaseContinuation :: Bool
  }
  deriving (Data.Data.Data, Eq, Ord, Show)

data ExternalSource
  = ExactPrintSource (Set AnnKey) Text
  | SourceFragment ExactSourceFragment
  deriving (Data.Data.Data, Eq, Ord, Show)

externalSourceText :: ExternalSource -> Text
externalSourceText = \case
  ExactPrintSource _ text -> text
  SourceFragment fragment -> fragmentText fragment

mapExternalSourceText :: (Text -> Text) -> ExternalSource -> ExternalSource
mapExternalSourceText mapText = \case
  ExactPrintSource keys text -> ExactPrintSource keys $ mapText text
  SourceFragment fragment -> SourceFragment fragment
    { fragmentText = mapText $ fragmentText fragment
    }
