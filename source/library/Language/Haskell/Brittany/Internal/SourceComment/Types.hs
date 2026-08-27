{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.SourceComment.Types where

import qualified Data.Data
import GHC (RealSrcSpan, SrcSpan)
import Language.Haskell.Brittany.Internal.ExactPrintCompat (AnnKey)
import Language.Haskell.Brittany.Internal.Prelude

newtype SourceCommentKey = SourceCommentKey SrcSpan
  deriving (Data.Data.Data, Eq, Show)

instance Ord SourceCommentKey where
  compare left right = compare (show left) (show right)

newtype NodeId = NodeId AnnKey
  deriving (Data.Data.Data, Eq, Ord, Show)

data SourceCommentSyntax
  = LineComment
  | BlockComment
  deriving (Data.Data.Data, Eq, Ord, Show)

data CommentedNode
  = SignatureArgument
  | SignatureResult
  | RecordField
  deriving (Data.Data.Data, Eq, Ord, Show)

data CommentBoundary
  = DerivingClause
  | TypeOperator
  | ListChildren
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
  , placementRelativeOrder :: Int
  }
  deriving (Data.Data.Data, Eq, Ord, Show)

data CommentPlan = CommentPlan
  { commentPlanSources :: Map SourceCommentKey SourceComment
  , commentPlanPlacements :: Map SourceCommentKey CommentPlacement
  }
  deriving (Data.Data.Data, Eq, Show)

data CommentPlanError
  = AmbiguousCommentOwnership SourceCommentKey [NodeId]
  | AmbiguousCommentPlacement SourceCommentKey [CommentRole]
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
