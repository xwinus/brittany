{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.AnnotationIndex
  ( AnnotationIndex
  , AnnotationNode(..)
  , buildAnnotationIndex
  , fromAnnotationOnlyNodes
  , fromNodes
  , fromOverrides
  , indexNodeCount
  , indexNodes
  , indexOverrideCount
  , indexOverrides
  , indexSpanMap
  ) where

import Data.Data (Data)
import qualified Data.Foldable as Foldable
import qualified Data.Generics as SYB
import qualified Data.Kind as Kind
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import GHC.Parser.Annotation (EpAnnComments)
import GHC.Types.SrcLoc (RealSrcSpan)
import Language.Haskell.Brittany.Internal.ExactPrintCompat
  ( AnnKey
  , Annotation
  )
import Language.Haskell.Brittany.Internal.Prelude

type AnnotationNode :: Kind.Type
data AnnotationNode = AnnotationNode
  { annotationNodeKey :: !AnnKey
  , annotationNodeSpan :: !RealSrcSpan
  , annotationNodeComments :: !EpAnnComments
  , annotationNodeSupportsOwnership :: !Bool
  }

type AnnotationIndex :: Kind.Type
data AnnotationIndex = AnnotationIndex
  { annotationIndexNodes :: !(Seq AnnotationNode)
  , annotationIndexOverrides :: !(Seq (AnnKey, Annotation))
  }

instance Semigroup AnnotationIndex where
  AnnotationIndex leftNodes leftOverrides
    <> AnnotationIndex rightNodes rightOverrides = AnnotationIndex
      (leftNodes <> rightNodes)
      (leftOverrides <> rightOverrides)

instance Monoid AnnotationIndex where
  mempty = AnnotationIndex Seq.empty Seq.empty

buildAnnotationIndex
  :: Data ast
  => SYB.GenericQ AnnotationIndex
  -> ast
  -> AnnotationIndex
buildAnnotationIndex query = SYB.everything (<>) query

fromNodes :: [(AnnKey, RealSrcSpan, EpAnnComments)] -> AnnotationIndex
fromNodes nodes = AnnotationIndex
  (Seq.fromList
    [ AnnotationNode key sourceSpan comments True
    | (key, sourceSpan, comments) <- nodes
    ])
  Seq.empty

fromAnnotationOnlyNodes
  :: [(AnnKey, RealSrcSpan, EpAnnComments)] -> AnnotationIndex
fromAnnotationOnlyNodes nodes = AnnotationIndex
  (Seq.fromList
    [ AnnotationNode key sourceSpan comments False
    | (key, sourceSpan, comments) <- nodes
    ])
  Seq.empty

fromOverrides :: [(AnnKey, Annotation)] -> AnnotationIndex
fromOverrides overrides = AnnotationIndex Seq.empty $ Seq.fromList overrides

indexNodeCount :: AnnotationIndex -> Int
indexNodeCount = Seq.length . annotationIndexNodes

indexNodes :: AnnotationIndex -> [(AnnKey, RealSrcSpan, EpAnnComments)]
indexNodes = fmap toTuple . Foldable.toList . annotationIndexNodes
 where
  toTuple node =
    ( annotationNodeKey node
    , annotationNodeSpan node
    , annotationNodeComments node
    )

indexOverrideCount :: AnnotationIndex -> Int
indexOverrideCount = Seq.length . annotationIndexOverrides

indexOverrides :: AnnotationIndex -> [(AnnKey, Annotation)]
indexOverrides = Foldable.toList . annotationIndexOverrides

indexSpanMap
  :: (RealSrcSpan -> position)
  -> (RealSrcSpan -> position)
  -> AnnotationIndex
  -> Map.Map AnnKey (position, position)
indexSpanMap startPosition endPosition index = Map.fromList
  [ (annotationNodeKey node, (startPosition span', endPosition span'))
  | node <- Foldable.toList $ annotationIndexNodes index
  , annotationNodeSupportsOwnership node
  , let span' = annotationNodeSpan node
  ]
