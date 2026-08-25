{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.TopLevelSpacing
  ( TopLevelUnit(..)
  , moduleKeywordSpan
  , moduleWhereSpan
  , preambleSeparatorLines
  , topLevelSeparatorLines
  , topLevelUnit
  ) where

import qualified Data.Maybe as Maybe
import Data.Kind (Type)
import GHC.Hs (AnnsModule(..), HsModule(..), hsmodAnn)
import GHC.Parser.Annotation (EpAnn(..), EpToken(..))
import GHC.Types.SrcLoc
  ( EpaLocation'(..)
  , RealSrcSpan
  , SrcSpan(..)
  , srcSpanEndCol
  , srcSpanEndLine
  , srcSpanStartLine
  )
import Language.Haskell.Brittany.Internal.ExactPrintCompat
  ( Annotation(..)
  , commentIdentifier
  , srcSpanToRealSpan
  )
import Language.Haskell.Brittany.Internal.Prelude

type TopLevelUnit :: Type
data TopLevelUnit = TopLevelUnit
  { topLevelNodeSpan :: RealSrcSpan
  , topLevelPriorSpans :: [RealSrcSpan]
  , topLevelFollowingSpans :: [RealSrcSpan]
  }
  deriving (Eq, Show)

topLevelUnit :: RealSrcSpan -> Maybe Annotation -> TopLevelUnit
topLevelUnit nodeSpan annotation = TopLevelUnit
  { topLevelNodeSpan = nodeSpan
  , topLevelPriorSpans = commentSpans $ maybe [] annPriorComments annotation
  , topLevelFollowingSpans =
      commentSpans $ maybe [] annFollowingComments annotation
  }
 where
  commentSpans = Maybe.mapMaybe
    (srcSpanToRealSpan . commentIdentifier . fst)

topLevelSeparatorLines :: TopLevelUnit -> TopLevelUnit -> Int
topLevelSeparatorLines previous next = max 1
  (effectiveStartLine next - effectiveEndLine previous)
 where
  effectiveStartLine unit = minimum
    $ srcSpanStartLine
    <$> (topLevelNodeSpan unit : topLevelPriorSpans unit)
  effectiveEndLine unit = maximum
    $ occupiedEndLine
    <$> (topLevelNodeSpan unit : topLevelFollowingSpans unit)

  occupiedEndLine span'
    | srcSpanEndCol span' == 1
    , srcSpanEndLine span' > srcSpanStartLine span'
    = srcSpanEndLine span' - 1
    | otherwise = srcSpanEndLine span'

-- | Preserve source-aware preamble spacing, capped at two blank lines.
preambleSeparatorLines :: RealSrcSpan -> TopLevelUnit -> Int
preambleSeparatorLines previous next = min 3 $ topLevelSeparatorLines
  (topLevelUnit previous Nothing)
  next

moduleWhereSpan :: HsModule GhcPs -> Maybe RealSrcSpan
moduleWhereSpan HsModule{hsmodExt = extension} =
  case hsmodAnn extension of
    EpAnn _ annotations _ -> case am_where annotations of
      EpTok (EpaSpan (RealSrcSpan span' _)) -> Just span'
      _ -> Nothing

moduleKeywordSpan :: HsModule GhcPs -> Maybe RealSrcSpan
moduleKeywordSpan HsModule{hsmodExt = extension} =
  case hsmodAnn extension of
    EpAnn _ annotations _ -> case am_mod annotations of
      EpTok (EpaSpan (RealSrcSpan span' _)) -> Just span'
      _ -> Nothing
