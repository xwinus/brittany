{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.ExtractAnns.GuardComments
  ( guardGroupComments
  ) where

import qualified Data.Generics as Generics
import qualified Data.List as List
import GHC (HsModule)
import GHC.Hs (GRHSs(..), LHsExpr)
import GHC.Parser.Annotation
  ( epaLocationRealSrcSpan, getFollowingComments, priorComments )
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.ExactPrintCompat
import Language.Haskell.Brittany.Internal.Prelude
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Utils as ExactPrint

-- GRHSs has an unlocated comment collection separate from each GRHS EpAnn.
-- The located-node annotation index cannot see comments stored only here.
-- Include every token kind so guard-run selection can protect documentation
-- and pragma continuations instead of treating their suffix as ordinary prose.
guardGroupComments :: HsModule GhcPs -> [(Comment, SrcLoc.RealSrcSpan)]
guardGroupComments = Generics.everything (++) $ Generics.mkQ [] fromGroup
 where
  fromGroup :: GRHSs GhcPs (LHsExpr GhcPs) -> [(Comment, SrcLoc.RealSrcSpan)]
  fromGroup group =
    [ ( Comment
          { commentOrigin = Nothing
          , commentIdentifier = realSpanToSrcSpan sourceSpan
          , commentContents = ExactPrint.commentContents comment
          }
      , sourceSpan
      )
    | comment <- List.concatMap ExactPrint.tokComment
        $ priorComments (grhssExt group) ++ getFollowingComments (grhssExt group)
    , let sourceSpan = epaLocationRealSrcSpan $ ExactPrint.commentLoc comment
    ]
