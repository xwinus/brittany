{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Expr.Application
  ( balancedHangingApplication
  , preferCompactRhs
  ) where

import Data.Semigroup (Last(..))
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Transformations.Alt (getSpacing)
import Language.Haskell.Brittany.Internal.Types

-- Try an enclosing break before fragmenting a RHS that can remain on one line.
preferCompactRhs
  :: Bool
  -> (ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered)
  -> (ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered)
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
preferCompactRhs allowCompact inline broken rhs = docAlt $
  [ candidate | allowCompact, candidate <-
      [inline $ docForceSingleline rhs, broken $ docForceSingleline rhs]
  ] ++
  [ inline $ docForceParSpacing rhs
  , broken rhs
  ]

-- Hanging arguments should have at least the function head's width available.
-- The ordinary fit check already reserves the widest argument. Reserve only
-- the difference, so long arguments and useful moderate alignment are unchanged.
balancedHangingApplication
  :: ToBriDocM BriDocNumbered
  -> [ToBriDocM BriDocNumbered]
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
balancedHangingApplication function arguments document = do
  headSpacing <- getSpacing =<< docForceSingleline function
  argumentSpacings <- mapM (getSpacing <=< docForceSingleline) arguments
  columns <- mAsk <&> _conf_layout .> _lconfig_cols .> confUnpack
  case (singleLineWidth headSpacing, traverse singleLineWidth argumentSpacings) of
    (Just headWidth, Just widths) ->
      let reserve = max 0 $ headWidth - maximum (0 : widths)
      in if reserve == 0
        then document
        else docColumnsLimit (columns - reserve) document
    _ -> document
 where
  singleLineWidth (LineModeValid (VerticalSpacing width VerticalSpacingParNone _)) =
    Just width
  singleLineWidth _ = Nothing
