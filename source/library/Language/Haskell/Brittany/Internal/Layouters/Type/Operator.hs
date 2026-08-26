{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Type.Operator
  ( layoutOperatorType
  ) where

import qualified Data.Text as Text
import GHC (Located, unLoc)
import GHC.Hs
import GHC.Types.Name.Occurrence (isSymOcc)
import GHC.Types.Name.Reader (rdrNameOcc)
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.IE (toL)
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types

layoutOperatorType
  :: (Located (HsType GhcPs) -> ToBriDocM BriDocNumbered)
  -> Located (HsType GhcPs)
  -> PromotionFlag
  -> LHsType GhcPs
  -> LIdOccP GhcPs
  -> LHsType GhcPs
  -> ToBriDocM BriDocNumbered
layoutOperatorType layoutOperand operatorType promotion left operator right = do
  leftDoc <- docSharedWrapper layoutOperand $ toL left
  rightDoc <- docSharedWrapper layoutOperand $ toL right
  operatorText <- lrdrNameToTextAnnTypeEqualityIsSpecial $ toL operator
  hasComments <- hasAnyCommentsBelow operatorType
  let adornedOperator = applyNameAdornment operator operatorText
      promotedOperator = applyPromotion promotion adornedOperator
      operatorDoc = docWrapNode (toL operator) $ docLit promotedOperator
      singleLine = docSeq
        [ docForceSingleline leftDoc
        , docSeparator
        , docForceSingleline operatorDoc
        , docSeparator
        , docForceSingleline rightDoc
        ]
      multiline = docPar (docSetIndentLevel leftDoc)
        $ docCols ColTyOpPrefix
        [ appSep operatorDoc
        , docSetBaseY rightDoc
        ]
  runFilteredAlternative $ do
    addAlternativeCond (not hasComments) singleLine
    addAlternative multiline
 where
  applyPromotion NotPromoted operatorName = operatorName
  applyPromotion IsPromoted operatorName
    | isSymOcc $ rdrNameOcc $ unLoc operator = Text.cons '\'' operatorName
    | Text.isPrefixOf (Text.pack "`") operatorName =
        Text.pack "`'" <> Text.drop 1 operatorName
    | otherwise = Text.cons '\'' operatorName
