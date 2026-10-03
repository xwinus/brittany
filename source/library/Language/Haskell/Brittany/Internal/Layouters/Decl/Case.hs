{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Decl.Case
  ( layoutCasePatternHead
  , layoutCommentedCasePatternHead
  , layoutCaseDoBody
  ) where

import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types

layoutCaseDoBody
  :: BrIndent
  -> BriDocNumbered
  -> BriDocNumbered
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
layoutCaseDoBody bodyIndent headDocument body separatedBody = docSeq
  [ pure headDocument
  -- Resolve this choice at the end of the selected head. Paragraph spacing
  -- checks the do line independently of the statements that follow it.
  , docAlt
    [ docSeq
      [ docSeparator
      , docAddBaseY bodyIndent $ docForceParSpacing $ pure body
      ]
    , docPar docEmpty separatedBody
    ]
  ]

layoutCasePatternHead
  :: BriDocNumbered
  -> BriDocNumbered
  -> BriDocNumbered
  -> ToBriDocM BriDocNumbered
-- Select the complete head independently of the body. The structural fallback
-- must not reselect the compact pattern without reserving space for the arrow.
layoutCasePatternHead compact structural binder = docAlt
  [ docForceSingleline $ docSeq [appSep $ pure compact, pure binder]
  , docSeq
    [ appSep $ pure structural
    , docAlt
      [ pure binder
      , docAddBaseY BrIndentRegular $ docPar docEmpty $ pure binder
      ]
    ]
  ]

layoutCommentedCasePatternHead
  :: BriDocNumbered
  -> BriDocNumbered
  -> BriDocNumbered
  -> ToBriDocM BriDocNumbered
-- Give an overflowing arrow comment its own line before expanding the pattern.
layoutCommentedCasePatternHead compact structural commentedBinder = docAlt
  [ docForceSingleline $ docSeq [appSep $ pure compact, pure commentedBinder]
  , docLines
    [ docAlt [docForceSingleline $ pure compact, pure structural]
    , docEnsureIndent BrIndentRegular $ pure commentedBinder
    ]
  ]
