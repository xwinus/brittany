{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Decl.Guard
  ( layoutGuardedBody
  , layoutGuardedHeadTail
  , layoutGuardedPredicates
  ) where

import qualified Data.Text as Text
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types

layoutGuardedBody
  :: BriDocNumbered
  -> BriDocNumbered
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
-- The guard has already advanced the cursor when this choice is resolved.
-- Keep a fitting RHS together before expanding it beside a long guard.
layoutGuardedBody binder body attached = docAlt
  [ docForceSingleline attached
  , docAddBaseY BrIndentRegular
    $ docPar (pure binder) (docForceSingleline $ pure body)
  , attached
  , docAddBaseY BrIndentRegular
    $ docPar (pure binder) (docNonBottomSpacing $ pure body)
  , docAddBaseY BrIndentRegular $ docPar docEmpty $ docAlt
    [ docForceSingleline attached
    , docPar (pure binder) (docNonBottomSpacing $ pure body)
    ]
  ]

layoutGuardedHeadTail
  :: [BriDocNumbered]
  -> ToBriDocM BriDocNumbered
  -> BriDocNumbered
  -> ToBriDocM BriDocNumbered
-- Include the separator when deciding whether the guard fits after the head.
layoutGuardedHeadTail guardDocs guards binder = do
  tailDoc <- docSeq [guards, pure binder]
  brokenTail <- docAlt
    [ docForceSingleline $ pure tailDoc
    , docSeq
      [ verticalGuardedPredicates guardDocs
      , docAlt
        [ pure binder
        , docAddBaseY BrIndentRegular $ docPar docEmpty (pure binder)
        ]
      ]
    ]
  docAlt
    [ docForceSingleline $ pure tailDoc
    , docAddBaseY BrIndentRegular $ docPar docEmpty (pure brokenTail)
    ]

layoutGuardedPredicates
  :: [BriDocNumbered]
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
layoutGuardedPredicates [] compact = compact
-- Try comma boundaries before breaking inside an individual predicate.
layoutGuardedPredicates guards compact = docAlt
  [ docForceSingleline compact
  , verticalGuardedPredicates guards
  ]

verticalGuardedPredicates :: [BriDocNumbered] -> ToBriDocM BriDocNumbered
verticalGuardedPredicates guards =
  docLines $ zipWith predicateLine ("|" : repeat ",") guards
 where
  predicateLine marker guard = docSeq
    [ appSep $ docLit $ Text.pack marker
    , appSep $ pure guard
    ]
