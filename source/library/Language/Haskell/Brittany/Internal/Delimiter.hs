{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Delimiter
  ( delimiterLayoutDocuments
  , prepareSelectedDelimiter
  , delimiterDocument
  , validateRenderedDelimiter
  ) where

import qualified Control.Monad.Trans.State.Strict as State
import Language.Haskell.Brittany.Internal.Delimiter.Render (renderLayout)
import Language.Haskell.Brittany.Internal.Delimiter.Types
import Language.Haskell.Brittany.Internal.Delimiter.Validation
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types

delimiterLayoutDocuments
  :: Int
  -> DelimitedGroup BriDocNumbered
  -> Either DelimiterInvariantError [(DelimiterLayout, BriDocNumbered)]
delimiterLayoutDocuments seed group = do
  validateDelimitedGroup group
  case delimitedSelection group of
    SelectedDelimiter layout document -> pure [(layout, document)]
    UnselectedDelimiter -> do
      let layouts = delimitedAllowedLayouts group
          documents = State.evalState
            (traverse (`renderLayout` delimitedSequence group) layouts)
            (negate $ 1000000 + abs seed * 1000)
          alternatives = zip layouts documents
      alternatives `forM_` \(layout, document) ->
        validateRenderedDelimiter layout group document
      pure alternatives

prepareSelectedDelimiter
  :: DelimitedGroup BriDoc
  -> Either DelimiterInvariantError (DelimiterLayout, BriDoc)
prepareSelectedDelimiter group = do
  validateDelimitedGroup group
  selected@(layout, document) <- selectedDelimiterDocument group
  validateSelectedDocument layout group document
  pure selected

delimiterDocument
  :: DelimitedGroup BriDoc -> Either DelimiterInvariantError BriDoc
delimiterDocument = fmap snd . prepareSelectedDelimiter
