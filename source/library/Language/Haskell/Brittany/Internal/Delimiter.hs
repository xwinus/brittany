{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Delimiter
  ( prepareSelectedDelimiter
  , delimiterDocument
  ) where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Generics.Uniplate.Direct as Uniplate
import Language.Haskell.Brittany.Internal.Delimiter.Types
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types

prepareSelectedDelimiter
  :: DelimitedGroup BriDoc
  -> Either DelimiterInvariantError (DelimiterLayout, BriDoc)
prepareSelectedDelimiter group = do
  validateDelimitedGroup group
  alternative <- selectedDelimiterAlternative group
  let layout = delimitedAlternativeLayout alternative
      spec = delimitedSpec group
      document = case layout of
        DelimiterAttached -> attachAfterOpenBoundary
          (delimiterOpenToken spec)
          (delimitedAlternativeDocument alternative)
        _ -> delimitedAlternativeDocument alternative
  if layout /= DelimiterVertical
      && startsWithStandaloneOpen (delimiterOpenToken spec) document
    then Left $ AccidentalStandaloneDelimiter
      layout
      (delimiterOpenToken spec)
    else Right (layout, document)

delimiterDocument
  :: DelimitedGroup BriDoc -> Either DelimiterInvariantError BriDoc
delimiterDocument = fmap snd . prepareSelectedDelimiter

attachAfterOpenBoundary :: Text -> BriDoc -> BriDoc
attachAfterOpenBoundary open document = State.evalState (visit document) initial
 where
  initial = BoundaryState False False
  visit current = do
    marked <- mark current
    Uniplate.descendM visit marked
  mark current = do
    state <- State.get
    case current of
      BDLit token
        | not (boundaryOpenSeen state)
        , token == open -> do
            State.put state { boundaryOpenSeen = True }
            pure current
      BDAnnotationPrior PriorCommentSource owner child
        | boundaryOpenSeen state
        , not (boundaryPriorAttached state) -> do
            State.put state { boundaryPriorAttached = True }
            pure $ BDAnnotationPrior PriorCommentInline owner child
      BDExternal{}
        | boundaryOpenSeen state
        , not (boundaryPriorAttached state) -> do
            State.put state { boundaryPriorAttached = True }
            pure current
      BDPlain{}
        | boundaryOpenSeen state
        , not (boundaryPriorAttached state) -> do
            State.put state { boundaryPriorAttached = True }
            pure current
      _ -> pure current

data BoundaryState = BoundaryState
  { boundaryOpenSeen :: Bool
  , boundaryPriorAttached :: Bool
  }

startsWithStandaloneOpen :: Text -> BriDoc -> Bool
startsWithStandaloneOpen open document = case stripTransparent document of
  BDLines (firstLine : _) -> onlyOpenToken open firstLine
  BDPar _ firstLine _ -> onlyOpenToken open firstLine
  _ -> False

onlyOpenToken :: Text -> BriDoc -> Bool
onlyOpenToken open = \case
  BDLit token -> token == open
  BDSeq documents -> case filter (not . isEmptyDocument) documents of
    [document] -> onlyOpenToken open document
    _ -> False
  BDCols _ documents -> case filter (not . isEmptyDocument) documents of
    [document] -> onlyOpenToken open document
    _ -> False
  BDAddBaseY _ document -> onlyOpenToken open document
  BDBaseYPushCur document -> onlyOpenToken open document
  BDBaseYPop document -> onlyOpenToken open document
  BDIndentLevelPushCur document -> onlyOpenToken open document
  BDIndentLevelPop document -> onlyOpenToken open document
  BDEnsureIndent _ document -> onlyOpenToken open document
  BDForceMultiline document -> onlyOpenToken open document
  BDForceSingleline document -> onlyOpenToken open document
  BDColumnsLimit _ document -> onlyOpenToken open document
  BDForwardLineMode document -> onlyOpenToken open document
  BDNonBottomSpacing _ document -> onlyOpenToken open document
  BDSetParSpacing document -> onlyOpenToken open document
  BDForceParSpacing document -> onlyOpenToken open document
  BDDebug _ document -> onlyOpenToken open document
  _ -> False

stripTransparent :: BriDoc -> BriDoc
stripTransparent = \case
  BDAddBaseY _ document -> stripTransparent document
  BDBaseYPushCur document -> stripTransparent document
  BDBaseYPop document -> stripTransparent document
  BDIndentLevelPushCur document -> stripTransparent document
  BDIndentLevelPop document -> stripTransparent document
  BDEnsureIndent _ document -> stripTransparent document
  BDForceMultiline document -> stripTransparent document
  BDForceSingleline document -> stripTransparent document
  BDColumnsLimit _ document -> stripTransparent document
  BDForwardLineMode document -> stripTransparent document
  BDNonBottomSpacing _ document -> stripTransparent document
  BDSetParSpacing document -> stripTransparent document
  BDForceParSpacing document -> stripTransparent document
  BDDebug _ document -> stripTransparent document
  document -> document

isEmptyDocument :: BriDoc -> Bool
isEmptyDocument = \case
  BDEmpty -> True
  BDSeq documents -> all isEmptyDocument documents
  BDCols _ documents -> all isEmptyDocument documents
  _ -> False
