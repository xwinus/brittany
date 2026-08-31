{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Delimiter.Validation
  ( validateRenderedDelimiter
  , validateSelectedDocument
  ) where

import Language.Haskell.Brittany.Internal.Delimiter.Types
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.Types

validateRenderedDelimiter
  :: DelimiterLayout
  -> DelimitedGroup document
  -> BriDocNumbered
  -> Either DelimiterInvariantError ()
validateRenderedDelimiter layout group document = case
    [ separator
    | separator <- delimiterSequenceSeparators sequence'
    , containsStandaloneToken (delimiterSeparatorToken separator) document
    ] of
  separator : _ -> Left $ StandaloneStructuralPunctuation
    (delimiterSequenceId sequence')
    layout
    (DelimiterSeparatorElement $ delimiterSeparatorId separator)
    (delimiterSeparatorToken separator)
  [] -> Right ()
 where
  sequence' = delimitedSequence group

validateSelectedDocument
  :: DelimiterLayout
  -> DelimitedGroup BriDoc
  -> BriDoc
  -> Either DelimiterInvariantError ()
validateSelectedDocument layout group document = case
    [ separator
    | separator <- delimiterSequenceSeparators sequence'
    , containsStandaloneBriDocToken
        (delimiterSeparatorToken separator) document
    ] of
  separator : _ -> Left $ StandaloneStructuralPunctuation
    (delimiterSequenceId sequence')
    layout
    (DelimiterSeparatorElement $ delimiterSeparatorId separator)
    (delimiterSeparatorToken separator)
  [] -> Right ()
 where
  sequence' = delimitedSequence group

containsStandaloneBriDocToken :: Text -> BriDoc -> Bool
containsStandaloneBriDocToken expected = \case
  BDLines lines -> any (onlyBriDocToken expected) lines
    || any (containsStandaloneBriDocToken expected) lines
  BDPar _ line indented -> containsStandaloneBriDocToken expected line
    || containsStandaloneBriDocToken expected indented
  BDSeq children -> any (containsStandaloneBriDocToken expected) children
  BDCols _ children -> any (containsStandaloneBriDocToken expected) children
  BDAddBaseY _ child -> containsStandaloneBriDocToken expected child
  BDBaseYPushCur child -> containsStandaloneBriDocToken expected child
  BDBaseYPop child -> containsStandaloneBriDocToken expected child
  BDIndentLevelPushCur child -> containsStandaloneBriDocToken expected child
  BDIndentLevelPop child -> containsStandaloneBriDocToken expected child
  BDAlt children -> any (containsStandaloneBriDocToken expected) children
  BDForwardLineMode child -> containsStandaloneBriDocToken expected child
  BDAnnotationPrior _ _ child -> containsStandaloneBriDocToken expected child
  BDAnnotationKW _ _ child -> containsStandaloneBriDocToken expected child
  BDAnnotationRest _ child -> containsStandaloneBriDocToken expected child
  BDMoveToKWDP _ _ _ child -> containsStandaloneBriDocToken expected child
  BDEnsureIndent _ child -> containsStandaloneBriDocToken expected child
  BDForceMultiline child -> containsStandaloneBriDocToken expected child
  BDForceSingleline child -> containsStandaloneBriDocToken expected child
  BDColumnsLimit _ child -> containsStandaloneBriDocToken expected child
  BDNonBottomSpacing _ child -> containsStandaloneBriDocToken expected child
  BDSetParSpacing child -> containsStandaloneBriDocToken expected child
  BDForceParSpacing child -> containsStandaloneBriDocToken expected child
  BDDebug _ child -> containsStandaloneBriDocToken expected child
  _ -> False

onlyBriDocToken :: Text -> BriDoc -> Bool
onlyBriDocToken expected = \case
  BDLit actual -> actual == expected
  BDSeq children -> case filter (not . isBriDocTokenSpacing) children of
    [child] -> onlyBriDocToken expected child
    _ -> False
  BDCols _ children -> case filter (not . isBriDocTokenSpacing) children of
    [child] -> onlyBriDocToken expected child
    _ -> False
  BDAddBaseY _ child -> onlyBriDocToken expected child
  BDBaseYPushCur child -> onlyBriDocToken expected child
  BDBaseYPop child -> onlyBriDocToken expected child
  BDIndentLevelPushCur child -> onlyBriDocToken expected child
  BDIndentLevelPop child -> onlyBriDocToken expected child
  BDEnsureIndent _ child -> onlyBriDocToken expected child
  _ -> False

isBriDocTokenSpacing :: BriDoc -> Bool
isBriDocTokenSpacing BDEmpty = True
isBriDocTokenSpacing BDSeparator = True
isBriDocTokenSpacing _ = False

containsStandaloneToken :: Text -> BriDocNumbered -> Bool
containsStandaloneToken expected (_, document) = case document of
  BDFLines lines -> any (onlyToken expected) lines
    || any (containsStandaloneToken expected) lines
  BDFPar _ line indented -> containsStandaloneToken expected line
    || containsStandaloneToken expected indented
  BDFSeq children -> any (containsStandaloneToken expected) children
  BDFCols _ children -> any (containsStandaloneToken expected) children
  BDFAddBaseY _ child -> containsStandaloneToken expected child
  BDFBaseYPushCur child -> containsStandaloneToken expected child
  BDFBaseYPop child -> containsStandaloneToken expected child
  BDFIndentLevelPushCur child -> containsStandaloneToken expected child
  BDFIndentLevelPop child -> containsStandaloneToken expected child
  BDFAlt children -> any (containsStandaloneToken expected) children
  BDFForwardLineMode child -> containsStandaloneToken expected child
  BDFAnnotationPrior _ _ child -> containsStandaloneToken expected child
  BDFAnnotationKW _ _ child -> containsStandaloneToken expected child
  BDFAnnotationRest _ child -> containsStandaloneToken expected child
  BDFMoveToKWDP _ _ _ child -> containsStandaloneToken expected child
  BDFEnsureIndent _ child -> containsStandaloneToken expected child
  BDFForceMultiline child -> containsStandaloneToken expected child
  BDFForceSingleline child -> containsStandaloneToken expected child
  BDFColumnsLimit _ child -> containsStandaloneToken expected child
  BDFNonBottomSpacing _ child -> containsStandaloneToken expected child
  BDFSetParSpacing child -> containsStandaloneToken expected child
  BDFForceParSpacing child -> containsStandaloneToken expected child
  BDFDebug _ child -> containsStandaloneToken expected child
  _ -> False

onlyToken :: Text -> BriDocNumbered -> Bool
onlyToken expected (_, document) = case document of
  BDFLit actual -> actual == expected
  BDFSeq children -> case filter (not . isTokenSpacing) children of
    [child] -> onlyToken expected child
    _ -> False
  BDFCols _ children -> case filter (not . isTokenSpacing) children of
    [child] -> onlyToken expected child
    _ -> False
  BDFAddBaseY _ child -> onlyToken expected child
  BDFBaseYPushCur child -> onlyToken expected child
  BDFBaseYPop child -> onlyToken expected child
  BDFIndentLevelPushCur child -> onlyToken expected child
  BDFIndentLevelPop child -> onlyToken expected child
  BDFEnsureIndent _ child -> onlyToken expected child
  _ -> False

isTokenSpacing :: BriDocNumbered -> Bool
isTokenSpacing (_, BDFEmpty) = True
isTokenSpacing (_, BDFSeparator) = True
isTokenSpacing _ = False
