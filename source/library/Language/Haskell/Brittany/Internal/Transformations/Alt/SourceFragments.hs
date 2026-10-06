{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Transformations.Alt.SourceFragments
  ( sourceFragmentSequenceSpacing
  ) where

import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.LineBoundary
  ( sourceFragmentRequiresLineBoundary )
import Language.Haskell.Brittany.Internal.SourceComment.Types (ExternalSource(..))
import Language.Haskell.Brittany.Internal.Types

-- A trailing source comment finishes its physical line only when another
-- document follows. Keep its width on that first line, and measure followers
-- independently rather than adding them to the comment's cursor.
sourceFragmentSequenceSpacing
  :: [BriDocNumbered]
  -> ([VerticalSpacing] -> VerticalSpacing)
  -> (VerticalSpacing -> VerticalSpacing)
  -> [VerticalSpacing]
  -> VerticalSpacing
sourceFragmentSequenceSpacing documents sumSpacing finishSpacing spacings
  | length documents /= length spacings = finishSpacing $ sumSpacing spacings
  | not $ any (== FragmentBoundary) endings = finishSpacing $ sumSpacing spacings
  | otherwise = case map sumSpacing $ segments $ zip endings spacings of
      [] -> finishSpacing $ sumSpacing []
      [single] -> finishSpacing single
      first : following -> first
        { _vs_paragraph = VerticalSpacingParSome $ maximum
            $ paragraphWidth first : map maximumWidth following
        , _vs_parFlag = False
        }
 where
  endings = map terminalContent documents

-- Separators are conditional spaces in the backend. Once a fragment requests
-- a newline, empty documents and pending separators do not start a new segment.
segments :: [(TerminalContent, VerticalSpacing)] -> [[VerticalSpacing]]
segments = go False [] []
 where
  go _ current previous [] = reverse $ finish current previous
  go pending current previous ((content, spacing) : remaining)
    | pending && content == NoContent = go pending current previous remaining
    | pending = go (content == FragmentBoundary) [spacing]
        (finish current previous) remaining
    | otherwise = go (content == FragmentBoundary) (spacing : current)
        previous remaining
  finish [] previous = previous
  finish current previous = reverse current : previous

paragraphWidth :: VerticalSpacing -> Int
paragraphWidth spacing = case _vs_paragraph spacing of
  VerticalSpacingParNone -> 0
  VerticalSpacingParSome width -> width
  VerticalSpacingParAlways width -> width

maximumWidth :: VerticalSpacing -> Int
maximumWidth spacing = max (_vs_sameLine spacing) $ paragraphWidth spacing

data TerminalContent = NoContent | FragmentBoundary | OtherContent
  deriving Eq

terminalContent :: BriDocNumbered -> TerminalContent
terminalContent (_, document) = case document of
  BDFExternal _ _ (SourceFragment fragment)
    | sourceFragmentRequiresLineBoundary fragment -> FragmentBoundary
  BDFEmpty -> NoContent
  BDFSeparator -> NoContent
  BDFSeq children -> lastContent children
  BDFCols _ children -> lastContent children
  BDFLines children -> lastContent children
  BDFPar _ line indented -> lastContent [line, indented]
  BDFAddBaseY _ child -> terminalContent child
  BDFBaseYPushCur child -> terminalContent child
  BDFBaseYPop child -> terminalContent child
  BDFIndentLevelPushCur child -> terminalContent child
  BDFIndentLevelPop child -> terminalContent child
  BDFForwardLineMode child -> terminalContent child
  BDFAnnotationPrior _ _ child -> terminalContent child
  BDFAnnotationKW _ _ child -> terminalContent child
  BDFAnnotationRest _ child -> terminalContent child
  BDFMoveToKWDP _ _ _ child -> terminalContent child
  BDFEnsureIndent _ child -> terminalContent child
  BDFForceMultiline child -> terminalContent child
  BDFForceSingleline child -> terminalContent child
  BDFColumnsLimit _ child -> terminalContent child
  BDFNonBottomSpacing _ child -> terminalContent child
  BDFSetParSpacing child -> terminalContent child
  BDFForceParSpacing child -> terminalContent child
  BDFDebug _ child -> terminalContent child
  BDFAlt alternatives -> case map terminalContent alternatives of
    [] -> NoContent
    contents | all (== NoContent) contents -> NoContent
    contents | all (== FragmentBoundary) contents -> FragmentBoundary
    _ -> OtherContent
  _ -> OtherContent

lastContent :: [BriDocNumbered] -> TerminalContent
lastContent = foldl' next NoContent
 where
  next previous document = case terminalContent document of
    NoContent -> previous
    content -> content
