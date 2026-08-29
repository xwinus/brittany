{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.Layouters.DataDecl.Boundary
  ( ConstructorLayout
  , buildConstructorLayout
  , constructorHasPriorComments
  , renderGadtConstructor
  , renderH98Constructor
  ) where

import qualified Data.Char                               as Char
import           Data.Kind                                ( Type )
import           GHC                                      ( GenLocated(L)
                                                          , Located
                                                          )
import           GHC.Hs                                   ( ConDecl )
import qualified GHC.Types.SrcLoc                        as SrcLoc
import           Language.Haskell.Brittany.Internal.ExactPrintCompat
                                                          ( Annotation
                                                            ( annPriorComments
                                                            )
                                                          , Comment
                                                            ( commentContents
                                                            , commentIdentifier
                                                            )
                                                          , mkAnnKey
                                                          , srcSpanToRealSpan
                                                          )
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Prelude
import           Language.Haskell.Brittany.Internal.Types

type ConstructorLayout :: Type
data ConstructorLayout = ConstructorLayout
  { constructorNode            :: Located (ConDecl GhcPs)
  , constructorLeadingComments :: ConstructorLeadingComments
  , constructorBody            :: ToBriDocM BriDocNumbered
  }

type ConstructorLeadingComments :: Type
data ConstructorLeadingComments
  = NoLeadingComments
  | LeadingHaddockInline
  | LeadingCommentsBeforeBoundary
  deriving Eq

buildConstructorLayout
  :: Located (ConDecl GhcPs)
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM ConstructorLayout
buildConstructorLayout constructor body = do
  leadingComments <- classifyLeadingComments constructor
  pure
    ConstructorLayout
      { constructorNode            = constructor
      , constructorLeadingComments = leadingComments
      , constructorBody            = body
      }

constructorHasPriorComments :: Located (ConDecl GhcPs) -> ToBriDocM Bool
constructorHasPriorComments constructor =
  (/= NoLeadingComments) <$> classifyLeadingComments constructor

renderH98Constructor :: String -> ConstructorLayout -> ToBriDocM BriDocNumbered
renderH98Constructor separator layout =
  case constructorLeadingComments layout of
    LeadingHaddockInline -> docWrapNodeRest node $ docLines
      [ docSeq
        [ docLitS separator
        , docSeparator
        , docAnnotationPriorInline (mkAnnKey node) docEmpty
        ]
      , docEnsureIndent BrIndentRegular body
      ]
    NoLeadingComments             -> renderWithCommentsBeforeBoundary
    LeadingCommentsBeforeBoundary -> renderWithCommentsBeforeBoundary
 where
  node = constructorNode layout
  body = constructorBody layout
  renderWithCommentsBeforeBoundary =
    docWrapNode node
      $ docSeq [docLitS separator, docSeparator, docSetBaseY body]

renderGadtConstructor :: ConstructorLayout -> ToBriDocM BriDocNumbered
renderGadtConstructor layout =
  docWrapNode (constructorNode layout) $ constructorBody layout

classifyLeadingComments
  :: Located (ConDecl GhcPs) -> ToBriDocM ConstructorLeadingComments
classifyLeadingComments constructor@(L constructorSpan _) =
  fmap
      (
        \case
          Just ann ->
            case
                filter (commentPrecedesConstructor constructorSpan . fst)
                       (annPriorComments ann)
              of
                (firstComment, _) : restComments
                  | isSingleLineHaddock firstComment
                  , all (isSingleLineComment . fst) restComments
                  -> LeadingHaddockInline
                  | otherwise
                  -> LeadingCommentsBeforeBoundary
                [] -> NoLeadingComments
          Nothing -> NoLeadingComments
      )
    $ astAnn constructor

commentPrecedesConstructor :: SrcLoc.SrcSpan -> Comment -> Bool
commentPrecedesConstructor constructorSpan sourceComment =
  case
      ( srcSpanToRealSpan $ commentIdentifier sourceComment
      , srcSpanToRealSpan constructorSpan
      )
    of
      (Just commentSpan, Just declarationSpan) ->
        realLocation (SrcLoc.realSrcSpanEnd commentSpan)
          <= realLocation (SrcLoc.realSrcSpanStart declarationSpan)
      _ -> False

realLocation :: SrcLoc.RealSrcLoc -> (Int, Int)
realLocation location = (SrcLoc.srcLocLine location, SrcLoc.srcLocCol location)

isSingleLineHaddock :: Comment -> Bool
isSingleLineHaddock priorComment =
  let contents = commentContents priorComment
      trimmed  = dropWhile Char.isSpace contents
  in  case trimmed of
        '-' : '-' : rest | '|' : _ <- dropWhile Char.isSpace rest ->
          isSingleLine contents
        _ -> False

isSingleLineComment :: Comment -> Bool
isSingleLineComment = isSingleLine . commentContents

isSingleLine :: String -> Bool
isSingleLine contents = '\n' `notElem` contents && '\r' `notElem` contents
