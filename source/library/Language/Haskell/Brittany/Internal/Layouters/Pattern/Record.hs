{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Pattern.Record
  ( layoutRecordPattern
  ) where

import qualified Data.Text                               as Text
import           GHC                                      ( GenLocated(L) )
import           GHC.Hs
import           GHC.Types.SrcLoc                         ( Located )
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Layouters.IE
                                                          ( toL )
import           Language.Haskell.Brittany.Internal.Layouters.Pattern.Comments
import           Language.Haskell.Brittany.Internal.Layouters.Pattern.Types
import           Language.Haskell.Brittany.Internal.Prelude
import           Language.Haskell.Brittany.Internal.SourceComment.Types
                                                          ( SourceComment )
import           Language.Haskell.Brittany.Internal.Types

layoutRecordPattern
  :: (LPat GhcPs -> ToBriDocM PatternLayout)
  -> LPat GhcPs
  -> LIdP GhcPs
  -> HsRecFields GhcPs (LPat GhcPs)
  -> ToBriDocM BriDocNumbered
layoutRecordPattern layoutChild outer name (HsRecFields _ fields dotdot) = do
  nameDoc <- docLit $ lrdrNameToText name
  fieldDocs <- fields `forM`
    \field@(L _ (HsFieldBind _ (L _ fieldOcc) value pun)) -> do
      let FieldOcc _ fieldName = fieldOcc
      valueLayout <- if pun
        then pure Nothing
        else Just . (,) value <$> layoutChild value
      pure (toL field, lrdrNameToText $ toL fieldName, valueLayout)
  sourceComments <- patternSourceCommentsWithinNode $ toL outer
  let leadingComments = case fieldDocs of
        [] -> []
        (firstField, _, _) : _ -> filter
          (patternSourceCommentPrecedesNode firstField)
          sourceComments
      fieldComments
        :: Located field -> LPat GhcPs -> [SourceComment]
      fieldComments field value = filter
        (\sourceComment ->
          patternSourceCommentWithinNodeSpan field sourceComment
            && patternSourceCommentPrecedesNode (toL value) sourceComment
        )
        sourceComments
      rowFields =
        [ ( field
          , fieldName
          , valueLayout
          , maybe [] (fieldComments field . fst) valueLayout
          )
        | (field, fieldName, valueLayout) <- fieldDocs
        ]
      dotdotEnabled = case dotdot of
        Just (L _ (RecFieldsDotDot index)) -> index == length fields
        Nothing -> False
      recordRows = case rowFields of
        [] -> if dotdotEnabled
          then [docSeq [docLit $ Text.pack "{", docLit $ Text.pack "..}"]]
          else [docLit $ Text.pack "{}"]
        firstField : remainingFields ->
          recordFieldRow (docLit $ Text.pack "{") firstField
            : [ recordFieldRow docCommaSep field
              | field <- remainingFields
              ]
            ++ (if dotdotEnabled
                  then [docSeq [docCommaSep, docLit $ Text.pack ".."]]
                  else []
               )
            ++ [docLit $ Text.pack "}"]
      rows = (layoutPatternSourceComment <$> leadingComments) ++ recordRows
  docWrapNode (toL outer)
    $ docAddBaseY BrIndentRegular
    $ docPar (pure nameDoc)
    $ docSetIndentLevel
    $ docLines rows

recordFieldRow
  :: ToBriDocM BriDocNumbered
  -> ( Located (HsFieldBind (LocatedA (FieldOcc GhcPs)) (LPat GhcPs))
     , Text.Text
     , Maybe (LPat GhcPs, PatternLayout)
     , [SourceComment]
     )
  -> ToBriDocM BriDocNumbered
recordFieldRow punctuation (field, fieldName, valueLayout, sourceComments) =
  docWrapNode field $ docCols ColRec
    [ appSep punctuation
    , appSep $ docLit fieldName
    , case valueLayout of
        Nothing -> docEmpty
        Just (_, layout) -> recordPatternFieldValue sourceComments layout
    ]

recordPatternFieldValue
  :: [SourceComment] -> PatternLayout -> ToBriDocM BriDocNumbered
recordPatternFieldValue sourceComments layout = do
  compactDocument <- patternCompactDocument layout
  selectedDocument <- patternDocument layout
  runFilteredAlternative $ do
    addAlternativeCond (null sourceComments) $ docSeq
      [ appSep $ docLit $ Text.pack "="
      , docForceSingleline $ pure compactDocument
      ]
    addAlternative $ docPar
      (docLit $ Text.pack "=")
      (docEnsureIndent BrIndentRegular
        $ docSetIndentLevel
        $ case sourceComments of
            [] -> pure selectedDocument
            _ -> docLines
              $ (layoutPatternSourceComment <$> sourceComments)
              ++ [pure selectedDocument]
      )
