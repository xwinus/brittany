{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Pattern.Record
  ( layoutRecordPattern
  ) where

import qualified Data.Text                               as Text
import           GHC                                      ( GenLocated(L) )
import           GHC.Hs
import           GHC.Types.SrcLoc                         ( Located )
import           Language.Haskell.Brittany.Internal.Delimiter.Types
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat
                                                         as ExactPrintCompat
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
          (patternSourceCommentPrecedesNode firstField) sourceComments
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
      children = zipWith
        (\index field@(located, _, _, _) ->
          ( Just $ ExactPrintCompat.mkAnnKey located
          , PresentDelimiterChild
          , recordFieldRow (index == 0) field
          )
        )
        [0 :: Int ..]
        rowFields
        ++ [ ( Nothing
             , RecordWildcardDelimiterChild
             , docLit $ Text.pack ".."
             )
           | dotdotEnabled
           ]
      layouts = [DelimiterAttached]
  docWrapNode (toL outer) $ do
    group <- docDelimitedSequence
      CurlyBracesDelimiter
      (Text.pack "{")
      (Text.pack "}")
      (Just $ ExactPrintCompat.mkAnnKey $ toL outer)
      children
      (replicate (max 0 $ length children - 1)
        (RepeatedDelimiterSeparator, Text.pack ",", AttachSeparatorRight))
      DelimiterIndentRegular
      RecordDelimiterFields
      layouts
    let body = case leadingComments of
          [] -> pure group
          comments -> docLines
            $ (layoutPatternSourceComment <$> comments) ++ [pure group]
    docAddBaseY BrIndentRegular
      $ docPar (pure nameDoc) $ docSetIndentLevel body

recordFieldRow
  :: Bool
  -> ( Located (HsFieldBind (LocatedA (FieldOcc GhcPs)) (LPat GhcPs))
     , Text.Text
     , Maybe (LPat GhcPs, PatternLayout)
     , [SourceComment]
     )
  -> ToBriDocM BriDocNumbered
recordFieldRow isFirst (field, fieldName, valueLayout, sourceComments) =
  (if isFirst then docWrapNodeRest field else docWrapNode field) $ docCols ColRec
    [ appSep $ docLit fieldName
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
