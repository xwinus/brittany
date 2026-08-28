{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.FixitySignature
  ( layoutFixitySignature
  ) where

import qualified Data.Text                               as Text
import           GHC                                      ( Located
                                                          , getLoc
                                                          )
import           GHC.Hs
import qualified GHC.OldList                             as List
import           GHC.Types.SrcLoc                         ( RealSrcSpan
                                                          , srcSpanEndCol
                                                          , srcSpanEndLine
                                                          , srcSpanStartCol
                                                          , srcSpanStartLine
                                                          )
import           Language.Haskell.Syntax.Basic            ( Fixity(..)
                                                          , FixityDirection(..)
                                                          )
import           Language.Haskell.Brittany.Internal.ExactPrintCompat
                                                          ( srcSpanToRealSpan )
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Layouters.IE
                                                          ( toL )
import           Language.Haskell.Brittany.Internal.Prelude
import           Language.Haskell.Brittany.Internal.Types

layoutFixitySignature
  :: Located (Sig GhcPs)
  -> FixitySig GhcPs
  -> Maybe (ToBriDocM BriDocNumbered)
layoutFixitySignature signature = \case
  FixitySig _ names@(_ : _) (Fixity precedence direction) -> Just
    $ docWrapNode signature
    $ do
        let nameDocs = flip fmap names $ \name -> docWrapNode (toL name) $ do
              nameText <- applyNameAdornment name <$> lrdrNameToTextAnn (toL name)
              docLit nameText
            prefixDocs =
              [ docLitS $ directionKeyword direction
              , docSeparator
              , docLitS $ show precedence
              , docSeparator
              ]
        commentGroups <- commentsBetweenNames names
        if all null commentGroups
          then docSeq $ prefixDocs ++ List.intersperse docCommaSep nameDocs
          else layoutCommentedNames prefixDocs nameDocs commentGroups
  _ -> Nothing

directionKeyword :: FixityDirection -> String
directionKeyword = \case
  InfixL -> "infixl"
  InfixR -> "infixr"
  InfixN -> "infix"

commentsBetweenNames
  :: [LIdP GhcPs] -> ToBriDocM [[String]]
commentsBetweenNames names = do
  OriginalSource source <- mAsk
  pure $ zipWith (commentsBetween source) names $ drop 1 names
 where
  commentsBetween source leftName rightName = case
      ( srcSpanToRealSpan $ getLoc $ toL leftName
      , srcSpanToRealSpan $ getLoc $ toL rightName
      )
    of
      (Just leftSpan, Just rightSpan) -> extractComments
        $ sourceBetween source leftSpan rightSpan
      _ -> []

layoutCommentedNames
  :: [ToBriDocM BriDocNumbered]
  -> [ToBriDocM BriDocNumbered]
  -> [[String]]
  -> ToBriDocM BriDocNumbered
layoutCommentedNames prefixDocs nameDocs commentGroups = case nameDocs of
  [] -> docSeq prefixDocs
  firstName : remainingNames -> docLines
    $ docSeq (prefixDocs ++ [firstName, docLitS ","])
    : List.concat
      (List.zipWith3
        layoutRemainingName
        commentGroups
        remainingNames
        hasFollowingName
      )
 where
  hasFollowingName = replicate (max 0 $ length nameDocs - 2) True ++ [False]

  layoutRemainingName commentTexts nameDoc hasFollowing =
    (docEnsureIndent BrIndentRegular . docLitS <$> commentTexts)
      ++ [ docEnsureIndent BrIndentRegular
         $ docSeq
         $ [nameDoc] ++ [docLitS "," | hasFollowing]
         ]

sourceBetween :: Text -> RealSrcSpan -> RealSrcSpan -> Text
sourceBetween source leftSpan rightSpan = case selectedLines of
  [] -> Text.empty
  [line] -> Text.take (rightColumn - leftColumn)
    $ Text.drop (leftColumn - 1) line
  firstLine : remainingLines -> case reverse remainingLines of
    [] -> Text.empty
    lastLine : reversedMiddle -> Text.intercalate (Text.singleton '\n')
      $ Text.drop (leftColumn - 1) firstLine
      : reverse reversedMiddle
      ++ [Text.take (rightColumn - 1) lastLine]
 where
  sourceLines = Text.splitOn (Text.singleton '\n') source
  leftLine = srcSpanEndLine leftSpan
  rightLine = srcSpanStartLine rightSpan
  leftColumn = srcSpanEndCol leftSpan
  rightColumn = srcSpanStartCol rightSpan
  selectedLines = take (rightLine - leftLine + 1)
    $ drop (leftLine - 1) sourceLines

extractComments :: Text -> [String]
extractComments = go . Text.unpack
 where
  go = \case
    [] -> []
    '-' : '-' : rest ->
      let (body, remaining) = break (== '\n') rest
      in ("--" ++ body) : go remaining
    '{' : '-' : rest ->
      let (body, remaining) = blockComment 1 "-{" rest
      in body : go remaining
    _ : rest -> go rest

  blockComment :: Int -> String -> String -> (String, String)
  blockComment depth accumulated = \case
    [] -> (reverse accumulated, [])
    '{' : '-' : rest -> blockComment (depth + 1) ("-{" ++ accumulated) rest
    '-' : '}' : rest
      | depth == 1 -> (reverse ("}-" ++ accumulated), rest)
      | otherwise -> blockComment (depth - 1) ("}-" ++ accumulated) rest
    character : rest -> blockComment depth (character : accumulated) rest
