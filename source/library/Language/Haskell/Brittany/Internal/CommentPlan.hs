{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.CommentPlan
  ( normalizeCommentPlan
  , isSourceComment
  , lookupCommentPlacement
  , lookupCommentRole
  , commentPlanKeys
  , commentPlanFingerprint
  ) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.ExactPrintCompat
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types

data CommentSlot
  = PriorSlot
  | FollowingSlot
  | InnerSlot
  deriving (Eq, Ord, Show)

data CommentOccurrence = CommentOccurrence
  { occurrenceOwner :: NodeId
  , occurrenceSlot :: CommentSlot
  , occurrenceComment :: Comment
  }

normalizeCommentPlan :: Anns -> Either [CommentPlanError] CommentPlan
normalizeCommentPlan annotations = case
  invalidSpanErrors ++ ownershipErrors ++ placementErrors of
  [] -> Right CommentPlan
    { commentPlanSources = Map.fromList sourceEntries
    , commentPlanPlacements = Map.fromList placementEntries
    }
  errors -> Left errors
 where
  occurrences = filter (isSourceComment . occurrenceComment)
    $ List.concatMap annotationOccurrences $ Map.toAscList annotations
  grouped = Map.fromListWith (++)
    [ (commentKey comment, [occurrence])
    | occurrence <- occurrences
    , let comment = occurrenceComment occurrence
    ]
  invalidSpanErrors =
    [ InvalidSourceCommentSpan (commentContents comment) (commentIdentifier comment)
    | occurrence <- occurrences
    , let comment = occurrenceComment occurrence
    , Maybe.isNothing $ srcSpanToRealSpan $ commentIdentifier comment
    ]
  ownershipErrors =
    [ AmbiguousCommentOwnership key
        owners
    | (key, groupedOccurrences) <- Map.toAscList grouped
    , let owners = List.nub $ occurrenceOwner <$> groupedOccurrences
    , length owners > 1
    ]
  placementErrors =
    [ AmbiguousCommentPlacement key roles
    | (key, groupedOccurrences) <- Map.toAscList grouped
    , let owners = List.nub $ occurrenceOwner <$> groupedOccurrences
          roles = List.nub $ classifyComment annotations <$> groupedOccurrences
    , length owners == 1
    , length roles > 1
    ]
  uniqueOccurrences =
    [ (key, occurrence)
    | (key, groupedOccurrences) <- Map.toAscList grouped
    , occurrence : _ <- [groupedOccurrences]
    , length (List.nub $ occurrenceOwner <$> groupedOccurrences) == 1
    , length (List.nub $ classifyComment annotations <$> groupedOccurrences) == 1
    ]
  orderedOccurrences = List.sortOn (commentPosition . occurrenceComment . snd)
    uniqueOccurrences
  relativeOrders = Map.fromList
    [ (key, order)
    | (order, (key, _)) <- zip [0 ..] orderedOccurrences
    ]
  sourceEntries = Maybe.mapMaybe toSourceComment uniqueOccurrences
  placementEntries =
    [ ( key
      , CommentPlacement
          (occurrenceOwner occurrence)
          (classifyComment annotations occurrence)
          (Map.findWithDefault 0 key relativeOrders)
      )
    | (key, occurrence) <- uniqueOccurrences
    ]

annotationOccurrences :: (AnnKey, Annotation) -> [CommentOccurrence]
annotationOccurrences (ownerKey, annotation) =
  fmap (toOccurrence PriorSlot) (annPriorComments annotation)
    ++ fmap (toOccurrence FollowingSlot) (annFollowingComments annotation)
    ++ [ CommentOccurrence owner InnerSlot comment
       | (AnnComment comment, _) <- annsDP annotation
       ]
 where
  owner = NodeId ownerKey
  toOccurrence slot (comment, _) = CommentOccurrence owner slot comment

toSourceComment
  :: (SourceCommentKey, CommentOccurrence)
  -> Maybe (SourceCommentKey, SourceComment)
toSourceComment (key, occurrence) = do
  let comment = occurrenceComment occurrence
  realSpan <- srcSpanToRealSpan $ commentIdentifier comment
  syntax <- sourceCommentSyntaxFor comment
  pure
    ( key
    , SourceComment
        key
        (Text.pack $ commentContents comment)
        realSpan
        syntax
    )

classifyComment :: Anns -> CommentOccurrence -> CommentRole
classifyComment annotations occurrence
  | isPostDocText stripped = postDocRole
  | isPostDocContinuation
  , not (isLeadingDocText stripped)
  , not (isSectionText stripped)
  , not (isPragmaText stripped) = postDocRole
  | isSectionText stripped = SectionComment
  | isPragmaText stripped = PragmaComment
  | ownerConstructor == "HsDerivingClause"
      || commentInside "HsDerivingClause" = BetweenChildren DerivingClause
  | commentInside "HsOpTy" = BetweenChildren TypeOperator
  | isLeadingDocText stripped = LeadingDoc
  | occurrenceSlot occurrence == PriorSlot = LeadingOrdinary
  | isSameLineFollowing = TrailingSameLine
  | occurrenceSlot occurrence == InnerSlot = BetweenChildren ListChildren
  | otherwise = Unattached
 where
  comment = occurrenceComment occurrence
  stripped = dropWhile Char.isSpace $ commentContents comment
  NodeId ownerKey@(AnnKey _ ownerName) = occurrenceOwner occurrence
  ownerConstructor = unConName ownerName
  ownerInside constructorName = case annKeyRealSpan ownerKey of
    Nothing -> False
    Just ownerSpan -> any (containsSpan ownerSpan)
      $ annotationSpans constructorName annotations
  commentInside constructorName = case srcSpanToRealSpan $ commentIdentifier comment of
    Nothing -> False
    Just commentSpan -> any (containsSpan commentSpan)
      $ annotationSpans constructorName annotations
  isSameLineFollowing = occurrenceSlot occurrence == FollowingSlot
    && case (annKeyRealSpan ownerKey, srcSpanToRealSpan $ commentIdentifier comment) of
      (Just ownerSpan, Just commentSpan) ->
        SrcLoc.srcSpanEndLine ownerSpan == SrcLoc.srcSpanStartLine commentSpan
      _ -> False
  postDocRole = case ownerConstructor of
    "HsConDeclRecField" -> HaddockPostDoc RecordField
    "SigD" -> HaddockPostDoc SignatureResult
    _ | ownerInside "HsConDeclRecField" -> HaddockPostDoc RecordField
      | ownerInside "SigD" -> HaddockPostDoc SignatureArgument
      | ownerInside "TyClD" -> HaddockPostDoc SignatureArgument
      | otherwise -> Unattached
  isPostDocContinuation = case srcSpanToRealSpan $ commentIdentifier comment of
    Nothing -> False
    Just commentSpan -> any (endsOnPreviousLine commentSpan)
      [ previousComment
      | annotation <- Map.elems annotations
      , previousComment <- annotationSourceComments annotation
      , isPostDocText
          $ dropWhile Char.isSpace
          $ commentContents previousComment
      ]

annotationSourceComments :: Annotation -> [Comment]
annotationSourceComments annotation =
  fmap fst (annPriorComments annotation)
    ++ fmap fst (annFollowingComments annotation)
    ++ [comment | (AnnComment comment, _) <- annsDP annotation]

endsOnPreviousLine :: SrcLoc.RealSrcSpan -> Comment -> Bool
endsOnPreviousLine current previous = case
  srcSpanToRealSpan $ commentIdentifier previous of
  Nothing -> False
  Just previousSpan ->
    SrcLoc.srcSpanEndLine previousSpan + 1
      == SrcLoc.srcSpanStartLine current

annotationSpans :: String -> Anns -> [SrcLoc.RealSrcSpan]
annotationSpans constructorName annotations =
  [ span'
  | key@(AnnKey _ name) <- Map.keys annotations
  , unConName name == constructorName
  , Just span' <- [annKeyRealSpan key]
  ]

containsSpan :: SrcLoc.RealSrcSpan -> SrcLoc.RealSrcSpan -> Bool
containsSpan child parent = spanStart child >= spanStart parent
  && spanEnd child <= spanEnd parent

spanStart :: SrcLoc.RealSrcSpan -> (Int, Int)
spanStart span' =
  (SrcLoc.srcSpanStartLine span', SrcLoc.srcSpanStartCol span')

spanEnd :: SrcLoc.RealSrcSpan -> (Int, Int)
spanEnd span' =
  (SrcLoc.srcSpanEndLine span', SrcLoc.srcSpanEndCol span')

commentPosition :: Comment -> (String, Int, Int, Int, Int)
commentPosition comment = case srcSpanToRealSpan $ commentIdentifier comment of
  Nothing -> (show $ commentIdentifier comment, 0, 0, 0, 0)
  Just span' ->
    ( show $ SrcLoc.srcSpanFile span'
    , SrcLoc.srcSpanStartLine span'
    , SrcLoc.srcSpanStartCol span'
    , SrcLoc.srcSpanEndLine span'
    , SrcLoc.srcSpanEndCol span'
    )

sourceCommentSyntaxFor :: Comment -> Maybe SourceCommentSyntax
sourceCommentSyntaxFor comment = case dropWhile Char.isSpace
  $ commentContents comment of
  '-' : '-' : _ -> Just LineComment
  '#' : _ -> Just LineComment
  '{' : '-' : _ -> Just BlockComment
  _ -> Nothing

isSourceComment :: Comment -> Bool
isSourceComment = Maybe.isJust . sourceCommentSyntaxFor

commentKey :: Comment -> SourceCommentKey
commentKey = SourceCommentKey . commentIdentifier

lookupCommentPlacement :: CommentPlan -> Comment -> Maybe CommentPlacement
lookupCommentPlacement plan = (`Map.lookup` commentPlanPlacements plan) . commentKey

lookupCommentRole :: CommentPlan -> Comment -> Maybe CommentRole
lookupCommentRole plan comment = placementRole
  <$> lookupCommentPlacement plan comment

commentPlanKeys :: CommentPlan -> Set.Set SourceCommentKey
commentPlanKeys = Map.keysSet . commentPlanSources

commentPlanFingerprint :: CommentPlan -> [(Text.Text, SourceCommentSyntax, CommentRole, String)]
commentPlanFingerprint plan = List.sort $ deduplicateTransport
  $ List.sortOn sourceIdentity
  [ ( sourceCommentText sourceComment
    , sourceCommentSyntax sourceComment
    , fingerprintRole sourceComment placement
    , fingerprintOwner sourceComment placement
    )
  | (key, placement) <- Map.toList $ commentPlanPlacements plan
  , Just sourceComment <- [Map.lookup key $ commentPlanSources plan]
  ]
 where
  sourceIdentity (commentText, syntax, _, _) = (commentText, syntax)
  deduplicateTransport [] = []
  deduplicateTransport (entry : entries) = entry
    : deduplicateTransport (List.dropWhile (sameSourceComment entry) entries)
  sameSourceComment (leftText, leftSyntax, _, _)
      (rightText, rightSyntax, _, _) =
    leftText == rightText && leftSyntax == rightSyntax

  ownerConstructor (NodeId (AnnKey _ name)) = unConName name
  isBindingMember placement = any
    (`List.isInfixOf` ownerConstructor (placementOwner placement))
    [ "FunBind"
    , "BodyStmt"
    , "GRHS"
    , "HsFunTy"
    , "HsListTy"
    , "HsQualTy"
    , "HsSig"
    , "HsTyVar"
    , "HsValBinds"
    , "InvisPat"
    , "Match"
    , "PatBind"
    , "TuplePat"
    , "VarPat"
    , "WildPat"
    ]
  fingerprintRole sourceComment placement
    | sourceCommentSyntax sourceComment == BlockComment
    , placementRole placement `elem` [LeadingOrdinary, TrailingSameLine] =
        LeadingOrdinary
    | placementRole placement == TrailingSameLine = LeadingOrdinary
    | "SigD" `List.isInfixOf` ownerConstructor (placementOwner placement)
    , placementRole placement `elem`
        [LeadingOrdinary, TrailingSameLine, Unattached] = LeadingOrdinary
    | placementRole placement == Unattached
    , isBindingMember placement = LeadingOrdinary
    | otherwise = placementRole placement
  fingerprintOwner sourceComment placement
    | fingerprintRole sourceComment placement == LeadingOrdinary =
        "LeadingMember"
    | isBindingMember placement = "BindingMember"
    | otherwise = ownerConstructor $ placementOwner placement

isPostDocText :: String -> Bool
isPostDocText = \case
  '-' : '-' : rest -> startsWith '^' rest
  '{' : '-' : rest -> startsWith '^' rest
  _ -> False

isSectionText :: String -> Bool
isSectionText = \case
  '-' : '-' : rest -> startsWith '*' rest
  _ -> False

isLeadingDocText :: String -> Bool
isLeadingDocText = \case
  '-' : '-' : rest -> startsWith '|' rest
  '{' : '-' : rest -> startsWith '|' rest
  _ -> False

isPragmaText :: String -> Bool
isPragmaText = List.isPrefixOf "{-#"

startsWith :: Char -> String -> Bool
startsWith expected = \case
  rest -> case dropWhile Char.isSpace rest of
    actual : _ -> actual == expected
    [] -> False
