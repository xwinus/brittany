{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.CommentIR
  ( CommentIRError(..)
  , lowerPlannedComments
  , planComment
  , plannedCommentKeys
  , validatePlannedCommentNodes
  ) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as StateS
import qualified Data.Generics.Uniplate.Direct as Uniplate
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany.Internal.Delimiter.Types
import Language.Haskell.Brittany.Internal.ExactPrintCompat
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Types
data CommentIRError
  = MissingPlannedComment SourceCommentKey
  | MissingCommentBoundary SourceCommentKey
  | DuplicatePlannedComment SourceCommentKey
  | UnconsumedCommentBoundary [SourceCommentKey]
  | AlternativeCommentMismatch [Set SourceCommentKey]
  deriving (Eq, Show)
data LowerState = LowerState
  { lowerClaimed :: Set SourceCommentKey
  , lowerExpected :: Set SourceCommentKey
  , lowerTransported :: Set SourceCommentKey
  , lowerCollecting :: Bool
  }
type LowerCache = Map (Int, Set SourceCommentKey, String)
  (BriDocNumbered, LowerState)
type LowerM = StateS.StateT LowerCache (Either [CommentIRError])
lowerPlannedComments
  :: Anns
  -> CommentPlan
  -> BriDocNumbered
  -> Either [CommentIRError] BriDocNumbered
lowerPlannedComments annotations plan document
  | Set.null expectedKeys = Right document
  | isSingleLanguagePragma plan expectedKeys = Right document
  | otherwise = case runLower collectingState of
      Left errors -> Left errors
      Right (_, discoveredState) ->
        let finalInitialState = LowerState
              { lowerClaimed = Set.empty
              , lowerExpected = expectedKeys
              , lowerTransported = lowerTransported discoveredState
              , lowerCollecting = False
              }
        in case runLower finalInitialState of
          Left errors -> Left errors
          Right (lowered, finalState) -> case Set.toList
            $ expectedKeys `Set.difference` lowerClaimed finalState of
            [] -> Right lowered
            missing -> Left [UnconsumedCommentBoundary missing]
 where
  expectedKeys = annotationPlanKeys annotations plan
  runLower initialState = StateS.evalStateT
    (lowerNode annotations plan initialState document)
    Map.empty
  collectingState = LowerState
    { lowerClaimed = Set.empty
    , lowerExpected = expectedKeys
    , lowerTransported = Set.empty
    , lowerCollecting = True
    }
plannedCommentKeys :: BriDoc -> [SourceCommentKey]
plannedCommentKeys = Uniplate.universe >=> \case
  BDComment planned -> [sourceCommentKey $ plannedCommentSource planned]
  _ -> []
validatePlannedCommentNodes :: BriDoc -> Either [CommentIRError] ()
validatePlannedCommentNodes document = case duplicateKeys of
  [] -> Right ()
  duplicates -> Left $ DuplicatePlannedComment <$> duplicates
 where
  keys = List.sort $ plannedCommentKeys document
  duplicateKeys =
    [ key
    | key : _ <- filter ((> 1) . length) $ List.group keys
    ]
annotationPlanKeys :: Anns -> CommentPlan -> Set SourceCommentKey
annotationPlanKeys annotations plan = Set.fromList
  [ key
  | annotation <- Map.elems annotations
  , comment <- annotationComments annotation
  , let key = SourceCommentKey $ commentIdentifier comment
  , Map.member key $ commentPlanSources plan
  ]
isSingleLanguagePragma :: CommentPlan -> Set SourceCommentKey -> Bool
isSingleLanguagePragma plan keys = case Set.toList keys of
  [key] -> case
      ( Map.lookup key $ commentPlanSources plan
      , Map.lookup key $ commentPlanPlacements plan
      ) of
    (Just source, Just placement) -> placementRole placement == PragmaComment
      && Text.isPrefixOf (Text.pack "{-# LANGUAGE")
        (Text.stripStart $ sourceCommentText source)
      && case placementOwner placement of
        NodeId (AnnKey _ constructor) -> unConName constructor == "HsModule"
    _ -> False
  _ -> False
annotationComments :: Annotation -> [Comment]
annotationComments annotation =
  (fst <$> annPriorComments annotation)
    ++ (fst <$> annFollowingComments annotation)
    ++ [comment | (AnnComment comment, _) <- annsDP annotation]

lowerNode
  :: Anns
  -> CommentPlan
  -> LowerState
  -> BriDocNumbered
  -> LowerM (BriDocNumbered, LowerState)
lowerNode annotations plan state numbered@(nodeId, document)
  | not (lowerCollecting state)
  , lowerExpected state `Set.isSubsetOf` lowerClaimed state =
      pure (numbered, state)
  | nodeId < 0 = compute
  | Nothing <- cacheDiscriminator document = compute
  | Just discriminator <- cacheDiscriminator document = do
      cache <- StateS.get
      let cacheKey = (nodeId, lowerClaimed state, discriminator)
      case Map.lookup cacheKey cache of
        Just cached -> pure cached
        Nothing -> do
          lowered <- compute
          StateS.modify $ Map.insert cacheKey lowered
          pure lowered
 where
  compute = case document of
    BDFEmpty -> leaf BDFEmpty
    BDFBlankLine -> leaf BDFBlankLine
    BDFLit text -> leaf $ BDFLit text
    BDFSeq children -> childrenNode BDFSeq children
    BDFCols signature children -> childrenNode (BDFCols signature) children
    BDFSeparator -> leaf BDFSeparator
    BDFAddBaseY indent child -> childNode (BDFAddBaseY indent) child
    BDFBaseYPushCur child -> childNode BDFBaseYPushCur child
    BDFBaseYPop child -> childNode BDFBaseYPop child
    BDFIndentLevelPushCur child -> childNode BDFIndentLevelPushCur child
    BDFIndentLevelPop child -> childNode BDFIndentLevelPop child
    BDFPar indent line indented -> do
      (line', state') <- lowerNode annotations plan state line
      (indented', state'') <- lowerNode annotations plan state' indented
      pure ((nodeId, BDFPar indent line' indented'), state'')
    BDFDelimited group -> alternativeNode (BDFDelimited . replace group)
      $ delimitedAlternativeDocument <$> delimitedAlternatives group
    BDFAlt alternatives -> alternativeNode BDFAlt alternatives
    BDFForwardLineMode child -> childNode BDFForwardLineMode child
    BDFExternal key addComment source -> case source of
      SourceFragment fragment -> do
        let transported = fragmentCommentKeys fragment
            claimed = lowerClaimed state <> transported
        pure
          ( (nodeId, BDFExternal key addComment source)
          , state
              { lowerClaimed = claimed
              , lowerTransported = lowerTransported state <> transported
              }
          )
      ExactPrintSource annotationKeys _ ->
        let transported = Set.fromList
              [ commentKey
              | annotationKey <- Set.toList annotationKeys
              , Just annotation <- [Map.lookup annotationKey annotations]
              , comment <- annotationComments annotation
              , let commentKey = SourceCommentKey $ commentIdentifier comment
              , Map.member commentKey $ commentPlanSources plan
              ]
            claimed = lowerClaimed state <> transported
        in pure
          ( (nodeId, BDFExternal key addComment source)
          , state
              { lowerClaimed = claimed
              , lowerTransported = lowerTransported state <> transported
              }
          )
    BDFPlain canHang text -> leaf $ BDFPlain canHang text
    BDFAnnotationPrior mode key child -> do
      (child', state') <- lowerNode annotations plan state child
      (comments, state'') <- plannedNodes state' $ priorComments key
      if null comments
        then pure ((nodeId, BDFAnnotationPrior mode key child'), state'')
        else pure
          ( ( -200000 - nodeId
            , BDFSeq $ comments ++ [(nodeId, BDFAnnotationPrior mode key child')]
            )
          , state''
          )
    BDFAnnotationKW key keyword child -> do
      (child', state') <- lowerNode annotations plan state child
      (comments, state'') <- plannedNodes state' $ keywordComments key keyword
      wrapSequence [] (nodeId, BDFAnnotationKW key keyword child')
        comments state''
    BDFAnnotationRest key child -> do
      (child', state') <- lowerNode annotations plan state child
      (comments, state'') <- plannedNodes state' $ restComments key
      wrapSequence [] (nodeId, BDFAnnotationRest key child') comments state''
    BDFMoveToKWDP key keyword restore child ->
      childNode (BDFMoveToKWDP key keyword restore) child
    BDFLines children -> childrenNode BDFLines children
    BDFEnsureIndent indent child -> childNode (BDFEnsureIndent indent) child
    BDFForceMultiline child -> childNode BDFForceMultiline child
    BDFForceSingleline child -> childNode BDFForceSingleline child
    BDFColumnsLimit limit child -> childNode (BDFColumnsLimit limit) child
    BDFNonBottomSpacing spacing child ->
      childNode (BDFNonBottomSpacing spacing) child
    BDFSetParSpacing child -> childNode BDFSetParSpacing child
    BDFForceParSpacing child -> childNode BDFForceParSpacing child
    BDFDebug label child -> childNode (BDFDebug label) child
    BDFComment planned ->
      let key = sourceCommentKey $ plannedCommentSource planned
      in if Set.member key $ lowerClaimed state
        then throwLower [DuplicatePlannedComment key]
        else pure
          ( (nodeId, BDFComment planned)
          , state { lowerClaimed = Set.insert key $ lowerClaimed state }
          )
  leaf value = pure ((nodeId, value), state)
  childNode constructor child = do
    (child', state') <- lowerNode annotations plan state child
    pure ((nodeId, constructor child'), state')
  childrenNode constructor children = do
    (children', state') <- lowerChildren state children
    pure ((nodeId, constructor children'), state')
  lowerChildren current = \case
    [] -> pure ([], current)
    child : rest -> do
      (child', next) <- lowerNode annotations plan current child
      (rest', final) <- lowerChildren next rest
      pure (child' : rest', final)
  alternativeNode constructor alternatives = do
    lowered <- traverse (lowerNode annotations plan state) alternatives
    let coverages = lowerClaimed . snd <$> lowered
        distinctCoverages = List.nub coverages
        maximalCoverages =
          [ candidate
          | candidate <- distinctCoverages
          , not $ any (candidate `Set.isProperSubsetOf`) distinctCoverages
          ]
    case maximalCoverages of
      [] -> pure ((nodeId, constructor []), state)
      [target] -> case
          [ loweredAlternative
          | (loweredAlternative, coverage) <- zip lowered coverages
          , coverage == target
          ] of
        [] -> throwLower [AlternativeCommentMismatch coverages]
        selected@((_, selectedState) : _) -> pure
          ( (nodeId, constructor $ fst <$> selected)
          , selectedState
          )
      _ -> throwLower [AlternativeCommentMismatch coverages]
  replace group alternatives = replaceDelimitedDocuments alternatives group
  plannedNodes current comments = Monad.foldM add ([], current)
    $ relativeCommentDeltas plan comments
  add (nodes, current) commentWithDelta@(comment, _) =
    let key = SourceCommentKey $ commentIdentifier comment
    in if Set.member key (lowerClaimed current)
        || Set.member key (lowerTransported current)
      then pure (nodes, current)
      else case planComment plan commentWithDelta of
        Right planned ->
          let plannedId = -100000
                - placementRelativeOrder (plannedCommentPlacement planned)
          in pure
            ( nodes ++ [(plannedId, BDFComment planned)]
            , current
                { lowerClaimed = Set.insert key $ lowerClaimed current }
            )
        Left commentError -> throwLower [commentError]
  wrapSequence before wrapped after current
    | null before, null after = pure (wrapped, current)
    | otherwise = pure
        ((-200000 - nodeId, BDFSeq $ before ++ [wrapped] ++ after), current)
  priorComments key = maybe [] annPriorComments $ Map.lookup key annotations
  restComments key = maybe [] followingAndInner $ Map.lookup key annotations
  followingAndInner annotation = annFollowingComments annotation
    ++ [ (comment, delta)
       | (AnnComment comment, delta) <- annsDP annotation
       ]
  keywordComments key keyword = case annsDP <$> Map.lookup key annotations of
    Nothing -> []
    Just entries -> takeComments $ case keyword of
      Nothing -> entries
      Just expected -> case dropWhile ((/= G expected) . fst) entries of
        [] -> []
        _ : remaining -> remaining
  takeComments = fmap fromComment . takeWhile isComment
  isComment (AnnComment{}, _) = True
  isComment _ = False
  fromComment (AnnComment comment, delta) = (comment, delta)
  fromComment _ = error "takeComments retained a non-comment annotation"
  throwLower errors = StateS.StateT $ const $ Left errors
cacheDiscriminator :: BriDocFInt -> Maybe String
cacheDiscriminator = \case
  BDFSeq children -> structural "seq" children
  BDFCols signature children -> structural ("cols:" ++ show signature) children
  BDFAlt alternatives -> structural "alt" alternatives
  BDFDelimited group -> structural "delimited"
    $ delimitedAlternativeDocument <$> delimitedAlternatives group
  _ -> Nothing
 where
  structural label children = Just $ label ++ ":" ++ show (fst <$> children)

relativeCommentDeltas
  :: CommentPlan -> [(Comment, DeltaPos)] -> [(Comment, DeltaPos)]
relativeCommentDeltas plan = \case
  [] -> []
  (comment, DP (rawLineDelta, columnDelta)) : remaining ->
    (comment, DP (ownerLineDelta comment rawLineDelta, columnDelta))
      : go comment remaining
 where
  go _ [] = []
  go previous ((current, DP (_, columnDelta)) : rest) =
    (current, DP (lineDelta previous current, columnDelta))
      : go current rest
  lineDelta previous current = fromMaybe 1 $ do
    previousSpan <- srcSpanToRealSpan $ commentIdentifier previous
    currentSpan <- srcSpanToRealSpan $ commentIdentifier current
    guard $ SrcLoc.srcSpanFile previousSpan == SrcLoc.srcSpanFile currentSpan
    pure $ max 1
      $ SrcLoc.srcSpanStartLine currentSpan - SrcLoc.srcSpanEndLine previousSpan
  ownerLineDelta comment fallback = fromMaybe fallback $ do
    placement <- Map.lookup
      (SourceCommentKey $ commentIdentifier comment)
      (commentPlanPlacements plan)
    guard $ placementAnchor placement /= AfterNode
    NodeId owner <- pure $ placementOwner placement
    ownerSpan <- annKeyRealSpan owner
    commentSpan <- srcSpanToRealSpan $ commentIdentifier comment
    guard $ SrcLoc.srcSpanFile ownerSpan == SrcLoc.srcSpanFile commentSpan
    guard $ SrcLoc.srcSpanStartLine commentSpan > SrcLoc.srcSpanEndLine ownerSpan
    pure $ SrcLoc.srcSpanStartLine commentSpan - SrcLoc.srcSpanEndLine ownerSpan

planComment
  :: CommentPlan -> (Comment, DeltaPos) -> Either CommentIRError PlannedComment
planComment plan (comment, DP (lineDelta, columnDelta)) = case
  Map.lookup key $ commentPlanSources plan of
  Nothing -> Left $ MissingPlannedComment key
  Just source -> planSourceCommentWithDelta plan source lineDelta columnDelta
 where
  key = SourceCommentKey $ commentIdentifier comment

planSourceCommentWithDelta
  :: CommentPlan
  -> SourceComment
  -> Int
  -> Int
  -> Either CommentIRError PlannedComment
planSourceCommentWithDelta plan source lineDelta columnDelta = case
    ( Map.lookup key $ commentPlanSources plan
    , Map.lookup key $ commentPlanPlacements plan
    , Map.lookup key $ commentPlanBoundaries plan
    ) of
  (Just plannedSource, Just originalPlacement, Just boundary) ->
    let rolePlacement = if leadingDocContinuation plan plannedSource boundary
          then originalPlacement { placementRole = HaddockPostDoc DataConstructor }
          else originalPlacement
        boundaryOwnLine = (commentBoundaryGap boundary
          `elem` [BeforeBoundary, BetweenBoundary]
          || case (commentBoundaryPath boundary, commentBoundaryGap boundary) of
            (ConstructorBoundaryPath{}, AfterLastBoundary) -> True
            _ -> False
          )
          && placementAnchor rolePlacement == BeforeNode
          && fromMaybe False (do
            NodeId owner <- pure $ placementOwner rolePlacement
            ownerSpan <- annKeyRealSpan owner
            pure $ SrcLoc.srcSpanEndLine (sourceCommentSpan plannedSource)
              < SrcLoc.srcSpanStartLine ownerSpan
          )
        placement = if boundaryOwnLine
          then rolePlacement { placementLineRelation = CommentOwnLine }
          else rolePlacement
        baseLineDelta = lineDelta
        inlineConstructorDoc = placementRole placement == LeadingDoc
          && case placementOwner placement of
            NodeId (AnnKey _ constructor) -> unConName constructor
              == "ConDeclH98"
          && length (Text.lines $ sourceCommentText plannedSource) == 1
        effectivePlacement = if inlineConstructorDoc
          then placement { placementLineRelation = InlineComment }
          else placement
        effectiveLineDelta
          | inlineConstructorDoc = 0
          | placementRole placement == BetweenChildren TypeOperator = 1
          | statementOwnerRelative placement = min 1 baseLineDelta
          | otherwise = baseLineDelta
        effectiveColumnDelta = if inlineConstructorDoc
          then 1
          else if placementLineRelation placement == InlineComment
              && ((case placementOwner placement of
                NodeId (AnnKey _ constructor) -> unConName constructor
                  `elem` ["ConDeclH98", "ConDeclGADT"])
                || case (commentBoundaryPath boundary, commentBoundaryGap boundary) of
                  (ConstructorBoundaryPath{}, AfterLastBoundary) -> True
                  _ -> False)
            then max 1 $ columnDelta - 1
          else if placementRole placement == BetweenChildren TypeOperator
              && case commentBoundaryPath boundary of
                ConstructorBoundaryPath{} -> True
                _ -> False
            then 0
          else if placementRole placement == BetweenChildren DerivingClause
              && placementAnchor placement == BeforeNode
            then 0
          else if structuralOwnerRelative placement
            then 0
          else if statementOwnerRelative placement
            then fromMaybe 0 $ do
              NodeId owner <- pure $ placementOwner placement
              ownerSpan <- annKeyRealSpan owner
              pure $ max 0
                $ SrcLoc.srcSpanStartCol (sourceCommentSpan plannedSource)
                - SrcLoc.srcSpanStartCol ownerSpan
          else if placementLineRelation placement == CommentOwnLine
              && baseLineDelta > 0
            then SrcLoc.srcSpanStartCol (sourceCommentSpan plannedSource) - 1
            else columnDelta
    in Right $ PlannedComment
      plannedSource
      effectivePlacement
      boundary
      (if inlineConstructorDoc
        then OwnerRelativeIndent
        else indentPolicy baseLineDelta plannedSource effectivePlacement boundary
      )
      effectiveLineDelta
      effectiveColumnDelta
  (Nothing, _, _) -> Left $ MissingPlannedComment key
  (_, Nothing, _) -> Left $ MissingPlannedComment key
  (_, _, Nothing) -> Left $ MissingCommentBoundary key
 where
  key = sourceCommentKey source

indentPolicy
  :: Int
  -> SourceComment
  -> CommentPlacement
  -> CommentBoundaryId
  -> CommentIndentPolicy
indentPolicy lineDelta source placement boundary
  | Text.isPrefixOf (Text.singleton '#')
      (Text.stripStart $ sourceCommentText source) = SourceColumnIndent
  | placementLineRelation placement /= InlineComment
  , case placementRole placement of
      HaddockPostDoc{} -> True
      _ -> False = SourceColumnIndent
  | placementLineRelation placement == InlineComment = TokenRelativeIndent
  | lineDelta == 0, placementAnchor placement == AfterNode = TokenRelativeIndent
  | placementRole placement == BetweenChildren TypeOperator
  , ConstructorBoundaryPath{} <- commentBoundaryPath boundary = TokenRelativeIndent
  | placementRole placement == BetweenChildren DerivingClause
  , placementAnchor placement == BeforeNode = OwnerRelativeIndent
  | structuralOwnerRelative placement = OwnerRelativeIndent
  | statementOwnerRelative placement = OwnerRelativeIndent
  | DelimiterBoundaryPath{} <- commentBoundaryPath boundary
  , commentBoundaryGap boundary `elem`
      [AfterOpenBoundary, WithinBoundary, BetweenBoundary, BeforeCloseBoundary] =
        ContainerRelativeIndent
  | otherwise = SourceColumnIndent

statementOwnerRelative :: CommentPlacement -> Bool
statementOwnerRelative placement = placementLineRelation placement
    == CommentOwnLine
  && placementAnchor placement == BeforeNode
  && case placementOwner placement of
    NodeId (AnnKey _ constructor) -> unConName constructor
      `elem` ["BodyStmt", "BindStmt", "LastStmt", "LetStmt"]

structuralOwnerRelative :: CommentPlacement -> Bool
structuralOwnerRelative placement = placementLineRelation placement
    == CommentOwnLine
  && placementAnchor placement == BeforeNode
  && placementRole placement `elem` [LeadingDoc, LeadingOrdinary]
  && case placementOwner placement of
    NodeId (AnnKey _ constructor) -> unConName constructor
      `elem` ["ConDeclH98", "ConDeclGADT", "VarPat"]

leadingDocContinuation :: CommentPlan -> SourceComment -> CommentBoundaryId -> Bool
leadingDocContinuation plan source boundary = case commentBoundaryPath boundary of
  ConstructorBoundaryPath{} -> any followsLeadingDoc
    $ Map.toList $ commentPlanSources plan
  _ -> False
 where
  followsLeadingDoc (otherKey, other) =
    SrcLoc.srcSpanEndLine (sourceCommentSpan other) + 1
      == SrcLoc.srcSpanStartLine (sourceCommentSpan source)
    && (placementRole <$> Map.lookup otherKey (commentPlanPlacements plan))
      == Just LeadingDoc
    && Map.lookup otherKey (commentPlanBoundaries plan) == Just boundary
