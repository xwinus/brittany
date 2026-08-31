{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.CommentBoundary
  ( canonicalCommentGraph
  , attachCommentBoundaries
  , materializeCommentBoundaries
  ) where

import qualified Data.Char                               as Char
import qualified Data.Generics                           as Generics
import qualified Data.List                               as List
import qualified Data.Map                                as Map
import qualified Data.Maybe                              as Maybe
import qualified Data.Set                                as Set
import qualified Data.Text                               as Text
import           GHC                                      ( GenLocated(L)
                                                          , HsModule(..)
                                                          , ParsedSource
                                                          , moduleNameString
                                                          , unLoc
                                                          )
import           GHC.Hs                                   ( DataDefnCons(..)
                                                          , HsDataDefn(..)
                                                          , HsDecl(..)
                                                          , ImportDecl(..)
                                                          , LConDecl
                                                          , LHsExpr
                                                          , LHsDecl
                                                          , LImportDecl
                                                          , TyClDecl(..)
                                                          )
import           GHC.Parser.Annotation                    ( HasLoc
                                                          , getLocA
                                                          )
import qualified GHC.Types.SrcLoc                        as SrcLoc
import           Language.Haskell.Brittany.Internal.ConstructorComments
                                                          ( normalizeConstructorComments
                                                          )
import           Language.Haskell.Brittany.Internal.CommentBoundary.Delimiter
                                                          ( delimiterBoundary
                                                          )
import           Language.Haskell.Brittany.Internal.ExactPrintCompat
import           Language.Haskell.Brittany.Internal.Prelude
import           Language.Haskell.Brittany.Internal.SourceComment.Types

materializeCommentBoundaries :: ParsedSource -> Anns -> Anns
materializeCommentBoundaries (L _ module') annotations =
  normalizeConstructorComments declarations
    $ foldl materializeDeclarationGap annotations
    $ zip declarations (drop 1 declarations)
 where
  declarations = hsmodDecls module'

materializeDeclarationGap :: Anns -> (LHsDecl GhcPs, LHsDecl GhcPs) -> Anns
materializeDeclarationGap annotations (previous, current) =
  case (locatedSpan previous, locatedSpan current) of
    (Just previousSpan, Just currentSpan) ->
      let comments = commentsInGap previousSpan currentSpan annotations
          (previousComments, currentComments) =
            splitDeclarationBoundary previous comments
      in  if null comments
            then annotations
            else
              attachPriorRun current currentComments
              $ attachFollowingRun previous previousSpan previousComments
              $ removeComments comments annotations
    _ -> annotations

splitDeclarationBoundary
  :: LHsDecl GhcPs -> [Comment] -> ([Comment], [Comment])
splitDeclarationBoundary previous comments = case unLoc previous of
  SigD{} -> takePostDocRun comments
  _      -> ([], comments)

takePostDocRun :: [Comment] -> ([Comment], [Comment])
takePostDocRun comments = case List.sortOn commentStart comments of
  firstComment : rest | isPostDocText $ commentContents firstComment ->
    let (continuations, remaining) = takeContinuations firstComment rest
    in  (firstComment : continuations, remaining)
  sorted -> ([], sorted)
 where
  takeContinuations previous = \case
    current : rest
      | commentsAreAdjacent previous current
      , not $ startsNewHaddockRun $ commentContents current
      -> let (continuations, remaining) = takeContinuations current rest
         in  (current : continuations, remaining)
    remaining -> ([], remaining)

startsNewHaddockRun :: String -> Bool
startsNewHaddockRun text =
  isLeadingDocText text || isSectionText text || isPragmaText text

commentsAreAdjacent :: Comment -> Comment -> Bool
commentsAreAdjacent previous current =
  case
      ( srcSpanToRealSpan $ commentIdentifier previous
      , srcSpanToRealSpan $ commentIdentifier current
      )
    of
      (Just previousSpan, Just currentSpan) ->
        SrcLoc.srcSpanEndLine previousSpan
          +  1
          == SrcLoc.srcSpanStartLine currentSpan
      _ -> False

commentsInGap :: SrcLoc.RealSrcSpan -> SrcLoc.RealSrcSpan -> Anns -> [Comment]
commentsInGap previousSpan currentSpan annotations = uniqueComments
  [ comment
  | annotation       <- Map.elems annotations
  , comment          <- annotationComments annotation
  , Just commentSpan <- [srcSpanToRealSpan $ commentIdentifier comment]
  , SrcLoc.srcSpanStartLine commentSpan > SrcLoc.srcSpanEndLine previousSpan
  , spanStart commentSpan < spanStart currentSpan
  ]

uniqueComments :: [Comment] -> [Comment]
uniqueComments = Map.elems . Map.fromList . fmap
  (\comment -> (SourceCommentKey $ commentIdentifier comment, comment))

annotationComments :: Annotation -> [Comment]
annotationComments annotation =
  fmap fst (annPriorComments annotation)
    ++ fmap fst (annFollowingComments annotation)
    ++ [ comment | (AnnComment comment, _) <- annsDP annotation ]

removeComments :: [Comment] -> Anns -> Anns
removeComments comments = Map.map remove
 where
  keys = Set.fromList $ SourceCommentKey . commentIdentifier <$> comments
  keep comment =
    Set.notMember (SourceCommentKey $ commentIdentifier comment) keys
  remove annotation =
    annotation
      { annPriorComments     = filter (keep . fst) $ annPriorComments annotation
      , annFollowingComments =
          filter (keep . fst) $ annFollowingComments annotation
      , annsDP               =
          filter
              (\case
                (AnnComment comment, _) -> keep comment
                _                       -> True
              )
            $ annsDP annotation
      }

attachPriorRun :: LHsDecl GhcPs -> [Comment] -> Anns -> Anns
attachPriorRun _           []       = id
attachPriorRun declaration comments = Map.alter attach key
 where
  key = mkAnnKey $ L (getLocA declaration) $ unLoc declaration
  attach maybeAnnotation =
    Just
      $ annotation
          { annPriorComments = rebaseRun comments ++ annPriorComments annotation
          }
   where
    annotation = fromMaybe emptyAnnotation maybeAnnotation

attachFollowingRun
  :: LHsDecl GhcPs -> SrcLoc.RealSrcSpan -> [Comment] -> Anns -> Anns
attachFollowingRun _           _               []       = id
attachFollowingRun declaration declarationSpan comments = Map.alter attach key
 where
  key = mkAnnKey $ L (getLocA declaration) $ unLoc declaration
  attach maybeAnnotation =
    Just
      $ annotation
          { annFollowingComments =
              annFollowingComments annotation
                ++ rebaseFollowing declarationSpan comments
          }
   where
    annotation = fromMaybe emptyAnnotation maybeAnnotation

rebaseFollowing :: SrcLoc.RealSrcSpan -> [Comment] -> [(Comment, DeltaPos)]
rebaseFollowing declarationSpan comments =
  snd $ mapAccumL rebase (spanEnd declarationSpan) $ List.sortOn commentStart
                                                                 comments
 where
  rebase previous comment =
    ( commentEnd comment
    , (comment, positionDelta previous $ commentStart comment)
    )

rebaseRun :: [Comment] -> [(Comment, DeltaPos)]
rebaseRun comments = case List.sortOn commentStart comments of
  [] -> []
  firstComment : rest ->
    (firstComment, DP (0, 0))
      : snd (mapAccumL rebase (commentEnd firstComment) rest)
 where
  rebase previous comment =
    ( commentEnd comment
    , (comment, positionDelta previous $ commentStart comment)
    )

emptyAnnotation :: Annotation
emptyAnnotation =
  Ann
    { annCapturedSpan      = Nothing
    , annSortKey           = Nothing
    , annsDP               = []
    , annFollowingComments = []
    , annPriorComments     = []
    , annEntryDelta        = DP (0, 0)
    }

canonicalCommentGraph :: ParsedSource -> CommentPlan -> [CanonicalComment]
canonicalCommentGraph (L _ module') plan = canonicalizeRuns
  [ CanonicalComment
                     (Map.findWithDefault
                       (boundaryFor
                         module'
                         expressionOwners
                         sourceComment
                         placement
                       )
                       (sourceCommentKey sourceComment)
                       (commentPlanBoundaries plan)
                     )
                     (sourceCommentText sourceComment)
                     (sourceCommentSyntax sourceComment)
                     (placementRole placement)
  | (sourceComment, placement) <- orderedComments plan
  ]
 where
  expressionOwners = expressionOwnerIndices module'

attachCommentBoundaries :: ParsedSource -> CommentPlan -> CommentPlan
attachCommentBoundaries (L _ module') plan = plan
  { commentPlanBoundaries = Map.fromList
      [ (key, boundaryFor module' expressionOwners sourceComment placement)
      | (key, sourceComment) <- Map.toList $ commentPlanSources plan
      , Just placement <- [Map.lookup key $ commentPlanPlacements plan]
      ]
  }
 where
  expressionOwners = expressionOwnerIndices module'

canonicalizeRuns :: [CanonicalComment] -> [CanonicalComment]
canonicalizeRuns = List.concatMap canonicalizeRun . List.groupBy sameBoundary
 where
  sameBoundary left right =
    canonicalCommentBoundary left == canonicalCommentBoundary right
  canonicalizeRun = snd . mapAccumL canonicalize False
  canonicalize inPostDocRun comment =
    let stripped = Text.unpack $ Text.stripStart $ canonicalCommentText comment
        explicitPostDoc =
          isPostDocText stripped
            && acceptsPostDoc (canonicalCommentBoundary comment)
        continuation =
          inPostDocRun
            && not (isLeadingDocText stripped)
            && not (isSectionText stripped)
            && not (isPragmaText stripped)
        role | explicitPostDoc || continuation = HaddockPostDoc DataConstructor
             | isLeadingDocText stripped       = LeadingDoc
             | isSectionText stripped          = SectionComment
             | isPragmaText stripped           = PragmaComment
             | otherwise                       = LeadingOrdinary
    in  ( explicitPostDoc || continuation
        , comment { canonicalCommentRole = role }
        )

acceptsPostDoc :: CommentBoundaryId -> Bool
acceptsPostDoc boundary =
  case (commentBoundaryPath boundary, commentBoundaryGap boundary) of
    (ConstructorBoundaryPath{}, AfterLastBoundary) -> True
    (ConstructorBoundaryPath{}, BetweenBoundary) -> True
    (ConstructorBoundaryPath{}, WithinBoundary) -> True
    _ -> False

boundaryFor
  :: HsModule GhcPs
  -> Map NodeId Int
  -> SourceComment
  -> CommentPlacement
  -> CommentBoundaryId
boundaryFor module' expressionOwners sourceComment placement =
  fromMaybe moduleBoundary
    $   delimiterBoundary module' commentSpan
    <|> constructorBoundary declarations commentSpan
    <|> expressionBoundary expressionOwners placement
    <|> declarationBoundary declarations commentSpan
    <|> importBoundary (hsmodImports module') commentSpan
 where
  declarations   = hsmodDecls module'
  commentSpan    = sourceCommentSpan sourceComment
  moduleBoundary = CommentBoundaryId ModuleBoundaryPath WithinBoundary

expressionOwnerIndices :: HsModule GhcPs -> Map NodeId Int
expressionOwnerIndices module' = Map.fromList $ zip expressionOwners [0 ..]
 where
  expressionOwners = Generics.everything (++) expressionQuery module'
  expressionQuery :: Generics.GenericQ [NodeId]
  expressionQuery = Generics.mkQ [] $ \expression ->
    [ NodeId $ mkAnnKey $ L
        (getLocA (expression :: LHsExpr GhcPs))
        (unLoc expression)
    ]

expressionBoundary
  :: Map NodeId Int -> CommentPlacement -> Maybe CommentBoundaryId
expressionBoundary expressionOwners placement = do
  guard $ placementRole placement == LeadingOrdinary
  guard $ placementAnchor placement == BeforeNode
  guard $ placementLineRelation placement == CommentOwnLine
  index <- Map.lookup (placementOwner placement) expressionOwners
  pure $ CommentBoundaryId (ExpressionBoundaryPath index) WithinBoundary

declarationBoundary
  :: [LHsDecl GhcPs] -> SrcLoc.RealSrcSpan -> Maybe CommentBoundaryId
declarationBoundary declarations commentSpan =
  case indexedSpans declarations of
    []    -> Nothing
    spans -> case List.find (contains commentSpan . snd) spans of
      Just (index, _) -> boundary index WithinBoundary
      Nothing         -> case List.find (isBefore commentSpan . snd) spans of
        Just (0    , _) -> boundary 0 BeforeBoundary
        Just (index, _) -> boundary (index - 1) BetweenBoundary
        Nothing         -> boundary (length spans - 1) AfterLastBoundary
 where
  boundary index gap =
    Just $ CommentBoundaryId (DeclarationBoundaryPath index) gap

constructorBoundary
  :: [LHsDecl GhcPs] -> SrcLoc.RealSrcSpan -> Maybe CommentBoundaryId
constructorBoundary declarations commentSpan = asum
  [ boundaryWithinDeclaration declarationIndex declaration nextDeclaration
  | (declarationIndex, declaration, nextDeclaration) <- zip3
    [0 ..]
    declarations
    ((Just <$> drop 1 declarations) ++ [Nothing])
  ]
 where
  boundaryWithinDeclaration declarationIndex declaration nextDeclaration = do
    declarationSpan <- locatedSpan declaration
    guard $ spanStart commentSpan >= spanStart declarationSpan
    guard $ maybe True
                  (\nextSpan -> spanStart commentSpan < spanStart nextSpan)
                  (nextDeclaration >>= locatedSpan)
    constructors <- declarationConstructors declaration
    let spans = indexedSpans constructors
    guard $ not $ null spans
    case List.find (contains commentSpan . snd) spans of
      Just (constructorIndex, _) ->
        boundary declarationIndex constructorIndex WithinBoundary
      Nothing -> case List.find (isBefore commentSpan . snd) spans of
        Just (0, _) -> Nothing
        Just (constructorIndex, _) ->
          boundary declarationIndex (constructorIndex - 1) BetweenBoundary
        Nothing ->
          boundary declarationIndex (length spans - 1) AfterLastBoundary
  boundary declarationIndex constructorIndex gap = Just $ CommentBoundaryId
    (ConstructorBoundaryPath declarationIndex constructorIndex)
    gap

declarationConstructors :: LHsDecl GhcPs -> Maybe [LConDecl GhcPs]
declarationConstructors declaration = case unLoc declaration of
  TyClD _ DataDecl { tcdDataDefn = HsDataDefn { dd_cons = constructors } } ->
    Just $ case constructors of
      NewTypeCon constructor -> [constructor]
      DataTypeCons _ values  -> values
  _ -> Nothing

importBoundary
  :: [LImportDecl GhcPs]
  -> SrcLoc.RealSrcSpan
  -> Maybe CommentBoundaryId
importBoundary imports commentSpan =
  case Maybe.mapMaybe importInfo $ zip [0 ..] imports of
    []    -> Nothing
    spans -> case List.find (contains commentSpan . third) spans of
      Just (name, occurrence, _) -> Just $ CommentBoundaryId
        (ImportBoundaryPath name occurrence)
        WithinBoundary
      Nothing -> Nothing
 where
  importInfo (occurrence, declaration) = case unLoc declaration of
    ImportDecl { ideclName = importedModule } -> do
      span' <- locatedSpan declaration
      pure (moduleNameString $ unLoc importedModule, occurrence, span')
    XImportDecl{} -> Nothing
  third (_, _, value) = value

orderedComments :: CommentPlan -> [(SourceComment, CommentPlacement)]
orderedComments plan = List.sortOn
  (placementRelativeOrder . snd)
  [ (sourceComment, placement)
  | (key, placement)   <- Map.toList $ commentPlanPlacements plan
  , Just sourceComment <- [Map.lookup key $ commentPlanSources plan]
  ]

indexedSpans
  :: HasLoc location
  => [GenLocated location value]
  -> [(Int, SrcLoc.RealSrcSpan)]
indexedSpans values =
  [ (index, span')
  | (index, value) <- zip [0 ..] values
  , Just span'     <- [locatedSpan value]
  ]

locatedSpan
  :: HasLoc location
  => GenLocated location value
  -> Maybe SrcLoc.RealSrcSpan
locatedSpan value = srcSpanToRealSpan $ getLocA value

contains :: SrcLoc.RealSrcSpan -> SrcLoc.RealSrcSpan -> Bool
contains child parent =
  spanStart child >= spanStart parent && spanEnd child <= spanEnd parent

isBefore :: SrcLoc.RealSrcSpan -> SrcLoc.RealSrcSpan -> Bool
isBefore child parent = spanEnd child <= spanStart parent

commentStart :: Comment -> (Int, Int)
commentStart comment =
  maybe (0, 0) spanStart $ srcSpanToRealSpan $ commentIdentifier comment

commentEnd :: Comment -> (Int, Int)
commentEnd comment =
  maybe (0, 0) spanEnd $ srcSpanToRealSpan $ commentIdentifier comment

spanStart :: SrcLoc.RealSrcSpan -> (Int, Int)
spanStart span' = (SrcLoc.srcSpanStartLine span', SrcLoc.srcSpanStartCol span')

spanEnd :: SrcLoc.RealSrcSpan -> (Int, Int)
spanEnd span' = (SrcLoc.srcSpanEndLine span', SrcLoc.srcSpanEndCol span')

positionDelta :: (Int, Int) -> (Int, Int) -> DeltaPos
positionDelta (previousLine, previousColumn) (currentLine, currentColumn)
  | currentLine == previousLine = DP (0, currentColumn - previousColumn)
  | otherwise = DP (currentLine - previousLine, currentColumn - 1)

isPostDocText :: String -> Bool
isPostDocText = hasMarker '^'

isLeadingDocText :: String -> Bool
isLeadingDocText = hasMarker '|'

isSectionText :: String -> Bool
isSectionText = hasMarker '*'

hasMarker :: Char -> String -> Bool
hasMarker marker = \case
  '-' : '-' : rest -> startsWith marker rest
  '{' : '-' : rest -> startsWith marker rest
  _                -> False

isPragmaText :: String -> Bool
isPragmaText = List.isPrefixOf "{-#"

startsWith :: Char -> String -> Bool
startsWith expected = \case
  actual : _ | actual == expected -> True
  space : actual : _ -> Char.isSpace space && actual == expected
  _ -> False
