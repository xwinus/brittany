{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Performance.Micro
  ( runFocusedOperation
  ) where

import qualified Control.Exception as Exception
import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import qualified Data.Generics.Uniplate.Direct as Uniplate
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified GHC
import qualified Language.Haskell.Brittany.Internal.AnnotationIndex as AnnotationIndex
import Language.Haskell.Brittany.Internal
  ( commentValidationErrors
  , semanticErrors
  )
import Language.Haskell.Brittany.Internal.Backend (layoutBriDocM)
import Language.Haskell.Brittany.Internal.CommentPlan (prepareCommentPlan)
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.ExactPrintCompat
import Language.Haskell.Brittany.Internal.ExactPrintUtils (extractToplevelAnns)
import Language.Haskell.Brittany.Internal.ExtractAnns
  ( buildModuleAnnotationIndex
  , extractAnnsFromModule
  )
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.Performance
import Language.Haskell.Brittany.Internal.Performance.Fixtures
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Transformations.Alt (transformAlts)
import Language.Haskell.Brittany.Internal.Transformations.Columns
  ( transformSimplifyColumns )
import Language.Haskell.Brittany.Internal.Transformations.Floating
  ( transformSimplifyFloating )
import Language.Haskell.Brittany.Internal.Transformations.Indent
  ( transformSimplifyIndent )
import Language.Haskell.Brittany.Internal.Transformations.Par
  ( transformSimplifyPar )
import Language.Haskell.Brittany.Internal.Types

runFocusedOperation
  :: PerformanceCollector
  -> Config
  -> PerformancePhase
  -> BenchmarkInput
  -> IO (Either String Int)
runFocusedOperation collector config phase input = case phase of
  AnnKeyComparison -> measured $ compareAnnKeys fixtureSize
  AnnKeyMapOperations -> measured $ exerciseAnnKeyMap fixtureSize
  AnnotationExtraction -> withParsed $ \_ parsedModule -> measured $ do
    let annotations = extractAnnsFromModule parsedModule
    Exception.evaluate $ Map.size annotations
  AnnotationIndexConstruction -> withParsed $ \_ parsedModule -> measured $ do
    let annotationIndex = buildModuleAnnotationIndex parsedModule
    Exception.evaluate
      $ AnnotationIndex.indexNodeCount annotationIndex
      + AnnotationIndex.indexOverrideCount annotationIndex
  CommentPlanning -> withParsed $ \annotations parsedModule -> measured $ do
    let planResult = prepareCommentPlan parsedModule annotations
    Exception.evaluate $ forceCommentPlan planResult
  TopLevelGrouping -> withParsed $ \annotations parsedModule -> measured $ do
    let grouped = extractToplevelAnns parsedModule annotations
    Exception.evaluate $ sum $ Map.size <$> Map.elems grouped
  AlternativeResolution -> measured $ do
    let (document, debugMessages) = runAlternatives config
          $ if benchmarkInputOrigin input == "generated-layout-error-path"
            then malformedAlternativeDocument
            else alternativeDocument alternativeCount alternativeDepth
    Exception.evaluate $ documentSize document + Seq.length debugMessages
  SimplifyFloating -> measureDocument transformSimplifyFloating
  SimplifyPar -> measureDocument transformSimplifyPar
  SimplifyColumns -> measureDocument transformSimplifyColumns
  SimplifyIndent -> measureDocument transformSimplifyIndent
  BackendRendering -> measured $ renderDocument config
    $ backendDocument fixtureSize
  SemanticValidation -> withParsed $ \_ parsedModule -> measured $ do
    Exception.evaluate $ length $ semanticErrors parsedModule parsedModule
  CommentValidation -> withParsed $ \annotations parsedModule -> measured $ do
    Exception.evaluate $ length $ commentValidationErrors False
      parsedModule annotations parsedModule annotations
  _ -> pure $ Left $ "unsupported focused phase: " ++ performancePhaseName phase
 where
  fixtureSize = 2000
  alternativeCount = Maybe.fromMaybe fixtureSize
    $ benchmarkInputAlternativeCount input
  alternativeDepth = Maybe.fromMaybe 1
    $ benchmarkInputAlternativeDepth input
  metrics = Just collector
  measured action = Right <$> measurePhase metrics phase action
  measureDocument transformation = measured $ Exception.evaluate
    $ documentSize $ transformation $ simplificationDocument fixtureSize
  withParsed action = parsePreparedInput config input >>= \case
    Left parseError -> pure $ Left parseError
    Right (annotations, parsedModule) -> action annotations parsedModule

parsePreparedInput
  :: Config
  -> BenchmarkInput
  -> IO (Either String (Anns, GHC.ParsedSource))
parsePreparedInput config input = do
  let ghcOptions = config & _conf_forward & _options_ghc & runIdentity
  fmap (fmap $ \(annotations, parsedModule, _) -> (annotations, parsedModule))
    $ ParseModule.parseModuleWithMetrics Nothing
      ghcOptions
      (benchmarkInputName input)
      (const $ pure $ Right ())
      (benchmarkInputSource input)

compareAnnKeys :: Int -> IO Int
compareAnnKeys count = Exception.evaluate $ sum
  [ fromEnum $ compare left right
  | (left, right) <- zip keys $ reverse keys
  ]
 where
  keys = annotationKeys count

exerciseAnnKeyMap :: Int -> IO Int
exerciseAnnKeyMap count = Exception.evaluate
  $ Map.foldl' (+) 0 annotationMap
  + sum [Map.findWithDefault 0 key annotationMap | key <- reverse keys]
 where
  keys = annotationKeys count
  annotationMap = Map.fromList $ zip keys [1 ..]

annotationKeys :: Int -> [AnnKey]
annotationKeys count =
  [ AnnKey [] $ CN $ "GeneratedDeclaration" ++ show index
  | index <- [1 .. count]
  ]

forceCommentPlan :: Either [CommentPlanError] CommentPlan -> Int
forceCommentPlan = \case
  Left errors -> length errors
  Right plan -> Map.size (commentPlanSources plan)
    + Map.size (commentPlanPlacements plan)
    + Map.size (commentPlanBoundaries plan)

alternativeDocument :: Int -> Int -> BriDocNumbered
alternativeDocument count depth =
  (0, BDFSeq $ alternative <$> [0 .. max 0 count - 1])
 where
  alternative index =
    nestedAlternative (1 + index * (5 * max 1 depth)) $ max 1 depth
  nestedAlternative node remaining = (node, BDFAlt
    [ (node + 1, BDFLit $ Text.pack "value")
    , if remaining == 1
      then (node + 2, BDFPar BrIndentRegular
        (node + 3, BDFLit $ Text.pack "longValue")
        (node + 4, BDFLit $ Text.pack "continuation"))
      else nestedAlternative (node + 2) (remaining - 1)
    ])

malformedAlternativeDocument :: BriDocNumbered
malformedAlternativeDocument = (0, BDFAlt [])

runAlternatives :: Config -> BriDocNumbered -> (BriDoc, Seq String)
runAlternatives config document = runIdentity
  $ MultiRWSS.runMultiRWSTNil
  $ MultiRWSS.withMultiWriterAW
  $ MultiRWSS.withMultiReader config
  $ transformAlts document

simplificationDocument :: Int -> BriDoc
simplificationDocument count = BDLines $ element <$> [1 .. count]
 where
  element index = BDAddBaseY BrIndentRegular $ BDSeq
    [ BDSeq [BDEmpty, BDLit $ Text.pack $ "value" ++ show index]
    , BDPar BrIndentRegular
        (BDLines [BDLit $ Text.pack "left", BDLit $ Text.pack "right"])
        (BDCols (ColApp $ Text.pack "micro")
          [ BDLit $ Text.pack "nested"
          , BDLit $ Text.pack "value"
          ])
    ]

backendDocument :: Int -> BriDoc
backendDocument count = BDLines
  [ BDSeq
      [ BDLit $ Text.pack $ "value" ++ show index
      , BDSeparator
      , BDLit $ Text.pack "= 1"
      ]
  | index <- [1 .. count]
  ]

documentSize :: BriDoc -> Int
documentSize = length . Uniplate.universe

renderDocument :: Config -> BriDoc -> IO Int
renderDocument config document = Exception.evaluate
  $ TextL.length renderedText `seq`
    (fromIntegral $ TextL.length renderedText)
      + length errors
      + Seq.length debugMessages
 where
  ((builder, errors), debugMessages) = renderedResult
  renderedResult :: ((Text.Builder.Builder, [BrittanyError]), Seq String)
  renderedResult = runIdentity
    $ MultiRWSS.runMultiRWSTNil
    $ MultiRWSS.withMultiWriterAW
    $ MultiRWSS.withMultiWriterAW
    $ MultiRWSS.withMultiWriterW
    $ MultiRWSS.withMultiStateS initialLayoutState
    $ MultiRWSS.withMultiReader emptyCommentPlan
    $ MultiRWSS.withMultiReader (Map.empty :: Anns)
    $ MultiRWSS.withMultiReader config
    $ layoutBriDocM document
  renderedText = Text.Builder.toLazyText builder

emptyCommentPlan :: CommentPlan
emptyCommentPlan = CommentPlan Map.empty Map.empty Map.empty

initialLayoutState :: LayoutState
initialLayoutState = LayoutState
  { _lstate_baseYs = [0]
  , _lstate_curYOrAddNewline = Right 0
  , _lstate_indLevels = [0]
  , _lstate_indLevelLinger = 0
  , _lstate_comments = Map.empty
  , _lstate_emittedComments = Set.empty
  , _lstate_commentCol = Nothing
  , _lstate_addSepSpace = Nothing
  , _lstate_commentNewlines = 0
  }
