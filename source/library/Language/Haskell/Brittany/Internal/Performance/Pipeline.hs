{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Performance.Pipeline
  ( pPrintModuleWithSourceMeasured
  , pPrintModuleAndCheckWithSourceMeasured
  ) where

import qualified Control.Exception as Exception
import qualified Data.Map as Map
import Data.Semigroup (Last(..))
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified GHC
import Language.Haskell.Brittany.Internal
  ( commentValidationErrorsWithInputPlan
  , pPrintModulePreparedMeasured
  , semanticErrors
  )
import Language.Haskell.Brittany.Internal.CommentPlan (prepareCommentPlan)
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.ExactPrintCompat (Anns)
import Language.Haskell.Brittany.Internal.ExactPrintUtils (extractToplevelAnns)
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.Performance
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils ((.>))
import Language.Haskell.Brittany.Internal.SourceComment.Types
  ( CommentPlan
      ( commentPlanBoundaries
      , commentPlanPlacements
      , commentPlanSources
      )
  , CommentPlanError
  )
import Language.Haskell.Brittany.Internal.Types
  ( BrittanyError(..)
  , PerItemConfig
  )

pPrintModuleWithSourceMeasured
  :: Maybe PerformanceCollector
  -> Maybe Text.Text
  -> Config
  -> PerItemConfig
  -> Anns
  -> GHC.ParsedSource
  -> IO ([BrittanyError], TextL.Text)
pPrintModuleWithSourceMeasured metrics originalSource conf inlineConf anns
    parsedModule = snd <$> pPrintModuleWithSourceMeasuredPrepared metrics
      originalSource conf inlineConf anns parsedModule

pPrintModuleWithSourceMeasuredPrepared
  :: Maybe PerformanceCollector
  -> Maybe Text.Text
  -> Config
  -> PerItemConfig
  -> Anns
  -> GHC.ParsedSource
  -> IO
       ( Either [CommentPlanError] CommentPlan
       , ([BrittanyError], TextL.Text)
       )
pPrintModuleWithSourceMeasuredPrepared metrics originalSource conf inlineConf
    anns parsedModule = do
  preparedPlan <- measurePhase metrics CommentPlanning $ do
    let result = prepareCommentPlan parsedModule anns
        forcePlan = case result of
          Left planErrors -> length planErrors
          Right plan -> Map.size (commentPlanSources plan)
            + Map.size (commentPlanPlacements plan)
            + Map.size (commentPlanBoundaries plan)
    _ <- Exception.evaluate forcePlan
    pure result
  groupedAnnotations <- case preparedPlan of
    Left{} -> pure Map.empty
    Right{} -> measurePhase metrics TopLevelGrouping $ do
      let grouped = extractToplevelAnns parsedModule anns
          annotationCount = sum $ Map.size <$> Map.elems grouped
      _ <- Exception.evaluate annotationCount
      pure grouped
  formatted <- measurePhase metrics LayoutAndRendering $ do
    let result@(errors, output) = pPrintModulePreparedMeasured metrics
          originalSource conf inlineConf anns parsedModule preparedPlan
          groupedAnnotations
    _ <- Exception.evaluate $ length errors
    _ <- Exception.evaluate $ TextL.length output
    pure result
  pure (preparedPlan, formatted)

pPrintModuleAndCheckWithSourceMeasured
  :: Maybe PerformanceCollector
  -> ParseModule.ParserContext
  -> Maybe Text.Text
  -> Config
  -> PerItemConfig
  -> Anns
  -> GHC.ParsedSource
  -> IO ([BrittanyError], TextL.Text)
pPrintModuleAndCheckWithSourceMeasured metrics parserContext originalSource conf
    inlineConf anns parsedModule = do
  (inputPlan, (formatErrors, output)) <-
    pPrintModuleWithSourceMeasuredPrepared metrics originalSource conf inlineConf
      anns parsedModule
  measurePhase metrics OutputValidation $ do
    parseResult <- measurePhase metrics OutputParsing
      $ ParseModule.parseModuleInContextWithMetrics metrics
          parserContext
          "output"
          (TextL.unpack output)
    validationErrors <- case parseResult of
      Left{} -> pure [ErrorOutputCheck]
      Right (outputAnns, outputModule) -> do
        semanticValidationErrors <- measurePhase metrics SemanticValidation $ do
          let errors = semanticErrors parsedModule outputModule
          _ <- Exception.evaluate $ length errors
          pure errors
        let omitCommentCheck = conf
              & _conf_errorHandling
              .> _econf_omit_unused_comment_check
              .> confUnpack
        commentErrors <- measurePhase metrics CommentValidation $ do
          let errors = commentValidationErrorsWithInputPlan omitCommentCheck
                parsedModule inputPlan outputModule outputAnns
          _ <- Exception.evaluate $ length errors
          pure errors
        pure $ semanticValidationErrors ++ commentErrors
    let result = (formatErrors ++ validationErrors, output)
    _ <- Exception.evaluate $ length $ fst result
    pure result
