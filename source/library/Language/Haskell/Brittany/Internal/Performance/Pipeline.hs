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
  ( commentValidationErrors
  , pPrintModulePrepared
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
    parsedModule = do
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
  measurePhase metrics LayoutAndRendering $ do
    let result@(errors, output) = pPrintModulePrepared originalSource conf
          inlineConf anns parsedModule preparedPlan groupedAnnotations
    _ <- Exception.evaluate $ length errors
    _ <- Exception.evaluate $ TextL.length output
    pure result

pPrintModuleAndCheckWithSourceMeasured
  :: Maybe PerformanceCollector
  -> Maybe Text.Text
  -> Config
  -> PerItemConfig
  -> Anns
  -> GHC.ParsedSource
  -> IO ([BrittanyError], TextL.Text)
pPrintModuleAndCheckWithSourceMeasured metrics originalSource conf inlineConf
    anns parsedModule = do
  (formatErrors, output) <- pPrintModuleWithSourceMeasured metrics originalSource
    conf inlineConf anns parsedModule
  measurePhase metrics OutputValidation $ do
    let ghcOptions = conf & _conf_forward & _options_ghc & runIdentity
    parseResult <- measurePhase metrics OutputParsing
      $ ParseModule.parseModuleWithMetrics metrics
          ghcOptions
          "output"
          (\_ -> return $ Right ())
          (TextL.unpack output)
    validationErrors <- case parseResult of
      Left{} -> pure [ErrorOutputCheck]
      Right (outputAnns, outputModule, _) -> do
        semanticValidationErrors <- measurePhase metrics SemanticValidation $ do
          let errors = semanticErrors parsedModule outputModule
          _ <- Exception.evaluate $ length errors
          pure errors
        let omitCommentCheck = conf
              & _conf_errorHandling
              .> _econf_omit_unused_comment_check
              .> confUnpack
        commentErrors <- measurePhase metrics CommentValidation $ do
          let errors = commentValidationErrors omitCommentCheck parsedModule anns
                outputModule outputAnns
          _ <- Exception.evaluate $ length errors
          pure errors
        pure $ semanticValidationErrors ++ commentErrors
    let result = (formatErrors ++ validationErrors, output)
    _ <- Exception.evaluate $ length $ fst result
    pure result
