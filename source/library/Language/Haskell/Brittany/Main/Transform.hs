{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Main.Transform
  ( ChangeStatus(..)
  , coreIO
  , coreIOInSession
  , shouldEmitOutput
  ) where

import qualified Control.Monad.Trans.Except as ExceptT
import Data.CZipWith
import qualified Data.List.Extra
import Data.Kind (Type)
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as TextL
import DataTreePrint
import GHC (GenLocated(L))
import qualified GHC.Driver.Session as GHC
import GHC.Hs (hsmodDecls)
import qualified GHC.LanguageExtensions.Type as GHC
import qualified GHC.OldList as List
import GHC.Parser.Annotation (getLocA)
import GHC.Types.SrcLoc (unLoc)
import GHC.Utils.Outputable (Outputable(..), showSDocUnsafe)
import Language.Haskell.Brittany.Internal
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Fallbacks
  ( FallbackId(..)
  , fallbackRenderNotice
  , renderRenderInventory
  , renderRenderNotice
  )
import Language.Haskell.Brittany.Internal.Obfuscation
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Preprocessor (cppUnsupportedMessage)
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Utils
import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified System.FilePath.Posix as FilePath
import qualified System.IO

type ChangeStatus :: Type
data ChangeStatus = Changes | NoChanges
  deriving (Eq)

shouldEmitOutput
  :: Bool
  -> Bool
  -> Bool
  -> Bool
  -> [BrittanyError]
  -> Bool
shouldEmitOutput suppressOutput checkMode hasErrors outputOnErrors errors =
  not suppressOutput
    && not checkMode
    && not (any isSemanticValidationError errors)
    && (not hasErrors || outputOnErrors)
 where
  isSemanticValidationError ErrorSemanticChange{} = True
  isSemanticValidationError ErrorSemanticProjection{} = True
  isSemanticValidationError _ = False

coreIO
  :: (String -> IO ())
  -> Config
  -> Bool
  -> Bool
  -> Maybe FilePath.FilePath
  -> Maybe FilePath.FilePath
  -> IO (Either Int ChangeStatus)
coreIO putErrorLnIO config suppressOutput checkMode inputPathM outputPathM =
  ParseModule.withParserSession $ \parserSession ->
    coreIOInSession parserSession putErrorLnIO config suppressOutput checkMode
      inputPathM outputPathM

coreIOInSession
  :: ParseModule.ParserSession
  -> (String -> IO ())
  -> Config
  -> Bool
  -> Bool
  -> Maybe FilePath.FilePath
  -> Maybe FilePath.FilePath
  -> IO (Either Int ChangeStatus)
coreIOInSession parserSession putErrorLnIO config suppressOutput checkMode
    inputPathM outputPathM =
  ExceptT.runExceptT $ do
    let putErrorLn = liftIO . putErrorLnIO :: String -> ExceptT.ExceptT e IO ()
    let ghcOptions = config & _conf_forward & _options_ghc & runIdentity
    let cppMode = config & _conf_preprocessor & _ppconf_CPPMode & confUnpack
    let
      hackAroundIncludes =
        config & _conf_preprocessor & _ppconf_hackAroundIncludes & confUnpack
    let
      exactprintOnly = viaGlobal || viaDebug
       where
        viaGlobal = config & _conf_roundtrip_exactprint_only & confUnpack
        viaDebug =
          config & _conf_debug & _dconf_roundtrip_exactprint_only & confUnpack

    let
      cppCheckFunc dynFlags = if GHC.xopt GHC.Cpp dynFlags
        then case cppMode of
          CPPModeAbort -> pure $ Left cppUnsupportedMessage
          CPPModeWarn -> do
            putErrorLnIO
              $ "Warning: Encountered -XCPP."
              ++ " Be warned that -XCPP is not supported and that"
              ++ " brittany cannot check that its output is syntactically"
              ++ " valid in its presence."
            pure $ Right True
          CPPModeNowarn -> pure $ Right True
        else pure $ Right False
    (parseResult, originalContents) <- case inputPathM of
      Nothing -> do
        let
          hackF sourceLine = if "#include" `isPrefixOf` sourceLine
            then "-- BRITANY_INCLUDE_HACK " ++ sourceLine
            else sourceLine

          hackTransform = if hackAroundIncludes && not exactprintOnly
            then List.intercalate "\n" . fmap hackF . lines'
            else id
        inputString <- liftIO System.IO.getContents
        parseRes <- liftIO $ ParseModule.parseModuleInSessionWithMetrics Nothing
          parserSession ghcOptions "stdin" cppCheckFunc $ hackTransform inputString
        pure (parseRes, Text.pack inputString)
      Just path -> liftIO $ do
        inputText <- Text.IO.readFile path
        parseRes <- ParseModule.parseModuleInSessionWithMetrics Nothing
          parserSession ghcOptions path cppCheckFunc $ Text.unpack inputText
        pure (parseRes, inputText)
    case parseResult of
      Left parseError -> do
        putErrorLn "parse error:"
        putErrorLn parseError
        ExceptT.throwE 60
      Right (anns, parsedSource, hasCPP, parserContext) -> do
        (inlineConf, perItemConf) <-
          case extractCommentConfigs anns $ getTopLevelDeclNameMap parsedSource of
            Left (configError, input) -> do
              putErrorLn "Error: parse error in inline configuration:"
              putErrorLn configError
              putErrorLn $ "  in the string \"" ++ input ++ "\"."
              ExceptT.throwE 61
            Right parsedConfig -> pure parsedConfig
        let moduleConf = cZipWith fromOptionIdentity config inlineConf
        when (config & _conf_debug & _dconf_dump_ast_full & confUnpack) $ do
          let value = printTreeWithCustom 100 (customLayouterF anns) parsedSource
          trace ("---- ast ----\n" ++ show value) $ pure ()
        let
          disableFormatting =
            moduleConf & _conf_disable_formatting & confUnpack
        (errsWarns, outputText, hasChanges) <- do
          let
            ensureTrailingNewline text =
              if Text.null text || Text.last text /= '\n'
                then Text.append text $ Text.singleton '\n'
                else text
          if
            | disableFormatting -> do
              let output = ensureTrailingNewline originalContents
              pure ([], output, output /= originalContents)
            | exactprintOnly -> do
              let
                output = ensureTrailingNewline
                  $ Text.pack $ ExactPrint.exactPrint parsedSource
                reportFallbacks =
                  (moduleConf & _conf_debug & _dconf_dump_fallbacks & confUnpack)
                    || ( moduleConf
                      & _conf_debug
                      & _dconf_dump_fallbacks_json
                      & confUnpack
                       )
                    || ( moduleConf
                      & _conf_errorHandling
                      & _econf_failOnExactSourceFallback
                      & confUnpack
                       )
                fallbacks =
                  [ ExactSourceFallback
                    $ fallbackRenderNotice ExactPrintOnlyFallback
                    $ show $ getLocA declaration
                  | reportFallbacks
                  , declaration <- hsmodDecls $ unLoc parsedSource
                  ]
              pure (fallbacks, output, output /= originalContents)
            | otherwise -> do
              let
                omitCheck = moduleConf
                  & _conf_errorHandling
                  .> _econf_omit_output_valid_check
                  .> confUnpack
              (errorsAndWarnings, rawOutput) <- if hasCPP || omitCheck
                then pure $ pPrintModuleWithSource
                  (Just originalContents) moduleConf perItemConf anns parsedSource
                else liftIO $ pPrintModuleAndCheckWithSourceInContext
                  parserContext (Just originalContents) moduleConf perItemConf anns
                  parsedSource
              let
                restoreInclude sourceLine = fromMaybe sourceLine
                  $ TextL.stripPrefix
                    (TextL.pack "-- BRITANY_INCLUDE_HACK ") sourceLine
                output = TextL.toStrict $ if hackAroundIncludes
                  then TextL.intercalate (TextL.pack "\n")
                    $ restoreInclude <$> TextL.splitOn (TextL.pack "\n") rawOutput
                  else rawOutput
              transformedOutput <- if moduleConf & _conf_obfuscate & confUnpack
                then lift $ obfuscate output
                else pure output
              let finalOutput = ensureTrailingNewline transformedOutput
              pure
                (errorsAndWarnings, finalOutput, finalOutput /= originalContents)
        reportErrors putErrorLn config moduleConf errsWarns
        let
          hasErrors = errorsAreFatal config moduleConf errsWarns
          outputOnErrors = config
            & _conf_errorHandling
            & _econf_produceOutputOnErrors
            & confUnpack
          shouldOutput = shouldEmitOutput
            suppressOutput checkMode hasErrors outputOnErrors errsWarns
        when shouldOutput
          $ addTraceSep (_conf_debug config)
          $ case outputPathM of
              Nothing -> liftIO $ Text.IO.putStr outputText
              Just path -> liftIO $ do
                let isIdentical = case inputPathM of
                      Just _ -> not hasChanges
                      Nothing -> False
                unless isIdentical $ Text.IO.writeFile path outputText
        when (checkMode && hasChanges) $ forM_ inputPathM $ \path ->
          liftIO $ putStrLn $ "formatting would modify: " ++ path
        when hasErrors $ ExceptT.throwE 70
        pure $ if hasChanges then Changes else NoChanges

reportErrors
  :: (String -> ExceptT.ExceptT errorType IO ())
  -> Config
  -> Config
  -> [BrittanyError]
  -> ExceptT.ExceptT errorType IO ()
reportErrors putErrorLn config moduleConf errorsAndWarnings = do
  unless (null errorsAndWarnings) $ do
    let grouped = Data.List.Extra.groupOn errorOrder
          $ List.sortOn errorOrder errorsAndWarnings
    grouped `forM_` \case
      (ErrorOutputCheck{} : _) -> putErrorLn
        "ERROR: brittany pretty printer returned syntactically invalid result."
      semanticChanges@(ErrorSemanticChange{} : _) -> do
        putErrorLn "ERROR: formatted output changes parsed semantics."
        semanticChanges `forM_` \case
          ErrorSemanticChange path inputSummary outputSummary -> do
            putErrorLn $ "  path: " ++ path
            putErrorLn $ "  input: " ++ inputSummary
            putErrorLn $ "  output: " ++ outputSummary
          _ -> error "cannot happen (TM)"
      projectionErrors@(ErrorSemanticProjection{} : _) -> do
        putErrorLn "ERROR: semantic syntax comparison is incomplete."
        projectionErrors `forM_` \case
          ErrorSemanticProjection path unknownType -> do
            putErrorLn $ "  path: " ++ path
            putErrorLn $ "  unknown syntax type: " ++ unknownType
          _ -> error "cannot happen (TM)"
      (ErrorInput message : _) -> putErrorLn $ "ERROR: parse error: " ++ message
      unknownNodes@(ErrorUnknownNode{} : _) -> do
        putErrorLn "WARNING: encountered unknown syntactical constructs:"
        unknownNodes `forM_` \case
          ErrorUnknownNode message ast@(L location _) -> do
            putErrorLn
              $ "  " <> message <> " at " <> showSDocUnsafe (ppr location)
            when
                (config & _conf_debug & _dconf_dump_ast_unknown & confUnpack)
              $ putErrorLn $ "  " ++ show (astToDoc ast)
          _ -> error "cannot happen (TM)"
        putErrorLn
          "  -> falling back on exactprint for this element of the module"
      warnings@(LayoutWarning{} : _) -> do
        putErrorLn "WARNINGS:"
        warnings `forM_` \case
          LayoutWarning message -> putErrorLn message
          _ -> error "cannot happen (TM)"
      fallbacks@(ExactSourceFallback{} : _) -> when dumpTextNotices $ do
        putErrorLn "EXACT-SOURCE FALLBACKS:"
        fallbacks `forM_` \case
          ExactSourceFallback notice ->
            putErrorLn $ "  " ++ renderRenderNotice notice
          _ -> error "cannot happen (TM)"
      opaqueNotices@(SupportedOpaqueSyntax{} : _) -> when dumpTextNotices $ do
        putErrorLn "SUPPORTED OPAQUE SYNTAX:"
        opaqueNotices `forM_` \case
          SupportedOpaqueSyntax notice ->
            putErrorLn $ "  " ++ renderRenderNotice notice
          _ -> error "cannot happen (TM)"
      unusedComments@(ErrorUnusedComment{} : _) -> do
        putErrorLn
          $ "Error: detected unprocessed comments."
          ++ " The transformation output will most likely not contain some"
          ++ " of the comments present in the input haskell source file."
        putErrorLn "Affected are the following comments:"
        unusedComments `forM_` \case
          ErrorUnusedComment message -> putErrorLn message
          _ -> error "cannot happen (TM)"
      planErrors@(ErrorCommentPlan{} : _) -> do
        putErrorLn "Error: source comment ownership is ambiguous or unstable."
        planErrors `forM_` \case
          ErrorCommentPlan message -> putErrorLn $ "  " ++ message
          _ -> error "cannot happen (TM)"
      delimiterErrors@(ErrorDelimiterInvariant{} : _) -> do
        putErrorLn "Error: delimiter layout invariant failed."
        delimiterErrors `forM_` \case
          ErrorDelimiterInvariant message -> putErrorLn $ "  " ++ message
          _ -> error "cannot happen (TM)"
      alignmentErrors@(ErrorAlignmentPlan{} : _) -> do
        putErrorLn "Error: column alignment planning failed."
        alignmentErrors `forM_` \case
          ErrorAlignmentPlan message -> putErrorLn $ "  " ++ message
          _ -> error "cannot happen (TM)"
      (ErrorMacroConfig configError input : _) -> do
        putErrorLn "Error: parse error in inline configuration:"
        putErrorLn configError
        putErrorLn $ "  in the string \"" ++ input ++ "\"."
      [] -> error "cannot happen"
  when dumpJsonNotices $ putErrorLn $ renderRenderInventory renderNotices
 where
  dumpTextNotices =
    (moduleConf & _conf_debug & _dconf_dump_fallbacks & confUnpack)
      || ( moduleConf
        & _conf_errorHandling
        & _econf_failOnExactSourceFallback
        & confUnpack
         )
      || (moduleConf & _conf_errorHandling & _econf_failOnOpaque & confUnpack)
  dumpJsonNotices = moduleConf
    & _conf_debug
    & _dconf_dump_fallbacks_json
    & confUnpack
  renderNotices = catMaybes $ errorsAndWarnings <&> \case
    ExactSourceFallback notice -> Just notice
    SupportedOpaqueSyntax notice -> Just notice
    _ -> Nothing

errorsAreFatal :: Config -> Config -> [BrittanyError] -> Bool
errorsAreFatal config moduleConf errorsAndWarnings =
  (failOnFallback && any isFallback errorsAndWarnings)
    || (failOnOpaque && any isOpaque errorsAndWarnings)
    || if config & _conf_errorHandling & _econf_Werror & confUnpack
      then any isWarningOrError errorsAndWarnings
      else 0 < maximum (-1 : fmap errorOrder errorsAndWarnings)
 where
  failOnFallback = moduleConf
    & _conf_errorHandling
    & _econf_failOnExactSourceFallback
    & confUnpack
  failOnOpaque = moduleConf
    & _conf_errorHandling
    & _econf_failOnOpaque
    & confUnpack
  isWarningOrError ExactSourceFallback{} = False
  isWarningOrError SupportedOpaqueSyntax{} = False
  isWarningOrError _ = True
  isFallback ExactSourceFallback{} = True
  isFallback _ = False
  isOpaque SupportedOpaqueSyntax{} = True
  isOpaque _ = False

errorOrder :: BrittanyError -> Int
errorOrder = \case
  ErrorInput{} -> 4
  LayoutWarning{} -> -1
  ExactSourceFallback{} -> -3
  SupportedOpaqueSyntax{} -> -4
  ErrorOutputCheck{} -> 1
  ErrorSemanticChange{} -> 7
  ErrorSemanticProjection{} -> 8
  ErrorUnusedComment{} -> 2
  ErrorUnknownNode{} -> -2
  ErrorMacroConfig{} -> 5
  ErrorCommentPlan{} -> 6
  ErrorDelimiterInvariant{} -> 9
  ErrorAlignmentPlan{} -> 10

addTraceSep :: DebugConfig -> value -> value
addTraceSep config =
  if or
      [ confUnpack $ _dconf_dump_annotations config
      , confUnpack $ _dconf_dump_fallbacks config
      , confUnpack $ _dconf_dump_ast_unknown config
      , confUnpack $ _dconf_dump_ast_full config
      , confUnpack $ _dconf_dump_bridoc_raw config
      , confUnpack $ _dconf_dump_bridoc_simpl_alt config
      , confUnpack $ _dconf_dump_bridoc_simpl_floating config
      , confUnpack $ _dconf_dump_bridoc_simpl_columns config
      , confUnpack $ _dconf_dump_bridoc_simpl_indent config
      , confUnpack $ _dconf_dump_bridoc_final config
      ]
    then trace "----"
    else id
