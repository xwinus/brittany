{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Brittany.Internal
  ( parsePrintModule
  , parsePrintModuleTests
  , pPrintModule
  , pPrintModuleWithSource
  , pPrintModuleAndCheck
  , pPrintModuleAndCheckWithSource
   -- re-export from utils:
  , parseModule
  , parseModuleFromString
  , extractCommentConfigs
  , getTopLevelDeclNameMap
  ) where

import Control.Monad.Trans.Except
import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import qualified Data.ByteString.Char8
import Data.CZipWith
import Data.Char (isSpace)
import Data.HList.HList
import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Data.Yaml
import qualified GHC hiding (parseModule)
import GHC (GenLocated(L))
import qualified GHC.Driver.Session as GHC
import GHC.Hs
import GHC.Parser.Annotation (getLocA)
import GHC.Types.SrcLoc (unLoc)
import qualified GHC.LanguageExtensions.Type as GHC
import qualified GHC.OldList as List
import Language.Haskell.Brittany.Internal.ExactPrintCompat
  ( AnnConName(..), AnnKey(..), AnnKeywordId(..), Anns, Annotation(..)
  , Comment(..), KeywordId(..), mkAnnKey
  )
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as EP
import GHC.Types.SrcLoc (SrcSpan)
import Language.Haskell.Brittany.Internal.Backend
import Language.Haskell.Brittany.Internal.BackendUtils
import Language.Haskell.Brittany.Internal.CommentUtils
  ( collectCommentContents
  , collectCommentPositions
  )
import Language.Haskell.Brittany.Internal.CommentPlan
import Language.Haskell.Brittany.Internal.Config
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.ExactSource (nodeSourceSlice)
import Language.Haskell.Brittany.Internal.Fallbacks
  ( FallbackId(..)
  , renderFallbackNotice
  )
import Language.Haskell.Brittany.Internal.ExactPrintUtils
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.IE (toL)
import Language.Haskell.Brittany.Internal.Layouters.Decl
import Language.Haskell.Brittany.Internal.Layouters.Module
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Preprocessor (cppUnsupportedMessage)
import Language.Haskell.Brittany.Internal.Transformations.Alt
import Language.Haskell.Brittany.Internal.Transformations.Columns
import Language.Haskell.Brittany.Internal.Transformations.Floating
import Language.Haskell.Brittany.Internal.Transformations.Indent
import Language.Haskell.Brittany.Internal.Transformations.Par
import Language.Haskell.Brittany.Internal.TopLevelSpacing
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Utils
import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint
import qualified UI.Butcher.Monadic as Butcher



data InlineConfigTarget
    = InlineConfigTargetModule
    | InlineConfigTargetNextDecl    -- really only next in module
    | InlineConfigTargetNextBinding -- by name
    | InlineConfigTargetBinding String

extractCommentConfigs
  :: Anns
  -> TopLevelDeclNameMap
  -> Either (String, String) (CConfig Maybe, PerItemConfig)
extractCommentConfigs anns (TopLevelDeclNameMap declNameMap) = do
  let
    commentLiness =
      [ ( k
        , [ commentContents c
          | (c, _) <-
            (annPriorComments ann
            ++ annFollowingComments ann
            )
          ]
        ++ [ commentContents c
           | (AnnComment c, _) <- annsDP ann
           ]
        )
      | (k, ann) <- Map.toList anns
      ]
  let
    configLiness = commentLiness <&> second
      (Data.Maybe.mapMaybe $ \line -> do
        l1 <-
          List.stripPrefix "-- BRITTANY" line
          <|> List.stripPrefix "--BRITTANY" line
          <|> List.stripPrefix "-- brittany" line
          <|> List.stripPrefix "--brittany" line
          <|> (List.stripPrefix "{- BRITTANY" line >>= stripSuffix "-}")
        let l2 = dropWhile isSpace l1
        guard
          (("@" `isPrefixOf` l2)
          || ("-disable" `isPrefixOf` l2)
          || ("-next" `isPrefixOf` l2)
          || ("{" `isPrefixOf` l2)
          || ("--" `isPrefixOf` l2)
          )
        pure l2
      )
  let
    configParser = Butcher.addAlternatives
      [ ( "commandline-config"
        , \s -> "-" `isPrefixOf` dropWhile (== ' ') s
        , cmdlineConfigParser
        )
      , ( "yaml-config-document"
        , \s -> "{" `isPrefixOf` dropWhile (== ' ') s
        , Butcher.addCmdPart (Butcher.varPartDesc "yaml-config-document")
        $ fmap (\lconf -> (mempty { _conf_layout = lconf }, ""))
        . either (const Nothing) Just
        . Data.Yaml.decodeEither'
        . Data.ByteString.Char8.pack
          -- TODO: use some proper utf8 encoder instead?
        )
      ]
    parser = do -- we will (mis?)use butcher here to parse the inline config
                -- line.
      let
        nextDecl = do
          conf <- configParser
          Butcher.addCmdImpl (InlineConfigTargetNextDecl, conf)
      Butcher.addCmd "-next-declaration" nextDecl
      Butcher.addCmd "-Next-Declaration" nextDecl
      Butcher.addCmd "-NEXT-DECLARATION" nextDecl
      let
        nextBinding = do
          conf <- configParser
          Butcher.addCmdImpl (InlineConfigTargetNextBinding, conf)
      Butcher.addCmd "-next-binding" nextBinding
      Butcher.addCmd "-Next-Binding" nextBinding
      Butcher.addCmd "-NEXT-BINDING" nextBinding
      let
        disableNextBinding = do
          Butcher.addCmdImpl
            ( InlineConfigTargetNextBinding
            , mempty { _conf_roundtrip_exactprint_only = pure $ pure True }
            )
      Butcher.addCmd "-disable-next-binding" disableNextBinding
      Butcher.addCmd "-Disable-Next-Binding" disableNextBinding
      Butcher.addCmd "-DISABLE-NEXT-BINDING" disableNextBinding
      let
        disableNextDecl = do
          Butcher.addCmdImpl
            ( InlineConfigTargetNextDecl
            , mempty { _conf_roundtrip_exactprint_only = pure $ pure True }
            )
      Butcher.addCmd "-disable-next-declaration" disableNextDecl
      Butcher.addCmd "-Disable-Next-Declaration" disableNextDecl
      Butcher.addCmd "-DISABLE-NEXT-DECLARATION" disableNextDecl
      let
        disableFormatting = do
          Butcher.addCmdImpl
            ( InlineConfigTargetModule
            , mempty { _conf_disable_formatting = pure $ pure True }
            )
      Butcher.addCmd "-disable" disableFormatting
      Butcher.addCmd "@" $ do
        -- Butcher.addCmd "module" $ do
        --   conf <- configParser
        --   Butcher.addCmdImpl (InlineConfigTargetModule, conf)
        Butcher.addNullCmd $ do
          bindingName <- Butcher.addParamString "BINDING" mempty
          conf <- configParser
          Butcher.addCmdImpl (InlineConfigTargetBinding bindingName, conf)
      conf <- configParser
      Butcher.addCmdImpl (InlineConfigTargetModule, conf)
  lineConfigss <- configLiness `forM` \(k, ss) -> do
    r <- ss `forM` \s -> case Butcher.runCmdParserSimple s parser of
      Left err -> Left $ (err, s)
      Right c -> Right $ c
    pure (k, r)

  let
    perModule = foldl'
      (<>)
      mempty
      [ conf
      | (_, lineConfigs) <- lineConfigss
      , (InlineConfigTargetModule, conf) <- lineConfigs
      ]
  let
    perBinding = Map.fromListWith
      (<>)
      [ (n, conf)
      | (k, lineConfigs) <- lineConfigss
      , (target, conf) <- lineConfigs
      , n <- case target of
        InlineConfigTargetBinding s -> [s]
        InlineConfigTargetNextBinding | Just name <- Map.lookup k declNameMap ->
          [name]
        _ -> []
      ]
  let
    perKey = Map.fromListWith
      (<>)
      [ (k, conf)
      | (k, lineConfigs) <- lineConfigss
      , (target, conf) <- lineConfigs
      , case target of
        InlineConfigTargetNextDecl -> True
        InlineConfigTargetNextBinding | Nothing <- Map.lookup k declNameMap ->
          True
        _ -> False
      ]

  pure
    $ ( perModule
      , PerItemConfig { _icd_perBinding = perBinding, _icd_perKey = perKey }
      )


getTopLevelDeclNameMap :: GHC.ParsedSource -> TopLevelDeclNameMap
getTopLevelDeclNameMap (L _ (HsModule _ _name _exports _ decls)) =
  TopLevelDeclNameMap $ Map.fromList
    [ (mkAnnKey (toL decl), name)
    | decl <- decls
    , (name : _) <- [getDeclBindingNames decl]
    ]


-- | Exposes the transformation in an pseudo-pure fashion. The signature
-- contains `IO` due to the GHC API not exposing a pure parsing function, but
-- there should be no observable effects.
--
-- Note that this function ignores/resets all config values regarding
-- debugging, i.e. it will never use `trace`/write to stderr.
--
-- Note that the ghc parsing function used internally currently is wrapped in
-- `mask_`, so cannot be killed easily. If you don't control the input, you
-- may wish to put some proper upper bound on the input's size as a timeout
-- won't do.
parsePrintModule :: Config -> Text -> IO (Either [BrittanyError] Text)
parsePrintModule configWithDebugs inputText = runExceptT $ do
  let
    config = configWithDebugs { _conf_debug = _conf_debug staticDefaultConfig }
  let ghcOptions = config & _conf_forward & _options_ghc & runIdentity
  let config_pp = config & _conf_preprocessor
  let cppMode = config_pp & _ppconf_CPPMode & confUnpack
  let hackAroundIncludes = config_pp & _ppconf_hackAroundIncludes & confUnpack
  (anns, parsedSource, hasCPP) <- do
    let
      hackF s =
        if "#include" `isPrefixOf` s then "-- BRITANY_INCLUDE_HACK " ++ s else s
    let
      hackTransform = if hackAroundIncludes
        then List.intercalate "\n" . fmap hackF . lines'
        else id
    let
      cppCheckFunc dynFlags = if GHC.xopt GHC.Cpp dynFlags
        then case cppMode of
          CPPModeAbort -> return $ Left cppUnsupportedMessage
          CPPModeWarn -> return $ Right True
          CPPModeNowarn -> return $ Right True
        else return $ Right False
    parseResult <- lift $ parseModuleFromString
      ghcOptions
      "stdin"
      cppCheckFunc
      (hackTransform $ Text.unpack inputText)
    case parseResult of
      Left err -> throwE [ErrorInput err]
      Right x -> pure x
  (inlineConf, perItemConf) <-
    either (throwE . (: []) . uncurry ErrorMacroConfig) pure
      $ extractCommentConfigs anns (getTopLevelDeclNameMap parsedSource)
  let moduleConfig = cZipWith fromOptionIdentity config inlineConf
  let disableFormatting = moduleConfig & _conf_disable_formatting & confUnpack
  if disableFormatting
    then do
      return inputText
    else do
      (errsWarns, outputTextL) <- do
        let
          omitCheck =
            moduleConfig
              & _conf_errorHandling
              & _econf_omit_output_valid_check
              & confUnpack
        (ews, outRaw) <- if hasCPP || omitCheck
          then return
            $ pPrintModuleWithSource
              (Just inputText)
              moduleConfig
              perItemConf
              anns
              parsedSource
          else lift
            $ pPrintModuleAndCheckWithSource
              (Just inputText)
              moduleConfig
              perItemConf
              anns
              parsedSource
        let
          hackF s = fromMaybe s
            $ TextL.stripPrefix (TextL.pack "-- BRITANY_INCLUDE_HACK ") s
        pure $ if hackAroundIncludes
          then
            ( ews
            , TextL.intercalate (TextL.pack "\n")
            $ hackF
            <$> TextL.splitOn (TextL.pack "\n") outRaw
            )
          else (ews, outRaw)
      let
        customErrOrder ErrorInput{} = 4
        customErrOrder LayoutWarning{} = 0 :: Int
        customErrOrder ExactSourceFallback{} = -1
        customErrOrder ErrorOutputCheck{} = 1
        customErrOrder ErrorUnusedComment{} = 2
        customErrOrder ErrorUnknownNode{} = 3
        customErrOrder ErrorMacroConfig{} = 5
        customErrOrder ErrorCommentPlan{} = 6
      let
        isWarningOrError ExactSourceFallback{} = False
        isWarningOrError _ = True
        isFallback ExactSourceFallback{} = True
        isFallback _ = False
      let
        failOnFallback =
          moduleConfig
            & _conf_errorHandling
            & _econf_failOnExactSourceFallback
            & confUnpack
        hasErrors =
          (failOnFallback && any isFallback errsWarns)
            || if moduleConfig & _conf_errorHandling & _econf_Werror & confUnpack
              then any isWarningOrError errsWarns
              else 0 < maximum (-1 : fmap customErrOrder errsWarns)
      if hasErrors
        then throwE $ errsWarns
        else pure $ TextL.toStrict outputTextL



-- BrittanyErrors can be non-fatal warnings, thus both are returned instead
-- of an Either.
-- This should be cleaned up once it is clear what kinds of errors really
-- can occur.
pPrintModule
  :: Config
  -> PerItemConfig
  -> Anns
  -> GHC.ParsedSource
  -> ([BrittanyError], TextL.Text)
pPrintModule conf inlineConf anns parsedModule =
  pPrintModuleWithSource Nothing conf inlineConf anns parsedModule

pPrintModuleWithSource
  :: Maybe Text.Text
  -> Config
  -> PerItemConfig
  -> Anns
  -> GHC.ParsedSource
  -> ([BrittanyError], TextL.Text)
pPrintModuleWithSource originalSource conf inlineConf anns parsedModule =
  case normalizeCommentPlan anns of
    Left planErrors ->
      ( fmap (ErrorCommentPlan . show) planErrors
      , maybe
          (TextL.pack $ ExactPrint.exactPrint parsedModule)
          TextL.fromStrict
          originalSource
      )
    Right commentPlan ->
      let
        ((out, errs), debugStrings) =
          runIdentity
            $ MultiRWSS.runMultiRWSTNil
            $ MultiRWSS.withMultiWriterAW
            $ MultiRWSS.withMultiWriterAW
            $ MultiRWSS.withMultiWriterW
            $ MultiRWSS.withMultiReader commentPlan
            $ MultiRWSS.withMultiReader anns
            $ MultiRWSS.withMultiReader conf
            $ MultiRWSS.withMultiReader inlineConf
            $ MultiRWSS.withMultiReader (extractToplevelAnns parsedModule anns)
            $ do
                traceIfDumpConf "bridoc annotations raw" _dconf_dump_annotations
                  $ annsDoc anns
                ppModule originalSource parsedModule
        tracer = if Seq.null debugStrings
          then id
          else
            trace ("---- DEBUGMESSAGES ---- ")
              . foldr (seq . join trace) id debugStrings
        -- ExactPrint preserves unsupported nodes as a whole-module fallback.
        unknownNodeLocations = Data.Maybe.mapMaybe (\case
          ErrorUnknownNode _ ast -> Just $ show $ GHC.getLoc ast
          _ -> Nothing
          ) errs
        hasUnknownNode = not $ null unknownNodeLocations
        fallbackOutput = maybe
          (TextL.pack $ ExactPrint.exactPrint parsedModule)
          TextL.fromStrict
          originalSource
        reportFallbacks =
          (conf & _conf_debug & _dconf_dump_fallbacks & confUnpack)
            || ( conf
              & _conf_errorHandling
              & _econf_failOnExactSourceFallback
              & confUnpack
               )
        wholeModuleNotice =
          [ ExactSourceFallback
            $ renderFallbackNotice WholeModuleFallback
            $ location
          | reportFallbacks
          , location <- take 1 unknownNodeLocations
          ]
        errs' = if hasUnknownNode
          then filter (\case { ErrorUnknownNode{} -> False; _ -> True }) errs
            ++ wholeModuleNotice
          else errs
      in tracer $
        if hasUnknownNode
          then (errs', fallbackOutput)
          else (errs, Text.Builder.toLazyText out)
  -- unless () $ do
  --
  --   debugStrings `forM_` \s ->
  --     trace s $ return ()

-- | Additionally checks that the output parses and preserves every source
-- comment, appending errors when either invariant fails.
pPrintModuleAndCheck
  :: Config
  -> PerItemConfig
  -> Anns
  -> GHC.ParsedSource
  -> IO ([BrittanyError], TextL.Text)
pPrintModuleAndCheck conf inlineConf anns parsedModule = do
  pPrintModuleAndCheckWithSource Nothing conf inlineConf anns parsedModule

pPrintModuleAndCheckWithSource
  :: Maybe Text.Text
  -> Config
  -> PerItemConfig
  -> Anns
  -> GHC.ParsedSource
  -> IO ([BrittanyError], TextL.Text)
pPrintModuleAndCheckWithSource originalSource conf inlineConf anns parsedModule = do
  let ghcOptions = conf & _conf_forward & _options_ghc & runIdentity
  let (errs, output) =
        pPrintModuleWithSource originalSource conf inlineConf anns parsedModule
  parseResult <- parseModuleFromString
    ghcOptions
    "output"
    (\_ -> return $ Right ())
    (TextL.unpack output)
  let omitCommentCheck =
        conf
          & _conf_errorHandling
          .> _econf_omit_unused_comment_check
          .> confUnpack
      checkErrors = case parseResult of
        Left{} -> [ErrorOutputCheck]
        Right (outputAnns, outputModule, _) ->
          if omitCommentCheck
            then []
            else case (normalizeCommentPlan anns, normalizeCommentPlan outputAnns) of
              (Left planErrors, _) -> fmap (ErrorCommentPlan . show) planErrors
              (_, Left planErrors) -> fmap (ErrorCommentPlan . show) planErrors
              (Right inputPlan, Right outputPlan) ->
                [ ErrorUnusedComment
                    $ "Comment missing from formatted output: " ++ show commentText
                | commentText <- collectCommentContents parsedModule
                    List.\\ collectCommentContents outputModule
                ]
                  ++ [ ErrorCommentPlan
                        "Comment ownership, order, or role changed after formatting."
                     | commentPlanFingerprint inputPlan
                        /= commentPlanFingerprint outputPlan
                     ]
      errs' = errs ++ checkErrors
  return (errs', output)


-- used for testing mostly, currently.
-- TODO: use parsePrintModule instead and remove this function.
parsePrintModuleTests :: Config -> String -> Text -> IO (Either String Text)
parsePrintModuleTests conf filename input = do
  let inputStr = Text.unpack input
  parseResult <- parseModuleFromString
    (conf & _conf_forward & _options_ghc & runIdentity)
    filename
    (const . pure $ Right ())
    inputStr
  case parseResult of
    Left err -> return $ Left err
    Right (anns, parsedModule, _) -> runExceptT $ do
      (inlineConf, perItemConf) <-
        case extractCommentConfigs anns (getTopLevelDeclNameMap parsedModule) of
          Left err -> throwE $ "error in inline config: " ++ show err
          Right x -> pure x
      let moduleConf = cZipWith fromOptionIdentity conf inlineConf
      let
        omitCheck =
          conf
            & _conf_errorHandling
            .> _econf_omit_output_valid_check
            .> confUnpack
      (errs, ltext) <- if omitCheck
        then return
          $ pPrintModuleWithSource
            (Just input)
            moduleConf
            perItemConf
            anns
            parsedModule
        else lift
          $ pPrintModuleAndCheckWithSource
            (Just input)
            moduleConf
            perItemConf
            anns
            parsedModule
      let
        failOnFallback =
          moduleConf
            & _conf_errorHandling
            & _econf_failOnExactSourceFallback
            & confUnpack
        actionableErrors = filter (\case
          ExactSourceFallback{} -> failOnFallback
          _ -> True
          ) errs
      if null actionableErrors
        then pure $ TextL.toStrict $ ltext
        else
          let
            errStrs = actionableErrors <&> \case
              ErrorInput str -> str
              ErrorUnusedComment str -> str
              ErrorCommentPlan str -> str
              LayoutWarning str -> str
              ExactSourceFallback str -> str
              ErrorUnknownNode str _ -> str
              ErrorMacroConfig str _ -> "when parsing inline config: " ++ str
              ErrorOutputCheck -> "Output is not syntactically valid."
          in throwE $ "pretty printing error(s):\n" ++ List.unlines errStrs

-- this approach would for if there was a pure GHC.parseDynamicFilePragma.
-- Unfortunately that does not exist yet, so we cannot provide a nominally
-- pure interface.

-- parsePrintModuleTests :: Text -> Either String Text
-- parsePrintModuleTests input = do
--   let dflags = GHC.unsafeGlobalDynFlags
--   let fakeFileName = "SomeTestFakeFileName.hs"
--   let pragmaInfo = GHC.getOptions
--         dflags
--         (GHC.stringToStringBuffer $ Text.unpack input)
--         fakeFileName
--   (dflags1, _, _) <- GHC.parseDynamicFilePragma dflags pragmaInfo
--   let parseResult = ExactPrint.Parsers.parseWith
--         dflags1
--         fakeFileName
--         GHC.parseModule
--         inputStr
--   case parseResult of
--     Left (_, s) -> Left $ "parsing error: " ++ s
--     Right (anns, parsedModule) -> do
--       let (out, errs) = runIdentity
--                       $ runMultiRWSTNil
--                       $ Control.Monad.Trans.MultiRWS.Lazy.withMultiWriterAW
--                       $ Control.Monad.Trans.MultiRWS.Lazy.withMultiWriterW
--                       $ Control.Monad.Trans.MultiRWS.Lazy.withMultiReader anns
--                       $ ppModule parsedModule
--       if (not $ null errs)
--         then do
--           let errStrs = errs <&> \case
--                 ErrorUnusedComment str -> str
--           Left $ "pretty printing error(s):\n" ++ List.unlines errStrs
--         else return $ TextL.toStrict $ Text.Builder.toLazyText out

toLocal :: Config -> Anns -> Text.Text -> PPMLocal a -> PPM a
toLocal conf anns source m = do
  commentPlan <- mAsk
  (x, write) <-
    lift $ MultiRWSS.runMultiRWSTAW
      (conf :+: anns :+: OriginalSource source :+: commentPlan :+: HNil)
      HNil
      m
  MultiRWSS.mGetRawW >>= \w -> MultiRWSS.mPutRawW (w `mappend` write)
  pure x

ppModule :: Maybe Text.Text -> GenLocated SrcSpan (HsModule GhcPs) -> PPM ()
ppModule originalSource lmod@(L _loc _m@(HsModule _ _name _exports imports decls)) = do
  let exactSource = fromMaybe (Text.pack $ ExactPrint.exactPrint lmod) originalSource
  let sourceCommentPositions = collectCommentPositions lmod
  annGroups <- mAsk
  defaultAnns <- do
    let annKey = mkAnnKey lmod
    let annMap = Map.findWithDefault Map.empty annKey annGroups
    let isEof = (== AnnEofPos)
    let overAnnsDP f a = a { annsDP = f $ annsDP a }
    -- Clear comments from all annotations in defaultAnns. These are
    -- module-level annotations that leak into every declaration's
    -- _lstate_comments; comments on them are either already emitted by
    -- ppPreamble or belong to a specific declaration (not the module).
    -- Without clearing, they produce spurious ErrorUnusedComment.
    let clearComments a = a
          { annPriorComments = []
          , annFollowingComments = []
          }
    pure $ fmap (clearComments . overAnnsDP (filter $ isEof . fst)) annMap

  (post, preambleEndsLine) <- ppPreamble lmod
  let toL x = L (getLocA x) (unLoc x)
  let annotationFor key = Map.lookup key annGroups >>= Map.lookup key
  let importUnit limport =
        let node = toL limport
        in (`topLevelUnit` annotationFor (mkAnnKey node))
          <$> EP.srcSpanToRealSpan (getLocA limport)
  let declUnit ldecl =
        let node = toL ldecl
        in (`topLevelUnit` annotationFor (mkAnnKey node))
          <$> EP.srcSpanToRealSpan (getLocA ldecl)
  let moduleUnit = (`topLevelUnit` annotationFor (mkAnnKey $ toL lmod))
        <$> moduleWhereSpan _m
  let preambleUnit =
        Data.Maybe.listToMaybe
          (reverse $ Data.Maybe.mapMaybe importUnit imports)
          <|> moduleUnit
  let declUnits = declUnit <$> decls
  let previousUnits = preambleUnit : declUnits
  let needsSeparators =
        (Data.Maybe.isJust _name || not (null imports)) : repeat True
  let completedSeparatorLines =
        (if preambleEndsLine then 1 else 0) : repeat 0
  let isFallbackNotice ExactSourceFallback{} = True
      isFallbackNotice _ = False
  forM_ (zip3 decls (zip needsSeparators completedSeparatorLines)
      $ zip previousUnits declUnits) $
      \(decl, (needsSeparator, completedLines), (previousUnit, currentUnit)) -> do
    let separatorLines = case (previousUnit, currentUnit) of
          (Just previous, Just current) ->
            topLevelSeparatorLines previous current
          _ -> 1
    when needsSeparator $ replicateM_ (max 0 $ separatorLines - completedLines)
      $ mTell (Text.Builder.fromString "\n")
    let decl' = toL decl
    let declAnnKey = mkAnnKey decl'
    let declBindingNames = getDeclBindingNames decl
    inlineConf <- mAsk
    let mDeclConf = Map.lookup declAnnKey $ _icd_perKey inlineConf
    let
      mBindingConfs =
        declBindingNames <&> \n -> Map.lookup n $ _icd_perBinding inlineConf
    filteredAnns <- mAsk <&> \annMap ->
      -- Declaration-specific annotations take priority over defaults
      -- (defaultAnns has comments cleared to avoid ErrorUnusedComment)
      Map.union (Map.findWithDefault Map.empty declAnnKey annMap) defaultAnns
    commentPlan <- mAsk
    let exactDeclText = nodeSourceSlice exactSource decl' filteredAnns commentPlan
    let hasSourceComments = case EP.srcSpanToRealSpan $ getLocA decl' of
          Nothing -> False
          Just span' -> any
            (\(line, _) ->
              line >= GHC.srcSpanStartLine span'
                && line <= GHC.srcSpanEndLine span'
            )
            sourceCommentPositions

    traceIfDumpConf
        "bridoc annotations filtered/transformed"
        _dconf_dump_annotations
      $ annsDoc filteredAnns

    config <- mAsk

    let
      config' = cZipWith fromOptionIdentity config
        $ mconcat (catMaybes (mBindingConfs ++ [mDeclConf]))

    let exactprintOnly = config' & _conf_roundtrip_exactprint_only & confUnpack
    toLocal config' filteredAnns exactSource $ do
      bd <- if exactprintOnly
        then briDocMToPPM
          $ briDocByExactNoComment ExactPrintOnlyFallback decl'
        else do
          (r, errs, debugs) <-
            briDocMToPPMInner
              $ layoutDeclWithExactText exactDeclText hasSourceComments decl'
          mTell debugs
          mTell errs
          if all isFallbackNotice errs
            then pure r
            else briDocMToPPM
              $ briDocByExactNoComment WholeModuleFallback decl'
      layoutBriDoc bd

  let
    finalComments = filter
      (fst .> \case
        AnnComment{} -> True
        _ -> False
      )
      post
  post `forM_` \case
    (AnnComment c, l) -> do
      ppmMoveToExactLoc l
      mTell $ Text.Builder.fromString (commentContents c)
    (AnnEofPos, (EP.DP (eofZ, eofX))) ->
      let
        folder (acc, _) (kw, EP.DP (y, x)) = case kw of
          AnnComment cm | span <- commentIdentifier cm ->
            case EP.srcSpanToRealSpan span of
              Just rspan ->
                ( acc + y + GHC.srcSpanEndLine rspan - GHC.srcSpanStartLine rspan
                , x + GHC.srcSpanEndCol rspan - GHC.srcSpanStartCol rspan
                )
              Nothing -> (acc + y, x)
          _ -> (acc + y, x)
        (cmY, cmX) = foldl' folder (0, 0) finalComments
      in ppmMoveToExactLoc $ EP.DP (eofZ - cmY, eofX - cmX)
    _ -> return ()

getDeclBindingNames :: LHsDecl GhcPs -> [String]
getDeclBindingNames ldecl = case unLoc ldecl of
  SigD _ (TypeSig _ ns _) -> ns <&> \(L _ n) -> Text.unpack (rdrNameToText n)
  ValD _ (FunBind _ (L _ n) _) -> [Text.unpack $ rdrNameToText n]
  _ -> []


-- Prints the information associated with the module annotation
-- This includes the imports
ppPreamble
  :: GenLocated SrcSpan (HsModule GhcPs)
  -> PPM ([(KeywordId, EP.DeltaPos)], Bool)
ppPreamble lmod@(L loc m@HsModule{}) = do
  annGroups <- mAsk
  let filteredAnns =
        Map.findWithDefault Map.empty (mkAnnKey (toL lmod)) annGroups
    -- Since ghc-exactprint adds annotations following (implicit)
    -- modules to both HsModule and the elements in the module
    -- this can cause duplication of comments. So strip
    -- attached annotations that come after the module's where
    -- from the module node
  config <- mAsk
  let
    shouldReformatPreamble =
      config & _conf_layout & _lconfig_reformatModulePreamble & confUnpack
    exactSource = Text.pack $ ExactPrint.exactPrint lmod
    canReformatPreamble =
      shouldReformatPreamble && not (preambleRequiresExactSource exactSource m)

  let
    (filteredAnns', post) =
      case Map.lookup (mkAnnKey (toL lmod)) filteredAnns of
        Nothing -> (filteredAnns, [])
        Just mAnn ->
          let
            modAnnsDp = annsDP mAnn
            isWhere (G AnnWhere) = True
            isWhere _ = False
            isEof (AnnEofPos) = True
            isEof _ = False
            whereInd = List.findIndex (isWhere . fst) modAnnsDp
            eofInd = List.findIndex (isEof . fst) modAnnsDp
            (pre, post') = case (whereInd, eofInd) of
              (Nothing, Nothing) -> ([], modAnnsDp)
              (Just i, Nothing) -> List.splitAt (i + 1) modAnnsDp
              (Nothing, Just _i) -> ([], modAnnsDp)
              (Just i, Just j) -> List.splitAt (min (i + 1) j) modAnnsDp
            mAnn' = mAnn { annsDP = pre }
            filteredAnns'' =
              Map.insert (mkAnnKey (toL lmod)) mAnn' filteredAnns
          in (filteredAnns'', post')
  traceIfDumpConf
      "bridoc annotations filtered/transformed"
      _dconf_dump_annotations
    $ annsDoc filteredAnns'

  -- Emit module prior comments (LANGUAGE pragmas, brittany config, etc.)
  -- first; layoutModule and processDefault don't include them. Our
  -- ExtractAnns puts these in annPriorComments.
  let modKey = mkAnnKey (toL lmod)
  let modPriorComments = maybe [] annPriorComments (Map.lookup modKey filteredAnns)
  when canReformatPreamble $ do
    forM_ (zip [0::Int ..] modPriorComments) $
      \(idx, (c, dp)) -> do
        -- For comments after the first, the DP accounts for the newline we
        -- added after the previous comment, so subtract 1 row.
        let dp' = if idx > 0
              then case dp of
                EP.DP (y, x) | y > 0 -> EP.DP (y - 1, x)
                _ -> dp
              else dp
        ppmMoveToExactLoc dp'
        mTell $ Text.Builder.fromString (commentContents c)
        mTell $ Text.Builder.fromString "\n"
    let sourceSeparatorLines = do
          (lastComment, _) <- Data.Maybe.listToMaybe $ reverse modPriorComments
          commentSpan <- EP.srcSpanToRealSpan $ commentIdentifier lastComment
          let annotatedUnit node = do
                nodeSpan <- EP.srcSpanToRealSpan $ getLocA node
                let nodeKey = mkAnnKey $ toL node
                pure $ topLevelUnit nodeSpan
                  $ Map.lookup nodeKey annGroups >>= Map.lookup nodeKey
          followerUnit <- ((`topLevelUnit` Nothing) <$> moduleKeywordSpan m)
            <|> (Data.Maybe.listToMaybe (hsmodImports m) >>= annotatedUnit)
            <|> (Data.Maybe.listToMaybe (hsmodDecls m) >>= annotatedUnit)
          pure $ preambleSeparatorLines commentSpan followerUnit
    replicateM_ (max 0 $ fromMaybe 1 sourceSeparatorLines - 1)
      $ mTell (Text.Builder.fromString "\n")

  -- Clear prior comments after the native path emits them so layoutBriDoc
  -- does not re-output them via BDAnnotationPrior.
  let clearModPriorComments anns = case Map.lookup modKey anns of
        Nothing -> anns
        Just ann -> Map.insert modKey (ann { annPriorComments = [] }) anns
      filteredAnns'' =
        if canReformatPreamble
          then clearModPriorComments filteredAnns'
          else filteredAnns'

  if canReformatPreamble
    then toLocal config filteredAnns'' exactSource $ withTransformedAnns lmod $ do
      briDoc <- briDocMToPPM $ layoutModuleWithExactText exactSource lmod
      layoutBriDoc briDoc
    else do
      let emptyModule = L loc m { hsmodDecls = [] }
      MultiRWSS.withMultiReader filteredAnns'' $ processDefault emptyModule
  return (post, not canReformatPreamble)

_sigHead :: Sig GhcPs -> String
_sigHead = \case
  TypeSig _ names _ ->
    "TypeSig " ++ intercalate "," (Text.unpack . lrdrNameToText <$> names)
  _ -> "unknown sig"

_bindHead :: HsBind GhcPs -> String
_bindHead = \case
  FunBind _ fId _ -> "FunBind " ++ (Text.unpack $ lrdrNameToText $ fId)
  PatBind _ _ _ _ -> "PatBind smth"
  _ -> "unknown bind"



layoutBriDoc :: BriDocNumbered -> PPMLocal ()
layoutBriDoc briDoc = do
  -- first step: transform the briDoc.
  briDoc' <- MultiRWSS.withMultiStateS BDEmpty $ do
    -- Note that briDoc is BriDocNumbered, but state type is BriDoc.
    -- That's why the alt-transform looks a bit special here.
    traceIfDumpConf "bridoc raw" _dconf_dump_bridoc_raw
      $ briDocToDoc
      $ unwrapBriDocNumbered
      $ briDoc
    -- bridoc transformation: remove alts
    transformAlts briDoc >>= mSet
    mGet
      >>= briDocToDoc
      .> traceIfDumpConf "bridoc post-alt" _dconf_dump_bridoc_simpl_alt
    -- bridoc transformation: float stuff in
    mGet >>= transformSimplifyFloating .> mSet
    mGet
      >>= briDocToDoc
      .> traceIfDumpConf
           "bridoc post-floating"
           _dconf_dump_bridoc_simpl_floating
    -- bridoc transformation: par removal
    mGet >>= transformSimplifyPar .> mSet
    mGet
      >>= briDocToDoc
      .> traceIfDumpConf "bridoc post-par" _dconf_dump_bridoc_simpl_par
    -- bridoc transformation: float stuff in
    mGet >>= transformSimplifyColumns .> mSet
    mGet
      >>= briDocToDoc
      .> traceIfDumpConf "bridoc post-columns" _dconf_dump_bridoc_simpl_columns
    -- bridoc transformation: indent
    mGet >>= transformSimplifyIndent .> mSet
    mGet
      >>= briDocToDoc
      .> traceIfDumpConf "bridoc post-indent" _dconf_dump_bridoc_simpl_indent
    mGet
      >>= briDocToDoc
      .> traceIfDumpConf "bridoc final" _dconf_dump_bridoc_final
    -- -- convert to Simple type
    -- simpl <- mGet <&> transformToSimple
    -- return simpl

  anns :: Anns <- mAsk

  let
    state = LayoutState
      { _lstate_baseYs = [0]
      , _lstate_curYOrAddNewline = Right 0 -- important that we dont use left
                                           -- here because moveToAnn stuff
                                           -- of the first node needs to do
                                           -- its thing properly.
      , _lstate_indLevels = [0]
      , _lstate_indLevelLinger = 0
      , _lstate_comments = anns
      , _lstate_commentCol = Nothing
      , _lstate_addSepSpace = Nothing
      , _lstate_commentNewlines = 0
      }

  state' <- MultiRWSS.withMultiStateS state $ layoutBriDocM briDoc'

  let
    remainingComments =
      [ c
      | (AnnKey _ con, elemAnns) <- Map.toList
        (_lstate_comments state')
    -- With the new import layouter, we manually process comments
    -- without relying on the backend to consume the comments out of
    -- the state/map. So they will end up here, and we need to ignore
    -- them.
      , unConName con /= "ImportDecl"
      , c <- extractAllComments elemAnns
      ]
  config <- mAsk
  unless
    (config & _conf_errorHandling & _econf_omit_unused_comment_check & confUnpack)
    $ remainingComments
      `forM_` (fst .> show .> ErrorUnusedComment .> (: []) .> mTell)

  return $ ()
