{-# LANGUAGE LambdaCase #-}

module DelimiterLayoutSpec (spec) where

import qualified Data.Text                               as Text
import           Language.Haskell.Brittany                ( parsePrintModule
                                                          , staticDefaultConfig
                                                          )
import           Language.Haskell.Brittany.Internal.Delimiter
                                                          ( delimiterLayoutDocuments
                                                          , prepareSelectedDelimiter
                                                          , validateRenderedDelimiter
                                                          )
import           Language.Haskell.Brittany.Internal.Delimiter.Types
import           Language.Haskell.Brittany.Internal.Transformations.Columns
                                                          ( transformSimplifyColumns
                                                          )
import           Language.Haskell.Brittany.Internal.Transformations.Floating
                                                          ( transformSimplifyFloating
                                                          )
import           Language.Haskell.Brittany.Internal.Transformations.Indent
                                                          ( transformSimplifyIndent
                                                          )
import           Language.Haskell.Brittany.Internal.Transformations.Par
                                                          ( transformSimplifyPar
                                                          )
import           Language.Haskell.Brittany.Internal.Types ( BrIndent(..)
                                                          , BriDoc(..)
                                                          , BriDocF(..)
                                                          , BriDocNumbered
                                                          , ColSig(..)
                                                          , unwrapBriDocNumbered
                                                          )
import qualified Test.Hspec                              as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "delimiter-aware BriDoc composition" $ do
  Hspec.it "keeps an attached opener inside a transformed application" $ do
    hasAttachedOpener (transformed attachedApplication) `Hspec.shouldBe` True

  Hspec.it "retains an explicitly vertical delimiter block" $ do
    transformed verticalBlock == verticalBlock `Hspec.shouldBe` True

  Hspec.it "leaves a compact delimiter group unchanged" $ do
    transformed compactGroup == compactGroup `Hspec.shouldBe` True

  Hspec.it "preserves first-class delimiter metadata through transforms" $ do
    case transformed firstClassCompactGroup of
      BDDelimited group ->
        delimiterSequenceId (delimitedSequence group)
          `Hspec.shouldBe` firstClassGroupId
      _ -> Hspec.expectationFailure "delimiter wrapper was removed"

  Hspec.it "accepts an explicitly vertical standalone opener" $ do
    case prepareSelectedDelimiter
      (selectedGroup DelimiterVertical verticalGroup) of
      Right (layout, document) -> do
        layout `Hspec.shouldBe` DelimiterVertical
        document == verticalGroup `Hspec.shouldBe` True
      Left delimiterError -> Hspec.expectationFailure $ show delimiterError

  Hspec.it "rejects a selected layout outside the allowed render plan" $ do
    case prepareSelectedDelimiter
      (selectDelimitedDocument DelimiterAttached verticalGroup compactOnlyGroup) of
      Left delimiterError -> delimiterError
        `Hspec.shouldBe` SelectedDelimiterLayoutNotAllowed DelimiterAttached
      Right{} -> Hspec.expectationFailure "unexpected layout was accepted"

  Hspec.it "rejects incomplete separator evidence" $ do
    validateDelimitedGroup invalidSeparatorGroup
      `Hspec.shouldBe` Left (InvalidDelimiterSeparatorCount 1 0)

  Hspec.it
    "builds typed list-comprehension render plans with stable identities"
    $ do
        let sequence' = delimitedSequence renderPlanGroup
            children  = delimiterSequenceChildren sequence'
            separators = delimiterSequenceSeparators sequence'
        (delimiterChildId <$> children) `Hspec.shouldBe`
          [ DelimiterChildId renderPlanGroupId 0
          , DelimiterChildId renderPlanGroupId 1
          , DelimiterChildId renderPlanGroupId 2
          ]
        (delimiterSeparatorId <$> separators) `Hspec.shouldBe`
          [ DelimiterSeparatorId renderPlanGroupId 0
          , DelimiterSeparatorId renderPlanGroupId 1
          ]
        case delimiterLayoutDocuments 41 renderPlanGroup of
          Left delimiterError -> Hspec.expectationFailure $ show delimiterError
          Right plans -> do
            (fst <$> plans) `Hspec.shouldBe`
              [DelimiterCompact, DelimiterAttached]
            mapM_
              (\(layout, document) -> do
                validateRenderedDelimiter layout renderPlanGroup document
                  `Hspec.shouldBe` Right ()
                standaloneSeparators (unwrapBriDocNumbered document)
                  `Hspec.shouldBe` []
              )
              plans

  Hspec.it
    "keeps the Internal list-comprehension separator attached for three passes"
    $ do
        firstPass  <- formatChecked internalReproducer
        secondPass <- formatChecked firstPass
        thirdPass  <- formatChecked secondPass
        mapM_ assertNoStandaloneSeparators [firstPass, secondPass, thirdPass]
        secondPass `Hspec.shouldBe` firstPass
        thirdPass `Hspec.shouldBe` firstPass

  Hspec.it "rejects a deterministic malformed separator link" $ do
    validateDelimitedGroup malformedSeparatorLinkGroup
      `Hspec.shouldBe` Left
        (InvalidDelimiterSeparatorLink
          (DelimiterSeparatorId renderPlanGroupId 0)
          (DelimiterChildId renderPlanGroupId 0)
          (DelimiterChildId renderPlanGroupId 1)
        )

transformed :: BriDoc -> BriDoc
transformed =
  transformSimplifyIndent
    . transformSimplifyColumns
    . transformSimplifyPar
    . transformSimplifyFloating

attachedApplication :: BriDoc
attachedApplication = BDPar
  BrIndentRegular
  (BDCols
    (ColApp $ Text.pack "application")
    [ BDLit $ Text.pack "apply"
    , BDSeq
      [ BDLit $ Text.pack "("
      , BDPar BrIndentRegular
              (BDLit $ Text.pack "\\case")
              (BDLit $ Text.pack "Nothing -> False")
      ]
    ]
  )
  (BDLit $ Text.pack ")")

hasAttachedOpener :: BriDoc -> Bool
hasAttachedOpener document = case document of
  BDCols _ [BDLit functionName, BDPar _ line _] ->
    functionName == Text.pack "apply" && attachedLine line
  _ -> False
 where
  attachedLine (BDSeq (BDLit opener : _)) = opener == Text.pack "("
  attachedLine _                          = False

verticalBlock :: BriDoc
verticalBlock = BDLines
  [ BDLit $ Text.pack "("
  , BDEnsureIndent BrIndentRegular $ BDLit $ Text.pack "body"
  , BDLit $ Text.pack ")"
  ]

compactGroup :: BriDoc
compactGroup =
  BDSeq [BDLit $ Text.pack "(", BDLit $ Text.pack "body", BDLit $ Text.pack ")"]

firstClassCompactGroup :: BriDoc
firstClassCompactGroup = BDDelimited
  $ selectedGroup DelimiterCompact
  $ BDSeq
    [ BDLit $ Text.pack "["
    , BDLit $ Text.pack "body"
    , BDLit $ Text.pack "]"
    ]

verticalGroup :: BriDoc
verticalGroup = BDLines
  [ BDLit $ Text.pack "["
  , BDEnsureIndent BrIndentRegular $ BDLit $ Text.pack "body"
  , BDLit $ Text.pack "]"
  ]

selectedGroup :: DelimiterLayout -> BriDoc -> DelimitedGroup BriDoc
selectedGroup layout document = selectDelimitedDocument layout document
  $ mkGroup firstClassGroupId [layout]

compactOnlyGroup :: DelimitedGroup BriDoc
compactOnlyGroup = mkGroup (DelimiterGroupId 8) [DelimiterCompact]

invalidSeparatorGroup :: DelimitedGroup BriDoc
invalidSeparatorGroup = valid
  { delimitedSequence = sequence'
      { delimiterSequenceSeparators = []
      }
  }
 where
  valid = mkDelimitedGroup
    9
    SquareBracketsDelimiter
    (Text.pack "[")
    (Text.pack "]")
    Nothing
    [ (Nothing, PresentDelimiterChild, BDLit $ Text.pack "first")
    , (Nothing, PresentDelimiterChild, BDLit $ Text.pack "second")
    ]
    [ ( RepeatedDelimiterSeparator
      , Text.pack ","
      , AttachSeparatorRight
      )
    ]
    DelimiterIndentRegular
    LeadingDelimiterSeparators
    [DelimiterCompact]
  sequence' = delimitedSequence valid

mkGroup :: DelimiterGroupId -> [DelimiterLayout] -> DelimitedGroup BriDoc
mkGroup (DelimiterGroupId rawGroupId) layouts = mkDelimitedGroup
  rawGroupId
  SquareBracketsDelimiter
  (Text.pack "[")
  (Text.pack "]")
  Nothing
  [(Nothing, PresentDelimiterChild, BDLit $ Text.pack "body")]
  []
  DelimiterIndentRegular
  LeadingDelimiterSeparators
  layouts

firstClassGroupId :: DelimiterGroupId
firstClassGroupId = DelimiterGroupId 7

renderPlanGroupId :: DelimiterGroupId
renderPlanGroupId = DelimiterGroupId 41

renderPlanGroup :: DelimitedGroup BriDocNumbered
renderPlanGroup = mkDelimitedGroup
  41
  SquareBracketsDelimiter
  (Text.pack "[")
  (Text.pack "]")
  Nothing
  [ (Nothing, PresentDelimiterChild, numberedLit 1 "result")
  , (Nothing, PresentDelimiterChild, numberedLit 2 "value <- values")
  , (Nothing, PresentDelimiterChild, numberedLit 3 "value > 0")
  ]
  [ (ListComprehensionBar, Text.pack "|", AttachSeparatorRight)
  , (RepeatedDelimiterSeparator, Text.pack ",", AttachSeparatorRight)
  ]
  DelimiterIndentRegular
  LeadingDelimiterSeparators
  [DelimiterCompact, DelimiterAttached]

malformedSeparatorLinkGroup :: DelimitedGroup BriDocNumbered
malformedSeparatorLinkGroup = renderPlanGroup
  { delimitedSequence = sequence'
      { delimiterSequenceSeparators = malformedSeparators
      }
  }
 where
  sequence' = delimitedSequence renderPlanGroup
  malformedSeparators = case delimiterSequenceSeparators sequence' of
    first : rest -> first
      { delimiterSeparatorLeft = DelimiterChildId renderPlanGroupId 1
      }
      : rest
    [] -> []

numberedLit :: Int -> String -> BriDocNumbered
numberedLit nodeId value = (nodeId, BDFLit $ Text.pack value)

standaloneSeparators :: BriDoc -> [Text.Text]
standaloneSeparators = \case
  BDLines rows -> concatMap standaloneLine rows
    ++ concatMap standaloneSeparators rows
  BDSeq children -> concatMap standaloneSeparators children
  BDCols _ children -> concatMap standaloneSeparators children
  BDAddBaseY _ child -> standaloneSeparators child
  BDBaseYPushCur child -> standaloneSeparators child
  BDBaseYPop child -> standaloneSeparators child
  BDIndentLevelPushCur child -> standaloneSeparators child
  BDIndentLevelPop child -> standaloneSeparators child
  BDPar _ line indented ->
    standaloneSeparators line ++ standaloneSeparators indented
  BDDelimited _ -> []
  BDAlt children -> concatMap standaloneSeparators children
  BDForwardLineMode child -> standaloneSeparators child
  BDAnnotationPrior _ _ child -> standaloneSeparators child
  BDAnnotationKW _ _ child -> standaloneSeparators child
  BDAnnotationRest _ child -> standaloneSeparators child
  BDMoveToKWDP _ _ _ child -> standaloneSeparators child
  BDEnsureIndent _ child -> standaloneSeparators child
  BDForceMultiline child -> standaloneSeparators child
  BDForceSingleline child -> standaloneSeparators child
  BDColumnsLimit _ child -> standaloneSeparators child
  BDNonBottomSpacing _ child -> standaloneSeparators child
  BDSetParSpacing child -> standaloneSeparators child
  BDForceParSpacing child -> standaloneSeparators child
  BDDebug _ child -> standaloneSeparators child
  _ -> []
 where
  standaloneLine line = case line of
    BDLit token | token `elem` structuralSeparators -> [token]
    BDSeq [BDLit token] | token `elem` structuralSeparators -> [token]
    _ -> []

structuralSeparators :: [Text.Text]
structuralSeparators = Text.pack <$> [",", "|"]

formatChecked :: String -> IO String
formatChecked source =
  parsePrintModule staticDefaultConfig (Text.pack source) >>= \case
    Left errors -> Hspec.expectationFailure
      ("formatting returned " ++ show (length errors) ++ " errors")
      >> fail "formatting failed"
    Right output -> pure $ Text.unpack output

assertNoStandaloneSeparators :: String -> Hspec.Expectation
assertNoStandaloneSeparators source =
  [ line
  | line <- Text.lines $ Text.pack source
  , Text.strip line `elem` structuralSeparators
  ] `Hspec.shouldBe` []

internalReproducer :: String
internalReproducer = unlines
  [ "module InternalReproducer where"
  , ""
  , "import qualified Data.Map as Map"
  , ""
  , "remainingComments state' ="
  , "  [ c"
  , "  | (AnnKey _ con, elemAnns) <- Map.toList (_lstate_comments state')"
  , "  -- With the new import layouter, we manually process comments"
  , "  -- without relying on the backend to consume the comments out of"
  , "  -- the state/map. So they will end up here, and we need to ignore"
  , "  -- them."
  , "  , unConName con /= \"ImportDecl\""
  , "  , c <- extractAllComments elemAnns"
  , "  ]"
  ]
