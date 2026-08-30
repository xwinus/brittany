module DelimiterLayoutSpec (spec) where

import qualified Data.Text                               as Text
import           Language.Haskell.Brittany.Internal.Delimiter
                                                          ( prepareSelectedDelimiter
                                                          )
import           Language.Haskell.Brittany.Internal.Delimiter.Types
import           Language.Haskell.Brittany.Internal.ExactPrintCompat
                                                          ( AnnKey )
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
                                                          , ColSig(..)
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
      BDDelimited group -> delimitedSpec group `Hspec.shouldBe` squareSpec [] []
      _ -> Hspec.expectationFailure "delimiter wrapper was removed"

  Hspec.it "accepts an explicitly vertical standalone opener" $ do
    case prepareSelectedDelimiter
      (selectedGroup DelimiterVertical verticalGroup) of
      Right (layout, document) -> do
        layout `Hspec.shouldBe` DelimiterVertical
        document == verticalGroup `Hspec.shouldBe` True
      Left delimiterError -> Hspec.expectationFailure $ show delimiterError

  Hspec.it "rejects an accidental standalone attached opener" $ do
    case prepareSelectedDelimiter
      (selectedGroup DelimiterAttached verticalGroup) of
      Left delimiterError -> delimiterError `Hspec.shouldBe`
        AccidentalStandaloneDelimiter DelimiterAttached (Text.pack "[")
      Right{} -> Hspec.expectationFailure "attached standalone opener accepted"

  Hspec.it "rejects incomplete separator evidence" $ do
    validateDelimitedGroup invalidSeparatorGroup
      `Hspec.shouldBe` Left (InvalidDelimiterSeparatorCount 1 0)

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
selectedGroup layout document = DelimitedGroup
  { delimitedSpec = squareSpec [] []
  , delimitedAlternatives = [DelimitedAlternative layout document]
  }

invalidSeparatorGroup :: DelimitedGroup BriDoc
invalidSeparatorGroup = DelimitedGroup
  { delimitedSpec = squareSpec [Nothing, Nothing] []
  , delimitedAlternatives =
      [DelimitedAlternative DelimiterCompact compactGroup]
  }

squareSpec :: [Maybe AnnKey] -> [Text.Text] -> DelimiterSpec
squareSpec = mkDelimiterSpec
  SquareBracketsDelimiter
  (Text.pack "[")
  (Text.pack "]")
  Nothing
