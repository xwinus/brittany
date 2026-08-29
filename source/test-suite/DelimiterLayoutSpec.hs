module DelimiterLayoutSpec (spec) where

import qualified Data.Text                               as Text
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
