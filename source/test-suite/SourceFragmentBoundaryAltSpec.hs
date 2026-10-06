{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module SourceFragmentBoundaryAltSpec (spec) where

import Control.Monad (forM_)
import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import Data.Functor.Identity (Identity(..), runIdentity)
import qualified Data.Generics as Generics
import Data.Semigroup (Last(..))
import Data.Sequence (Seq)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified GHC.Data.FastString as FastString
import qualified GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Brittany
  ( CConfig(..), CLayoutConfig(..), Config, staticDefaultConfig )
import Language.Haskell.Brittany.Internal.Config.Types (AltChooser(..))
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as EP
import Language.Haskell.Brittany.Internal.SourceComment.Types
import Language.Haskell.Brittany.Internal.Transformations.Alt
  ( getSpacing, transformAlts )
import Language.Haskell.Brittany.Internal.Transformations.Alt.Comments
  ( containsCommentLineBreak, sequenceRequiresCommentLineBreak )
import Language.Haskell.Brittany.Internal.Types
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "source-fragment alternative boundaries" $ do
  forM_ [AltChooserShallowBest, AltChooserBoundedSearch 3] $ \chooser -> do
    let check name = Hspec.it $ name ++ " with " ++ show chooser
        selects columns expected document =
          selectedMarkers (choose chooser columns document)
            `Hspec.shouldBe` [expected]

    check "uses the next line for an alternative after a long fragment" $
      selects 40 "compact" $ afterFragment $ alternative 20

    check "keeps a boundary pending through separators and empty nodes" $
      selects 40 "compact" $ afterFragment $ sequenceNode 20
        [ (21, BDFSeparator), (22, BDFEmpty), alternative 20 ]

    check "detects a boundary in the preceding column" $
      selects 40 "compact" $ (0, BDFCols ColOpPrefix
        [ sequenceNode 1 [literal 2 "= ", lineFragment 3]
        , alternative 20
        ])

    check "carries a boundary through annotation wrappers" $
      selects 40 "compact" $ sequenceNode 0
        [ (1, BDFAnnotationRest ownerKey
            (2, BDFAnnotationPrior PriorCommentSource ownerKey $ lineFragment 3))
        , alternative 20
        ]

    check "does not capture the old comment column as a hanging base" $
      selects 40 "compact" $ afterFragment
        (20, BDFBaseYPop (21, BDFBaseYPushCur $ alternative 20))

    check "respects a structural base before a pending hanging capture" $
      selects 24 "compact" $ afterFragment
        (20, BDFAddBaseY (BrIndentSpecial 4)
          (21, BDFBaseYPop (22, BDFBaseYPushCur $ alternative 20)))

    check "does not move the pending first literal when only its base changes" $
      selects 23 "compact" $ afterFragment
        (20, BDFAddBaseY (BrIndentSpecial 4)
          (21, BDFBaseYPop (22, BDFBaseYPushCur $ alternative 20)))

    check "fits a paragraph at the captured structural continuation base" $
      selects 24 "compact" $ afterFragment
        (20, BDFAddBaseY (BrIndentSpecial 4)
          (21, BDFBaseYPop (22, BDFBaseYPushCur $ paragraphChoice 20)))

    check "checks paragraph width against the captured continuation base" $
      selects 23 "wrapped" $ afterFragment
        (20, BDFAddBaseY (BrIndentSpecial 4)
          (21, BDFBaseYPop (22, BDFBaseYPushCur $ paragraphChoice 20)))

    check "uses the ensured indentation for the following alternative" $
      selects 24 "compact" $ afterFragment
        (20, BDFEnsureIndent (BrIndentSpecial 4) $ alternative 20)

    check "wraps a component exceeding its ensured indentation budget" $
      selects 23 "wrapped" $ afterFragment
        (20, BDFEnsureIndent (BrIndentSpecial 4) $ alternative 20)

    check "retains genuinely over-width expression wrapping" $
      selects 40 "wrapped" $ afterFragment $ alternative 41

    check "resets a pending boundary at an explicit paragraph" $
      selects 40 "compact" $ (0, BDFPar BrIndentNone
        (lineFragment 1) (alternative 20))

    check "resets a pending boundary at an explicit lines continuation" $
      selects 40 "compact" $ (0, BDFLines
        [lineFragment 1, alternative 20])

    check "allows an empty literal to consume the pending line" $
      selects 40 "compact" $ afterFragment $ sequenceNode 20
        [literal 21 "", alternative 20]

    check "rejects a forced single line containing a fragment and follower" $
      selects 200 "wrapped" $ forcedChoice $ sequenceNode 1
        [lineFragment 2, literal 3 "value"]

    check "keeps a terminal fragment eligible for a single line" $
      selects 200 "compact" $ forcedChoice $ sequenceNode 1
        [lineFragment 2, (3, BDFSeparator), (4, BDFEmpty)]

    check "keeps an inline block fragment on its actual code line" $
      selects 40 "wrapped" $ sequenceNode 0
        [fragment 1 "{- an inline block comment -}", alternative 20]

    check "retains a fitting inline block fragment and code" $
      selects 80 "compact" $ sequenceNode 0
        [fragment 1 "{- an inline block comment -}", alternative 20]

    check "does not change ordinary exact-print fragments into line breaks" $
      selects 40 "wrapped" $ sequenceNode 0
        [ (1, BDFExternal ownerKey False
            (ExactPrintSource Set.empty $ Text.replicate 30 $ Text.pack "x"))
        , alternative 20
        ]

    check "does not sum widths across a source-fragment line boundary" $
      selects 80 "compact" $ (100, BDFAlt
        [ (101, BDFDebug "compact" $ sequenceNode 1
            [lineFragment 2, literal 3 $ replicate 50 'x'])
        , (102, BDFDebug "wrapped" $ literal 4 "fallback")
        ])

    check "rejects an over-width follower after a source-fragment boundary" $
      selects 80 "wrapped" $ (100, BDFAlt
        [ (101, BDFDebug "compact" $ sequenceNode 1
            [lineFragment 2, literal 3 $ replicate 100 'x'])
        , (102, BDFDebug "wrapped" $ literal 4 "fallback")
        ])

  Hspec.it "recognizes a source-fragment boundary with following code" $ do
    let documents = [lineFragment 1, literal 2 "value"]
        document = sequenceNode 0 documents
    containsCommentLineBreak document `Hspec.shouldBe` True
    sequenceRequiresCommentLineBreak True documents `Hspec.shouldBe` True

  Hspec.it "does not require a line break for an empty fragment tail" $
    sequenceRequiresCommentLineBreak True
      [lineFragment 1, (2, BDFSeparator), (3, BDFEmpty)]
        `Hspec.shouldBe` False

  Hspec.it "keeps non-boundary block fragments out of boundary detection" $
    containsCommentLineBreak (fragment 1 "{- block -}")
      `Hspec.shouldBe` False

  Hspec.it "reports a fragment and follower as multiline spacing" $
    isMultilineSpacing (spacing $ sequenceNode 0
      [lineFragment 1, literal 2 "value"]) `Hspec.shouldBe` True

  Hspec.it "reports a terminal source comment as single-line spacing" $
    isMultilineSpacing (spacing $ lineFragment 1) `Hspec.shouldBe` False

  Hspec.it "counts only the fragment on the first physical line" $
    case spacing $ sequenceNode 0
      [lineFragment 1, literal 2 $ replicate 50 'x'] of
      LineModeValid measured -> _vs_sameLine measured `Hspec.shouldBe` 53
      _ -> Hspec.expectationFailure "valid boundary sequence rejected"

  Hspec.it "preserves an inline block fragment's physical width" $ do
    case spacing $ fragment 1 "{- block -}" of
      LineModeValid measured -> do
        _vs_sameLine measured `Hspec.shouldBe` 11
        _vs_paragraph measured `Hspec.shouldBe` VerticalSpacingParNone
      _ -> Hspec.expectationFailure "valid block fragment rejected"

afterFragment :: BriDocNumbered -> BriDocNumbered
afterFragment following = sequenceNode 0
  [literal 1 "= ", lineFragment 2, (3, BDFSeparator), following]

sequenceNode :: Int -> [BriDocNumbered] -> BriDocNumbered
sequenceNode nodeId children = (nodeId, BDFSeq children)

alternative :: Int -> BriDocNumbered
alternative width = (100, BDFAlt
  [ (101, BDFDebug "compact" $ literal 102 $ replicate width 'x')
  , (103, BDFDebug "wrapped"
      (104, BDFPar BrIndentNone (literal 105 "first") (literal 106 "second")))
  ])

paragraphChoice :: Int -> BriDocNumbered
paragraphChoice width = (100, BDFAlt
  [ (101, BDFDebug "compact"
      (102, BDFPar BrIndentNone
        (literal 103 "first") (literal 104 $ replicate width 'x')))
  , (105, BDFDebug "wrapped" $ literal 106 "fallback")
  ])

forcedChoice :: BriDocNumbered -> BriDocNumbered
forcedChoice document = (100, BDFAlt
  [ (101, BDFDebug "compact" (102, BDFForceSingleline document))
  , (103, BDFDebug "wrapped" $ literal 104 "fallback")
  ])

literal :: Int -> String -> BriDocNumbered
literal nodeId text = (nodeId, BDFLit $ Text.pack text)

lineFragment :: Int -> BriDocNumbered
lineFragment nodeId = fragment nodeId $ "-- " ++ replicate 50 'x'

fragment :: Int -> String -> BriDocNumbered
fragment nodeId text = (nodeId, BDFExternal ownerKey False $ SourceFragment
  ExactSourceFragment
    { fragmentText = Text.pack text
    , fragmentRange = SourceRange "Fragment.hs" 1 1 1 (length text + 1)
    , fragmentAnnotationKeys = Set.empty
    , fragmentCommentKeys = Set.empty
    , fragmentAbsoluteColumn = Nothing
    , fragmentRebaseContinuation = True
    })

ownerKey :: EP.AnnKey
ownerKey = EP.AnnKey [EP.realSpanToSrcSpan sourceSpan] $ EP.CN "SourceComment"
 where
  location = SrcLoc.mkRealSrcLoc (FastString.mkFastString "Fragment.hs") 1 1
  sourceSpan = SrcLoc.mkRealSrcSpan location location

selectedMarkers :: BriDoc -> [String]
selectedMarkers = Generics.everything (++) $ Generics.mkQ [] marker
 where
  marker :: BriDoc -> [String]
  marker = \case
    BDDebug name _ -> [takeWhile (/= '@') name]
    _ -> []

choose :: AltChooser -> Int -> BriDocNumbered -> BriDoc
choose chooser columns document = fst result
 where
  result :: (BriDoc, Seq String)
  result = runIdentity $ MultiRWSS.runMultiRWSTNil
    $ MultiRWSS.withMultiWriterAW
    $ MultiRWSS.withMultiReader (config chooser columns)
    $ transformAlts document

spacing :: BriDocNumbered -> LineModeValidity VerticalSpacing
spacing document = fst result
 where
  result :: (LineModeValidity VerticalSpacing, Seq String)
  result = runIdentity $ MultiRWSS.runMultiRWSTNil
    $ MultiRWSS.withMultiWriterAW
    $ MultiRWSS.withMultiReader (config AltChooserShallowBest 200)
    $ getSpacing document

isMultilineSpacing :: LineModeValidity VerticalSpacing -> Bool
isMultilineSpacing = \case
  LineModeValid measured -> _vs_paragraph measured /= VerticalSpacingParNone
  _ -> False

config :: AltChooser -> Int -> Config
config chooser columns = staticDefaultConfig
  { _conf_layout = (_conf_layout staticDefaultConfig)
      { _lconfig_altChooser = Identity $ Last chooser
      , _lconfig_cols = Identity $ Last columns
      }
  }
