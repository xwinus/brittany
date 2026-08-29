{-# LANGUAGE LambdaCase #-}

module CommentOwnershipSpec (spec) where

import qualified Data.Map                                as Map
import qualified Data.Text                               as Text
import qualified Data.Text.Lazy                          as TextLazy
import qualified GHC.Data.FastString                     as FastString
import qualified GHC.Types.SrcLoc                        as SrcLoc
import           Language.Haskell.Brittany                ( staticDefaultConfig
                                                          )
import qualified Language.Haskell.Brittany.Internal      as Internal
import           Language.Haskell.Brittany.Internal.CommentPlan
                                                          ( normalizeCommentPlan
                                                          )
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat
                                                         as EP
import qualified Language.Haskell.Brittany.Internal.ParseModule
                                                         as ParseModule
import           Language.Haskell.Brittany.Internal.SourceComment.Types
                                                          ( CommentPlanError(..)
                                                          )
import           Language.Haskell.Brittany.Internal.Types ( PerItemConfig(..) )
import qualified Test.Hspec                              as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "self-hosted comment ownership" $ do
  Hspec.it "preserves same-line H98 constructor comments" $ do
    firstPass <- formatChecked "ConstructorComments.hs" constructorSource
    firstPass `Hspec.shouldContain` "-- first constructor"
    firstPass `Hspec.shouldContain` "-- second constructor"
    formatChecked "ConstructorComments.hs" firstPass
      `Hspec.shouldReturn` firstPass

  Hspec.it "keeps disabled alternatives and nested comments stable" $ do
    firstPass <- formatChecked "DisabledAlternatives.hs" edgeSource
    commentLines firstPass `Hspec.shouldBe` commentLines edgeSource
    formatChecked "DisabledAlternatives.hs" firstPass
      `Hspec.shouldReturn` firstPass

  Hspec.it "rejects genuine duplicate ownership before rendering" $ do
    let comment     = sourceCommentAt 3 "-- shared"
        annotations = Map.fromList
          [ (nodeKeyAt "First" 2 , annotationWithPrior comment)
          , (nodeKeyAt "Second" 4, annotationWithPrior comment)
          ]
    normalizeCommentPlan annotations `Hspec.shouldSatisfy` \case
      Left [AmbiguousCommentOwnership _ owners] -> length owners == 2
      _ -> False

formatChecked :: FilePath -> String -> IO String
formatChecked filename source = do
  parsed <- ParseModule.parseModule [] filename (const $ pure $ Right ()) source
  case parsed of
    Left parseError -> Hspec.expectationFailure parseError >> fail parseError
    Right (annotations, parsedSource, ()) -> do
      (errors, output) <- Internal.pPrintModuleAndCheckWithSource
        (Just $ Text.pack source)
        staticDefaultConfig
        emptyPerItemConfig
        annotations
        parsedSource
      case errors of
        [] -> pure $ Text.unpack $ TextLazy.toStrict output
        _ ->
          Hspec.expectationFailure
              ("formatting returned " ++ show (length errors) ++ " errors")
            >> fail "formatting failed"

emptyPerItemConfig :: PerItemConfig
emptyPerItemConfig =
  PerItemConfig { _icd_perBinding = Map.empty, _icd_perKey = Map.empty }

annotationWithPrior :: EP.Comment -> EP.Annotation
annotationWithPrior comment =
  EP.Ann
    { EP.annCapturedSpan      = Nothing
    , EP.annSortKey           = Nothing
    , EP.annsDP               = []
    , EP.annFollowingComments = []
    , EP.annPriorComments     = [(comment, EP.DP (1, 0))]
    , EP.annEntryDelta        = EP.DP (0, 0)
    }

sourceCommentAt :: Int -> String -> EP.Comment
sourceCommentAt line contents =
  EP.Comment Nothing (sourceSpan line 1 line $ length contents + 1) contents

nodeKeyAt :: String -> Int -> EP.AnnKey
nodeKeyAt constructorName line =
  EP.AnnKey [sourceSpan line 1 line 8] (EP.CN constructorName)

sourceSpan :: Int -> Int -> Int -> Int -> SrcLoc.SrcSpan
sourceSpan startLine startColumn endLine endColumn =
  EP.realSpanToSrcSpan $ SrcLoc.mkRealSrcSpan
    (SrcLoc.mkRealSrcLoc file startLine startColumn)
    (SrcLoc.mkRealSrcLoc file endLine endColumn)
  where file = FastString.mkFastString "AmbiguousOwnership.hs"

commentLines :: String -> [String]
commentLines =
  fmap (Text.unpack . Text.strip)
    . filter (Text.isPrefixOf (Text.pack "--") . Text.stripStart)
    . Text.lines
    . Text.pack

constructorSource :: String
constructorSource = unlines
  [ "module ConstructorComments where"
  , ""
  , "data Choice"
  , "  = First  -- first constructor"
  , "  | Second -- second constructor"
  ]

edgeSource :: String
edgeSource = unlines
  [ "{-# LANGUAGE LambdaCase #-}"
  , "module DisabledAlternatives where"
  , ""
  , "rewrite = \\case"
  , "  -- Disabled -> 0"
  , "  -- DisabledAgain -> 1"
  , "  Active -> 2"
  , ""
  , "nested value ="
  , "  let"
  , "    -- first local note"
  , "    -- second local note"
  , "    result = value"
  , "  in result"
  ]
