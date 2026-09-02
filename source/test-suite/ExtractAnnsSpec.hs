module ExtractAnnsSpec (spec) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import GHC (GenLocated(L), unLoc)
import GHC.Hs (GhcPs, HsModule(..), LHsDecl)
import GHC.Parser.Annotation (getLocA)
import Language.Haskell.Brittany.Internal.ExactPrintCompat
import Language.Haskell.Brittany.Internal.CommentPlan (normalizeCommentPlan)
import Language.Haskell.Brittany.Internal.ParseModule (parseModule)
import Language.Haskell.Brittany.Internal.SourceComment.Types
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "annotation extraction" $ do
  Hspec.it "assigns a final post-doc only to the preceding signature" $ do
    parsed <- parseModule [] "FinalResultHaddockAnnotation.hs"
      (const $ pure $ Right ()) finalResultSource
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError
      Right (annotations, L _ parsedModule, ()) ->
        case hsmodDecls parsedModule of
          signature : valueDeclaration : _ -> do
            let signatureComments = commentsFor annFollowingComments
                  signature annotations
                valueComments = commentsFor annPriorComments
                  valueDeclaration annotations
            signatureComments `Hspec.shouldBe`
              [("-- ^ conversion result", DP (1, 4))]
            valueComments `Hspec.shouldBe`
              [("-- | Documents convert.", DP (0, 0))]
            allComments annotations
              `Hspec.shouldSatisfy` ((== 1) . length . List.elemIndices
                "-- ^ conversion result")
          declarations -> Hspec.expectationFailure
            $ "expected signature and value declarations, got "
            ++ show (length declarations)
  Hspec.it "classifies export Haddock sections without mutating origins" $ do
    parsed <- parseModule [] "ExportSectionAnnotations.hs"
      (const $ pure $ Right ()) exportSectionSource
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError
      Right (annotations, _, ()) -> do
        let comments = allCommentValues annotations
            sectionComments = List.sort
              $ filter (List.isPrefixOf "-- *")
              $ map commentContents comments
        sectionComments `Hspec.shouldBe`
          ["-- * Public API", "-- ** Nested values"]
        map commentOrigin comments `Hspec.shouldSatisfy` all (== Nothing)
        case normalizeCommentPlan annotations of
          Left planErrors -> Hspec.expectationFailure $ show planErrors
          Right plan -> do
            let plannedSections = List.sort
                  [ Text.unpack $ sourceCommentText sourceComment
                  | (key, sourceComment) <- Map.toList $ commentPlanSources plan
                  , Just placement <- [Map.lookup key $ commentPlanPlacements plan]
                  , placementRole placement == SectionComment
                  ]
            plannedSections `Hspec.shouldBe` sectionComments
  Hspec.it "prefers the enclosing constructor when child spans start together" $ do
    parsed <- parseModule ["-haddock"] "ConstructorOwner.hs"
      (const $ pure $ Right ()) constructorOwnerSource
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError
      Right (annotations, _, ()) -> do
        let owners =
              [ unConName constructorName
              | (AnnKey _ constructorName, annotation) <- Map.toList annotations
              , comment <- allAnnotationComments annotation
              , commentContents comment == "-- | Documents the infix constructor."
              ]
        owners `Hspec.shouldBe` ["ConDeclH98"]
 where
  finalResultSource = unlines
    [ "module FinalResultHaddockAnnotation where"
    , ""
    , "convert"
    , "    :: Int"
    , "    -> Bool"
    , "    -- ^ conversion result"
    , "-- | Documents convert."
    , "convert = (> 0)"
    ]
  exportSectionSource = unlines
    [ "module ExportSectionAnnotations"
    , "    ( -- * Public API"
    , "      first"
    , "      -- ** Nested values"
    , "    , second"
    , "    )"
    , "where"
    , "first = 1"
    , "second = 2"
    ]
  constructorOwnerSource = unlines
    [ "{-# LANGUAGE TypeOperators #-}"
    , "module ConstructorOwner where"
    , "data Mixed = Prefix Int"
    , "  | -- | Documents the infix constructor."
    , "    Int :*: Bool"
    ]

commentsFor
  :: (Annotation -> [(Comment, DeltaPos)])
  -> LHsDecl GhcPs
  -> Anns
  -> [(String, DeltaPos)]
commentsFor select declaration annotations =
  maybe [] (map (\(comment, delta) -> (commentContents comment, delta)) . select)
    $ Map.lookup key annotations
 where
  key = mkAnnKey $ L (getLocA declaration) (unLoc declaration)

allComments :: Anns -> [String]
allComments = map commentContents . allCommentValues

allCommentValues :: Anns -> [Comment]
allCommentValues annotations = do
  annotation <- Map.elems annotations
  allAnnotationComments annotation

allAnnotationComments :: Annotation -> [Comment]
allAnnotationComments annotation =
  map fst (annPriorComments annotation ++ annFollowingComments annotation)
    ++ [comment | (AnnComment comment, _) <- annsDP annotation]
