module AnnotationIndexSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified GHC
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Language.Haskell.Brittany.Internal.AnnotationIndex as AnnotationIndex
import Language.Haskell.Brittany.Internal.ExactPrintCompat
  ( AnnConName(..)
  , AnnKey(..)
  , Anns
  )
import Language.Haskell.Brittany.Internal.ExtractAnns
  ( buildModuleAnnotationIndex
  , extractAnnsFromModule
  )
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "annotation index" $ do
  Hspec.it "indexes nested nodes and expression overrides in one result" $ do
    parsed <- parseSource "AnnotationIndexExpected.hs" $ unlines
      [ "module AnnotationIndexExpected where"
      , "value flag = let local = 1 in if flag then local else 2"
      ]
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError
      Right (_, parsedModule, ()) -> do
        let annotationIndex = buildModuleAnnotationIndex parsedModule
            spans = AnnotationIndex.indexSpanMap spanStart spanEnd
              annotationIndex
        AnnotationIndex.indexNodeCount annotationIndex
          `Hspec.shouldSatisfy` (> 3)
        AnnotationIndex.indexOverrideCount annotationIndex
          `Hspec.shouldSatisfy` (> 0)
        let constructorNames =
              [ name
              | (AnnKey _ (CN name), _, _) <-
                  AnnotationIndex.indexNodes annotationIndex
              ]
        constructorNames `Hspec.shouldContain` ["HsValBinds"]
        Map.size spans `Hspec.shouldSatisfy`
          (<= AnnotationIndex.indexNodeCount annotationIndex)

  Hspec.it "retains synthetic MatchGroup ownership for case comments" $ do
    parsed <- parseSource "AnnotationIndexMatchGroup.hs" $ unlines
      [ "module AnnotationIndexMatchGroup where"
      , "value flag = case flag of"
      , "  -- before first match"
      , "  True -> 1"
      , "  False -> 0"
      ]
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError
      Right (_, parsedModule, ()) -> do
        let constructorNames =
              [ name
              | AnnKey _ (CN name) <- Map.keys
                  $ extractAnnsFromModule parsedModule
              ]
        constructorNames `Hspec.shouldContain` ["MatchGroup"]

  Hspec.it "returns an empty index for a module without declarations" $ do
    parsed <- parseSource "AnnotationIndexEmpty.hs"
      "module AnnotationIndexEmpty where\n"
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError
      Right (_, parsedModule, ()) -> do
        let annotationIndex = buildModuleAnnotationIndex parsedModule
        AnnotationIndex.indexNodeCount annotationIndex `Hspec.shouldBe` 0
        AnnotationIndex.indexOverrideCount annotationIndex `Hspec.shouldBe` 0

  Hspec.it "does not construct an index after a malformed parse" $ do
    parsed <- parseSource "AnnotationIndexMalformed.hs"
      "module AnnotationIndexMalformed where\nvalue = if then\n"
    case parsed of
      Left{} -> pure ()
      Right{} -> Hspec.expectationFailure "expected malformed input to fail"

parseSource
  :: FilePath
  -> String
  -> IO (Either String (Anns, GHC.ParsedSource, ()))
parseSource filename = ParseModule.parseModule [] filename
  $ const $ pure $ Right ()

spanStart :: SrcLoc.RealSrcSpan -> (Int, Int)
spanStart sourceSpan =
  (SrcLoc.srcSpanStartLine sourceSpan, SrcLoc.srcSpanStartCol sourceSpan)

spanEnd :: SrcLoc.RealSrcSpan -> (Int, Int)
spanEnd sourceSpan =
  (SrcLoc.srcSpanEndLine sourceSpan, SrcLoc.srcSpanEndCol sourceSpan)
