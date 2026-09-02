module DeclarationSpanIndexSpec (spec) where

import qualified Data.Map as Map
import GHC (GenLocated(L), unLoc)
import qualified GHC.Data.FastString as FastString
import GHC.Hs (HsModule(..))
import GHC.Parser.Annotation (getLocA)
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Language.Haskell.Brittany.Internal.ExactPrintCompat as EP
import Language.Haskell.Brittany.Internal.ExactPrintUtils
  ( declarationMapBySpan
  , extractToplevelAnns
  , quadraticDeclarationMapBySpan
  )
import Language.Haskell.Brittany.Internal.ParseModule (parseModule)
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "declaration span index" $ do
  Hspec.it "maps contained annotations and leaves source gaps unmatched" $ do
    let firstDeclaration = keyAt "FirstDeclaration" 2 1 4 20
        secondDeclaration = keyAt "SecondDeclaration" 6 1 8 20
        firstAnnotation = keyAt "FirstAnnotation" 3 3 3 9
        secondAnnotation = keyAt "SecondAnnotation" 7 3 7 9
        gapAnnotation = keyAt "GapAnnotation" 5 1 5 5
        result = declarationMapBySpan
          [declarationEntry firstDeclaration, declarationEntry secondDeclaration]
          [firstAnnotation, gapAnnotation, secondAnnotation]
    Map.lookup firstAnnotation result `Hspec.shouldBe` Just firstDeclaration
    Map.lookup secondAnnotation result `Hspec.shouldBe` Just secondDeclaration
    Map.lookup gapAnnotation result `Hspec.shouldBe` Nothing

  Hspec.it "is independent of declaration and annotation input order" $ do
    let firstDeclaration = keyAt "FirstDeclaration" 2 1 4 20
        secondDeclaration = keyAt "SecondDeclaration" 6 1 8 20
        annotations =
          [keyAt "SecondAnnotation" 7 3 7 9, keyAt "FirstAnnotation" 3 3 3 9]
        declarations =
          [declarationEntry secondDeclaration, declarationEntry firstDeclaration]
    declarationMapBySpan declarations annotations `Hspec.shouldBe`
      declarationMapBySpan (reverse declarations) (reverse annotations)

  Hspec.it "retains inclusive source-boundary containment" $ do
    let declaration = keyAt "Declaration" 2 1 4 20
        atStart = keyAt "AtStart" 2 1 2 1
        atEnd = keyAt "AtEnd" 4 20 4 20
        result = declarationMapBySpan [declarationEntry declaration]
          [atStart, atEnd]
    Map.lookup atStart result `Hspec.shouldBe` Just declaration
    Map.lookup atEnd result `Hspec.shouldBe` Just declaration

  Hspec.it "uses the compatibility fallback for overlapping declarations" $ do
    let outer = keyAt "Outer" 2 1 8 20
        inner = keyAt "Inner" 4 1 6 20
        annotation = keyAt "Annotation" 5 3 5 9
        declarations = [declarationEntry outer, declarationEntry inner]
        result = declarationMapBySpan declarations [annotation]
    Map.lookup annotation result `Hspec.shouldBe` Just inner
    result `Hspec.shouldBe`
      quadraticDeclarationMapBySpan declarations [annotation]

  Hspec.it "uses the compatibility fallback when declaration endpoints touch" $ do
    let first = keyAt "First" 2 1 4 1
        second = keyAt "Second" 4 1 6 1
        annotation = keyAt "Boundary" 4 1 4 1
        declarations = [declarationEntry first, declarationEntry second]
    Map.lookup annotation (declarationMapBySpan declarations [annotation])
      `Hspec.shouldBe` Just second

  Hspec.it "handles multi-span and unhelpful keys without inventing ownership" $ do
    let declaration = keyAt "Declaration" 2 1 8 20
        inside = sourceSpan 4 1 4 8
        generated = SrcLoc.UnhelpfulSpan SrcLoc.UnhelpfulGenerated
        multiRealFirst = EP.AnnKey [inside, generated] (EP.CN "RealFirst")
        multiGeneratedFirst = EP.AnnKey [generated, inside] (EP.CN "GeneratedFirst")
        descriptive = EP.AnnKey
          [SrcLoc.UnhelpfulSpan
            $ SrcLoc.UnhelpfulOther $ FastString.mkFastString "fixture"]
          (EP.CN "Descriptive")
        result = declarationMapBySpan [declarationEntry declaration]
          [multiRealFirst, multiGeneratedFirst, descriptive]
    Map.lookup multiRealFirst result `Hspec.shouldBe` Just declaration
    Map.lookup multiGeneratedFirst result `Hspec.shouldBe` Nothing
    Map.lookup descriptive result `Hspec.shouldBe` Nothing

  Hspec.it "matches the quadratic reference for ordinary declarations" $ do
    let declarations =
          [ declarationEntry $ keyAt "First" 2 1 4 20
          , declarationEntry $ keyAt "Second" 6 1 8 20
          , declarationEntry $ keyAt "Third" 10 1 12 20
          ]
        annotations =
          [ keyAt "Before" 1 1 1 5
          , keyAt "FirstStart" 2 1 2 1
          , keyAt "FirstBody" 3 5 3 9
          , keyAt "Gap" 5 1 5 5
          , keyAt "SecondBody" 7 5 7 9
          , keyAt "ThirdEnd" 12 20 12 20
          , keyAt "After" 13 1 13 5
          ]
    declarationMapBySpan declarations annotations `Hspec.shouldBe`
      quadraticDeclarationMapBySpan declarations annotations

  Hspec.it "matches the quadratic reference for parsed annotations" $ do
    parsed <- parseModule [] "DeclarationSpanParsed.hs"
      (const $ pure $ Right ()) parsedSource
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError
      Right (annotations, L _ parsedModule, ()) -> do
        let declarations =
              [ (EP.mkAnnKey $ L sourceLocation $ unLoc declaration, sourceLocation)
              | declaration <- hsmodDecls parsedModule
              , let sourceLocation = getLocA declaration
              ]
            annotationKeys = Map.keys annotations
            indexed = declarationMapBySpan declarations annotationKeys
        Map.size indexed `Hspec.shouldSatisfy` (> 0)
        indexed `Hspec.shouldBe`
          quadraticDeclarationMapBySpan declarations annotationKeys

  Hspec.it "keeps unsupported spans in the module group without duplication" $ do
    parsed <- parseModule [] "DeclarationSpanParsed.hs"
      (const $ pure $ Right ()) parsedSource
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError
      Right (annotations, parsedModule, ()) ->
        case Map.lookupMin annotations of
          Nothing -> Hspec.expectationFailure "expected parsed annotations"
          Just (_, sampleAnnotation) -> do
            let unsupported = EP.AnnKey
                  [SrcLoc.UnhelpfulSpan SrcLoc.UnhelpfulGenerated]
                  (EP.CN "Unsupported")
                augmented = Map.insert unsupported sampleAnnotation annotations
                grouped = extractToplevelAnns parsedModule augmented
                groupedAnnotations = Map.elems grouped
                moduleKey = EP.mkAnnKey
                  $ L (getLocA parsedModule) (unLoc parsedModule)
            sum (Map.size <$> groupedAnnotations)
              `Hspec.shouldBe` Map.size augmented
            Map.unions groupedAnnotations `Hspec.shouldBe` augmented
            (Map.lookup moduleKey grouped >>= Map.lookup unsupported)
              `Hspec.shouldBe` Just sampleAnnotation

declarationEntry :: EP.AnnKey -> (EP.AnnKey, SrcLoc.SrcSpan)
declarationEntry annotationKey@(EP.AnnKey (sourceSpanValue : _) _) =
  (annotationKey, sourceSpanValue)
declarationEntry annotationKey = (annotationKey, SrcLoc.noSrcSpan)

keyAt :: String -> Int -> Int -> Int -> Int -> EP.AnnKey
keyAt constructorName startLine startColumn endLine endColumn = EP.AnnKey
  [sourceSpan startLine startColumn endLine endColumn]
  (EP.CN constructorName)

sourceSpan :: Int -> Int -> Int -> Int -> SrcLoc.SrcSpan
sourceSpan startLine startColumn endLine endColumn =
  EP.realSpanToSrcSpan $ SrcLoc.mkRealSrcSpan
    (SrcLoc.mkRealSrcLoc fixtureFile startLine startColumn)
    (SrcLoc.mkRealSrcLoc fixtureFile endLine endColumn)

fixtureFile :: FastString.FastString
fixtureFile = FastString.mkFastString "DeclarationSpanIndexSpec.hs"

parsedSource :: String
parsedSource = unlines
  $ "module DeclarationSpanParsed where"
  : "import Data.List (sort)"
  : ""
  : "-- Before the first declaration."
  : [ "value" ++ show index ++ " = " ++ show index
    | index <- [1 :: Int .. 25]
    ]
  ++ ["-- After the last declaration."]
