{-# LANGUAGE LambdaCase #-}

module TupleSemanticNameSpec (spec) where

import qualified Data.List                               as List
import qualified Data.Map                                as Map
import qualified Data.Text                               as Text
import qualified Data.Text.IO                            as TextIO
import qualified Data.Text.Lazy                          as Text.Lazy
import qualified GHC
import qualified GHC.Builtin.Types                       as Builtin
import           GHC.Types.Basic                          ( TupleSort(..) )
import qualified GHC.Types.Name                          as Name
import qualified GHC.Types.Name.Occurrence               as Occurrence
import qualified GHC.Types.Name.Reader                   as Reader
import qualified GHC.Utils.Outputable                    as Outputable
import           Language.Haskell.Brittany                ( parsePrintModule
                                                          , staticDefaultConfig
                                                          )
import qualified Language.Haskell.Brittany.Internal      as Internal
import           Language.Haskell.Brittany.Internal.ExactPrintCompat
                                                          ( Anns )
import qualified Language.Haskell.Brittany.Internal.ParseModule
                                                         as ParseModule
import           Language.Haskell.Brittany.Internal.SemanticFingerprint
                                                          ( SemanticDifference(..)
                                                          , compareSemanticSyntax
                                                          , semanticFingerprint
                                                          )
import           Language.Haskell.Brittany.Internal.Types ( BrittanyError(..)
                                                          , PerItemConfig(..)
                                                          )
import qualified Language.Haskell.Brittany.Main          as Brittany
import qualified System.Directory                        as Directory
import qualified System.Exit                             as Exit
import qualified System.FilePath                         as FilePath
import qualified Test.Hspec                              as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "tuple constructor semantic names" $ do
  Hspec.it "formats a standalone deriving tuple constructor idempotently" $ do
    firstPass <- formatSource minimalInput
    firstPass `Hspec.shouldBe` minimalExpected
    formatSource firstPass `Hspec.shouldReturn` firstPass

  Hspec.it "accepts the formatted production Types module" $ do
    let path = FilePath.combine
          projectRoot
          "source/library/Language/Haskell/Brittany/Internal/Types.hs"
    input                      <- TextIO.readFile path
    (annotations, parsedInput) <- parseSource path input
    let (_, formatted) = Internal.pPrintModuleWithSource (Just input)
                                                         staticDefaultConfig
                                                         emptyPerItemConfig
                                                         annotations
                                                         parsedInput
    (_, parsedOutput) <- parseSource "TypesFormatted.hs"
      $ Text.Lazy.toStrict formatted
    case Internal.semanticErrors parsedInput parsedOutput of
      []                        -> pure ()
      ErrorSemanticChange{} : _ -> Hspec.expectationFailure
        "formatted Types module changed semantic syntax"
      ErrorSemanticProjection{} : _ ->
        Hspec.expectationFailure "formatted Types module could not be projected"
      _ -> Hspec.expectationFailure "unexpected semantic validation error"

  Hspec.it "canonicalizes nested boxed and unboxed tuple names" $ do
    let tupleCases =
          [ (sort, arity)
          | sort  <- [BoxedTuple, UnboxedTuple]
          , arity <- [0, 2, 3, 8]
          ]
    mapM_ assertCanonicalTuple tupleCases
    mapM_ assertCanonicalTuple $ reverse tupleCases
    firstPass <- formatSource edgeInput
    firstPass `Hspec.shouldContain` "(,,)"
    firstPass `Hspec.shouldContain` "(#,#)"
    firstPass `Hspec.shouldNotContain` "Tuple2"
    formatSource firstPass `Hspec.shouldReturn` firstPass

  Hspec.it "still detects tuple arity and namespace changes" $ do
    let
      pairName       = Builtin.tupleTyConName BoxedTuple 2
      tripleName     = Builtin.tupleTyConName BoxedTuple 3
      pairOccurrence = sourceTupleOccurrence pairName
      dataOccurrence =
        Occurrence.setOccNameSpace Occurrence.dataName pairOccurrence
    assertDifferent (Reader.Exact pairName) (Reader.Exact tripleName)
    assertDifferent (Reader.Unqual pairOccurrence)
                    (Reader.Unqual dataOccurrence)

  Hspec.it "leaves malformed tuple syntax byte-identical" $ do
    let fixture = fixturePath projectRoot "TupleSemanticNameInvalid.hs"
        output  = outputPath projectRoot "TupleSemanticNameInvalid.hs"
    expected <- readFile fixture
    Directory.copyFile fixture output
    Brittany.mainWith
        "brittany"
        [ "--config-file"
        , FilePath.combine projectRoot "data/brittany.yaml"
        , "--no-user-config"
        , "--write-mode"
        , "inplace"
        , output
        ]
      `Hspec.shouldThrow` (== Exit.ExitFailure 60)
    readFile output `Hspec.shouldReturn` expected

assertCanonicalTuple :: (TupleSort, Int) -> Hspec.Expectation
assertCanonicalTuple (sort, arity) = do
  let tupleName       = Builtin.tupleTyConName sort arity
      exactName       = Reader.Exact tupleName
      unqualifiedName = Reader.Unqual $ sourceTupleOccurrence tupleName
  compareSemanticSyntax exactName unqualifiedName `Hspec.shouldBe` Right Nothing
  semanticFingerprint exactName
    `Hspec.shouldBe` semanticFingerprint unqualifiedName

assertDifferent :: Reader.RdrName -> Reader.RdrName -> Hspec.Expectation
assertDifferent input output = case compareSemanticSyntax input output of
  Right (Just difference) -> do
    semanticDifferencePath difference `Hspec.shouldNotBe` []
    semanticInputSummary difference
      `Hspec.shouldNotBe` semanticOutputSummary difference
  Left projectionError ->
    Hspec.expectationFailure
      $  "semantic projection failed: "
      ++ show projectionError
  Right Nothing -> Hspec.expectationFailure "semantic mutation was accepted"

sourceTupleOccurrence :: Name.Name -> Occurrence.OccName
sourceTupleOccurrence name =
  Occurrence.mkOccName (Name.nameNameSpace name) (showTupleName name)

showTupleName :: Name.Name -> String
showTupleName = Outputable.showSDocUnsafe . Outputable.ppr

formatSource :: String -> IO String
formatSource input =
  parsePrintModule staticDefaultConfig (Text.pack input) >>= \case
    Left errors ->
      Hspec.expectationFailure
          ("formatting returned " ++ show (length errors) ++ " errors")
        >> fail "formatting failed"
    Right output -> pure $ Text.unpack output

parseSource :: FilePath -> Text.Text -> IO (Anns, GHC.ParsedSource)
parseSource filename input = do
  parsed <- ParseModule.parseModule [] filename (const $ pure $ Right ())
    $ Text.unpack input
  case parsed of
    Left parseError -> Hspec.expectationFailure parseError >> fail parseError
    Right (annotations, parsedSource, ()) -> pure (annotations, parsedSource)

emptyPerItemConfig :: PerItemConfig
emptyPerItemConfig =
  PerItemConfig { _icd_perBinding = Map.empty, _icd_perKey = Map.empty }

fixturePath :: FilePath -> FilePath -> FilePath
fixturePath projectRoot =
  FilePath.combine $ FilePath.combine projectRoot "source/test-suite/fixtures"

outputPath :: FilePath -> FilePath -> FilePath
outputPath projectRoot =
  FilePath.combine $ FilePath.combine projectRoot "output"

minimalInput :: String
minimalInput = unlines
  [ "{-# LANGUAGE StandaloneDeriving #-}"
  , "module TupleSemanticName where"
  , "data BriDocF f = BriDocF"
  , "deriving instance Eq (BriDocF ((,) Int))"
  ]

minimalExpected :: String
minimalExpected = List.intercalate
  "\n"
  [ "{-# LANGUAGE StandaloneDeriving #-}"
  , "module TupleSemanticName where"
  , "data BriDocF f = BriDocF"
  , "deriving instance Eq (BriDocF ((,) Int))"
  ]

edgeInput :: String
edgeInput = unlines
  [ "{-# LANGUAGE UnboxedTuples #-}"
  , "module TupleSemanticNameEdge where"
  , "type Pair = (,) Int Bool"
  , "type Triple = (,,) Int Bool Char"
  , "type Nested = (,) ((,,) Int Bool Char) ((#,#) Int Bool)"
  ]
