{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Language.Haskell.Brittany.Internal.SemanticFingerprint.Ghc
  ( projectSemanticSyntax
  ) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Short as ShortByteString
import qualified Data.Data as Data
import Data.Kind (Type)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import GHC (GenLocated(L), HsModule(..), unLoc)
import qualified GHC.Data.FastString as FastString
import GHC.Hs (XModulePs(..))
import GHC.Hs.Decls (DerivClauseTys(..), HsDerivingClause(..))
import GHC.Hs.ImpExp (XImportDeclPass(..))
import qualified GHC.Types.Name as Name
import qualified GHC.Types.Name.Occurrence as Occurrence
import qualified GHC.Types.Name.Reader as Reader
import GHC.Types.PkgQual (RawPkgQual(..))
import GHC.Types.SourceText (StringLiteral(..))
import GHC.Unit.Types (IsBootInterface(..))
import qualified GHC.Utils.Outputable as Outputable
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.SemanticModel
import Language.Haskell.Syntax.ImpExp
  ( IE(..)
  , IEWildcard(..)
  , IEWrappedName(..)
  , ImportDecl(..)
  , ImportDeclLevel(..)
  , ImportDeclLevelStyle(..)
  , ImportDeclQualifiedStyle(..)
  , ImportListInterpretation(..)
  )
import qualified Language.Haskell.Syntax.Module.Name as ModuleName

projectSemanticSyntax
  :: Data.Data ast
  => ast
  -> Either SemanticProjectionError SemanticModel
projectSemanticSyntax value = do
  projected <- projectValue [] value
  case projected of
    Just model -> pure model
    Nothing -> projectionFailure ["root"] "ignored root syntax"

projectValue
  :: forall ast
   . Data.Data ast
  => [String]
  -> ast
  -> Either SemanticProjectionError (Maybe SemanticModel)
projectValue path value
  | Just parsedModule <- Data.cast value =
      Just <$> projectModule path parsedModule
  | Just derivingClause <- Data.cast value =
      Just <$> projectDerivingClause path derivingClause
  | Just readerName <- Data.cast value =
      projectReaderName path readerName
  | ignoredRepresentation typeRepresentation = pure Nothing
  | Just atomValue <- atomicValue value =
      pure $ Just $ SemanticAtom typeName atomValue
  | otherwise = projectGeneric path value
 where
  typeRepresentation = Typeable.typeOf value
  typeConstructor = Typeable.typeRepTyCon typeRepresentation
  typeName = Typeable.tyConName typeConstructor

projectReaderName
  :: [String]
  -> Reader.RdrName
  -> Either SemanticProjectionError (Maybe SemanticModel)
projectReaderName path = \case
  Reader.Exact name
    | Name.isTupleTyConName name ->
        projectGeneric path $ Reader.Unqual $ Occurrence.mkOccName
          (Name.nameNameSpace name)
          (Outputable.showSDocUnsafe $ Outputable.ppr name)
  readerName -> projectGeneric path readerName

projectModule
  :: [String]
  -> HsModule GhcPs
  -> Either SemanticProjectionError SemanticModel
projectModule path = \case
  HsModule
    { hsmodExt = XModulePs{ hsmodDeprecMessage = deprecation }
    , hsmodName = moduleName
    , hsmodExports = exports
    , hsmodImports = imports
    , hsmodDecls = declarations
    } -> do
      nameModel <- projectMaybe ("name" : path) moduleName
      deprecationModel <- projectMaybe ("deprecation" : path) deprecation
      exportsModel <- projectMaybe ("exports" : path) exports
      importModels <- traverse
        (projectImportDecl ("imports" : path) . unLoc) imports
      declarationModels <- projectSequence
        ("declarations" : path) declarations
      pure $ SemanticNode "module"
        [ SemanticField "name" nameModel
        , SemanticField "deprecation" deprecationModel
        , SemanticField "exports" exportsModel
        , SemanticField "imports" $ multiset "imports" importModels
        , SemanticField "declarations" declarationModels
        ]

projectImportDecl
  :: [String]
  -> ImportDecl GhcPs
  -> Either SemanticProjectionError SemanticModel
projectImportDecl path = \case
  ImportDecl
    { ideclExt = XImportDeclPass{ ideclImplicit = implicitImport }
    , ideclName = importedModule
    , ideclPkgQual = packageQualifier
    , ideclSource = sourceImport
    , ideclLevelSpec = levelStyle
    , ideclSafe = safeImport
    , ideclQualified = qualifiedStyle
    , ideclAs = importAlias
    , ideclImportList = importList
    } -> do
      moduleModel <- projectRequired ("module" : path) importedModule
      aliasModel <- projectMaybe ("alias" : path) importAlias
      importListModel <- projectImportList ("list" : path) importList
      pure $ SemanticNode "import"
        [ SemanticField "module" moduleModel
        , SemanticField "package" $ projectPackage packageQualifier
        , SemanticField "source" $ projectSource sourceImport
        , SemanticField "level" $ projectLevel levelStyle
        , SemanticField "safe" $ semanticBool safeImport
        , SemanticField "qualification" $ projectQualification qualifiedStyle
        , SemanticField "alias" aliasModel
        , SemanticField "implicit" $ semanticBool implicitImport
        , SemanticField "list" importListModel
        ]

projectImportList
  :: [String]
  -> Maybe
    ( ImportListInterpretation
    , GenLocated listLocation [GenLocated itemLocation (IE GhcPs)]
    )
  -> Either SemanticProjectionError SemanticModel
projectImportList path = \case
  Nothing -> pure $ SemanticNode "import-list.none" []
  Just (interpretation, L _ items) -> do
    itemModels <- traverse (projectImportItem path . unLoc) items
    pure $ SemanticNode "import-list"
      [ SemanticField "mode" $ SemanticAtom "import-list-mode" $ case interpretation of
          Exactly -> "explicit"
          EverythingBut -> "hiding"
      , SemanticField "items" $ multiset "import-items" itemModels
      ]

projectImportItem
  :: [String]
  -> IE GhcPs
  -> Either SemanticProjectionError SemanticModel
projectImportItem path = \case
  IEVar warning name _ -> importItem "value" warning name
  IEThingAbs warning name _ -> importItem "abstract" warning name
  IEThingAll (warning, _) name _ -> importItem "all" warning name
  IEThingWith (warning, _) name wildcard children _ -> do
    nameModel <- projectWrappedName ("name" : path) $ unLoc name
    warningModel <- projectMaybe ("warning" : path) warning
    childModels <- traverse
      (projectWrappedName ("children" : path) . unLoc) children
    pure $ SemanticNode "import-item.with"
      [ SemanticField "warning" warningModel
      , SemanticField "name" nameModel
      , SemanticField "wildcard" $ projectWildcard wildcard
      , SemanticField "children" $ multiset "import-children" childModels
      ]
  IEModuleContents (warning, _) moduleName -> do
    warningModel <- projectMaybe ("warning" : path) warning
    moduleModel <- projectRequired ("module" : path) moduleName
    pure $ SemanticNode "import-item.module"
      [ SemanticField "warning" warningModel
      , SemanticField "module" moduleModel
      ]
  IEGroup{} -> projectionFailure path "IE.IEGroup in import list"
  IEDoc{} -> projectionFailure path "IE.IEDoc in import list"
  IEDocNamed{} -> projectionFailure path "IE.IEDocNamed in import list"
 where
  importItem itemKind warning name = do
    warningModel <- projectMaybe ("warning" : path) warning
    nameModel <- projectWrappedName ("name" : path) $ unLoc name
    pure $ SemanticNode ("import-item." ++ itemKind)
      [ SemanticField "warning" warningModel
      , SemanticField "name" nameModel
      ]

projectWrappedName
  :: [String]
  -> IEWrappedName GhcPs
  -> Either SemanticProjectionError SemanticModel
projectWrappedName path wrappedName = case wrappedName of
  IEName _ name -> named "default" name
  IEDefault _ name -> named "default-keyword" name
  IEPattern _ name -> named "pattern" name
  IEType _ name -> named "type" name
  IEData _ name -> named "data" name
 where
  named namespace name = do
    nameModel <- projectRequired ("identifier" : path) name
    pure $ SemanticNode "import-name"
      [ SemanticField "namespace" $ SemanticAtom "namespace" namespace
      , SemanticField "identifier" nameModel
      ]

projectDerivingClause
  :: [String]
  -> HsDerivingClause GhcPs
  -> Either SemanticProjectionError SemanticModel
projectDerivingClause path = \case
  HsDerivingClause
    { deriv_clause_strategy = strategy
    , deriv_clause_tys = clauseTypes
    } -> do
      strategyModel <- projectMaybe ("strategy" : path) strategy
      classModels <- case unLoc clauseTypes of
        DctSingle _ classType ->
          (: []) <$> projectRequired ("classes" : path) classType
        DctMulti _ classTypes -> traverse
          (projectRequired ("classes" : path)) classTypes
      pure $ SemanticNode "deriving-clause"
        [ SemanticField "strategy" strategyModel
        , SemanticField "classes" $ sequenceModel "deriving-classes" classModels
        ]

projectGeneric
  :: forall ast
   . Data.Data ast
  => [String]
  -> ast
  -> Either SemanticProjectionError (Maybe SemanticModel)
projectGeneric path value = case Data.dataTypeRep $ Data.dataTypeOf value of
  Data.NoRep -> projectionFailure path qualifiedTypeName
  _ -> do
    let constructor = Data.toConstr value
        fieldNames = Data.constrFields constructor
        childNames = fieldNames ++ repeat "argument"
    children <- sequence $ zipWith projectChild childNames $ Data.gmapQ Box value
    let semanticFields = catMaybes children
        label = typeName ++ "." ++ Data.showConstr constructor
    pure $ normalizeNode typeName label semanticFields
 where
  typeRepresentation = Typeable.typeOf value
  typeConstructor = Typeable.typeRepTyCon typeRepresentation
  typeModule = Typeable.tyConModule typeConstructor
  typeName = Typeable.tyConName typeConstructor
  qualifiedTypeName = typeModule ++ "." ++ typeName

  projectChild fieldName (Box child) = do
    projected <- projectValue (fieldName : path) child
    pure $ SemanticField fieldName <$> projected

projectRequired
  :: Data.Data value
  => [String]
  -> value
  -> Either SemanticProjectionError SemanticModel
projectRequired path value = projectValue path value >>= \case
  Just model -> pure model
  Nothing -> projectionFailure path "ignored semantic field"

projectMaybe
  :: Data.Data value
  => [String]
  -> Maybe value
  -> Either SemanticProjectionError SemanticModel
projectMaybe path = \case
  Nothing -> pure $ SemanticNode "optional.none" []
  Just value -> SemanticNode "optional.some" . (: []) . SemanticField "value"
    <$> projectRequired path value

projectSequence
  :: Data.Data value
  => [String]
  -> [value]
  -> Either SemanticProjectionError SemanticModel
projectSequence path values =
  sequenceModel "sequence" <$> traverse (projectRequired path) values

sequenceModel :: String -> [SemanticModel] -> SemanticModel
sequenceModel label = SemanticNode label . fmap (SemanticField "item")

multiset :: String -> [SemanticModel] -> SemanticModel
multiset label = sequenceModel label . List.sort

projectPackage :: RawPkgQual -> SemanticModel
projectPackage = \case
  NoRawPkgQual -> SemanticNode "package.none" []
  RawPkgQual packageName ->
    SemanticAtom "package" $ FastString.unpackFS $ sl_fs packageName

projectSource :: IsBootInterface -> SemanticModel
projectSource = SemanticAtom "source-import" . \case
  IsBoot -> "source"
  NotBoot -> "normal"

projectLevel :: ImportDeclLevelStyle -> SemanticModel
projectLevel = SemanticAtom "import-level" . \case
  NotLevelled -> "none"
  LevelStylePre ImportDeclQuote -> "quote-pre"
  LevelStylePre ImportDeclSplice -> "splice-pre"
  LevelStylePost ImportDeclQuote -> "quote-post"
  LevelStylePost ImportDeclSplice -> "splice-post"

projectQualification :: ImportDeclQualifiedStyle -> SemanticModel
projectQualification = SemanticAtom "qualification" . \case
  NotQualified -> "none"
  QualifiedPre -> "pre"
  QualifiedPost -> "post"

projectWildcard :: IEWildcard -> SemanticModel
projectWildcard = SemanticAtom "wildcard" . \case
  NoIEWildcard -> "none"
  IEWildcard position -> "position " ++ show position

semanticBool :: Bool -> SemanticModel
semanticBool value = SemanticAtom "boolean" $ if value then "true" else "false"

type Box :: Type
data Box = forall value. Data.Data value => Box value

normalizeNode
  :: String
  -> String
  -> [SemanticField]
  -> Maybe SemanticModel
normalizeNode typeName label fields
  | typeName == "GenLocated" = singleField fields
  | constructorName label `elem` redundantParentheses = payloadField fields
  | otherwise = Just $ SemanticNode label fields
 where
  redundantParentheses = ["HsPar", "HsParTy", "ParPat"]
  constructorName = reverse . takeWhile (/= '.') . reverse

  singleField = \case
    [SemanticField _ value] -> Just value
    _ -> Just $ SemanticNode label fields

  payloadField = \case
    [] -> Just $ SemanticNode label fields
    SemanticField _ firstValue : semanticFields -> Just
      $ foldl (\_ (SemanticField _ nextValue) -> nextValue)
        firstValue semanticFields

ignoredRepresentation :: Typeable.TypeRep -> Bool
ignoredRepresentation representation =
  ignoredType typeModule typeName
    || isDocumentationWrapper typeName typeArguments
    || (not $ null typeArguments)
      && all ignoredRepresentation typeArguments
 where
  (typeConstructor, typeArguments) = Typeable.splitTyConApp representation
  typeModule = Typeable.tyConModule typeConstructor
  typeName = Typeable.tyConName typeConstructor

  isDocumentationWrapper wrapperName arguments =
    wrapperName == "WithHsDocIdentifiers" && any isHsDocString arguments

  isHsDocString argument =
    Typeable.tyConName (Typeable.typeRepTyCon argument) == "HsDocString"

ignoredType :: String -> String -> Bool
ignoredType typeModule typeName = or
  [ typeName /= "GenLocated"
      && "GHC.Types.SrcLoc" `isPrefixOf` typeModule
  , typeName /= "GenLocated"
      && "GHC.Parser.Annotation" `isPrefixOf` typeModule
  , typeModule == "GHC.Types.SourceText" && typeName == "SourceText"
  , typeName `elem`
      [ "HsDocString"
      , "HsDocStringChunk"
      ]
  ]

atomicValue :: forall value. Data.Data value => value -> Maybe String
atomicValue value = asum
  [ show <$> (Data.cast value :: Maybe Int)
  , show <$> (Data.cast value :: Maybe Integer)
  , show <$> (Data.cast value :: Maybe Word)
  , show <$> (Data.cast value :: Maybe Float)
  , show <$> (Data.cast value :: Maybe Double)
  , show <$> (Data.cast value :: Maybe Char)
  , Text.unpack <$> (Data.cast value :: Maybe Text.Text)
  , FastString.unpackFS <$> (Data.cast value :: Maybe FastString.FastString)
  , ModuleName.moduleNameString
      <$> (Data.cast value :: Maybe ModuleName.ModuleName)
  , occurrenceValue <$> (Data.cast value :: Maybe Occurrence.OccName)
  , Name.nameStableString <$> (Data.cast value :: Maybe Name.Name)
  , show <$> (Data.cast value :: Maybe ByteString.ByteString)
  , show <$> (Data.cast value :: Maybe ShortByteString.ShortByteString)
  ]

occurrenceValue :: Occurrence.OccName -> String
occurrenceValue occurrence = namespace ++ ":" ++ Occurrence.occNameString occurrence
 where
  nameSpace = Occurrence.occNameSpace occurrence
  namespace
    | Occurrence.isFieldNameSpace nameSpace = "field"
    | Occurrence.isDataConNameSpace nameSpace = "data-constructor"
    | Occurrence.isTvNameSpace nameSpace = "type-variable"
    | Occurrence.isTcClsNameSpace nameSpace = "type-constructor-or-class"
    | Occurrence.isVarNameSpace nameSpace = "variable"
    | otherwise = "unknown"

projectionFailure
  :: [String]
  -> String
  -> Either SemanticProjectionError value
projectionFailure path typeName = Left SemanticProjectionError
  { projectionErrorPath = reverse path
  , projectionErrorType = typeName
  }
