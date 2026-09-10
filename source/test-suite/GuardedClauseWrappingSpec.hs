{-# LANGUAGE LambdaCase #-}

module GuardedClauseWrappingSpec (spec) where

import qualified Control.Exception as Exception
import Control.Monad (forM_)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import Data.Semigroup (Last(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Language.Haskell.Brittany
  ( CConfig(..)
  , CErrorHandlingConfig(..)
  , CLayoutConfig(..)
  , Config
  , parsePrintModule
  , staticDefaultConfig
  )
import Language.Haskell.Brittany.Internal.CommentPlan
  ( commentPlanFingerprint
  , normalizeCommentPlan
  )
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.SemanticFingerprint
  ( compareSemanticSyntax
  )
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified Test.Hspec as Hspec

spec :: FilePath -> Hspec.Spec
spec projectRoot = Hspec.describe "guarded clause wrapping" $ do
  forM_ corpusOccurrences $ \(relativePath, start, end) ->
    Hspec.it ("wraps the guarded clause in the complete " ++ relativePath ++ " module") $ do
      source <- readFile $ projectRoot
        </> "source/library/Language/Haskell/Brittany/Internal" </> relativePath
      let config = configWithLayout 80 2
      output <- formatChecked config source
      clause <- extractClause start end output
      filter ((> 80) . length) clause `Hspec.shouldBe` []
      assertStableAndEquivalent config source output

  Hspec.it "keeps both constructor predicates intact in the complete DataDecl.Support module" $ do
    source <- readFile $ projectRoot
      </> "source/library/Language/Haskell/Brittany/Internal/Layouters/DataDecl/Support.hs"
    let config = configWithLayout 80 2
    output <- formatChecked config source
    let predicateLines = filter (List.isInfixOf "simpleH98 constructor") $ lines output
    length predicateLines `Hspec.shouldBe` 2
    filter ((> 80) . length) predicateLines `Hspec.shouldBe` []
    assertStableAndEquivalent config source output

  Hspec.it "keeps a fitting guarded equation compact" $ do
    output <- checkWithinColumns 80 2 $ moduleSource
      [ "choose value | otherwise = value" ]
    output `Hspec.shouldContain` "| otherwise = value"

  Hspec.it "keeps a fitting guarded case clause compact" $ do
    output <- checkWithinColumns 80 2 $ moduleSource
      [ "choose value = case value of"
      , "  Just selected | otherwise -> selected"
      , "  Nothing -> value"
      ]
    output `Hspec.shouldContain` "| otherwise -> selected"

  forM_ [40, 80] $ \columns -> forM_ [False, True] $ \caseClause ->
    Hspec.it
      ("keeps an exactly fitting " ++ (if caseClause then "case" else "equation")
        ++ " clause attached at width " ++ show columns) $ do
        let prefix = if caseClause
              then "  Just value | otherwise -> "
              else "choose value | otherwise = "
            result = "result" ++ replicate (columns - length prefix - 6) 'x'
            exactLine = prefix ++ result
            declarations = if caseClause
              then ["choose value = case value of", exactLine, "  Nothing -> value"]
              else [exactLine]
        output <- checkWithinColumns columns 2 $ moduleSource declarations
        lines output `Hspec.shouldContain` [exactLine]

  Hspec.it "keeps an unavoidable guarded RHS token intact on its own line" $ do
    let result = "unavoidable" ++ replicate 80 'x'
        source = moduleSource ["choose value | otherwise = " ++ result]
        config = configWithLayout 40 2
    output <- formatChecked config source
    map (dropWhile (== ' ')) (filter ((> 40) . length) $ lines output)
      `Hspec.shouldBe` [result]
    assertStableAndEquivalent config source output

  forM_ [40, 80] $ \columns -> forM_ [2, 4] $ \indent ->
    forM_ [False, True] $ \caseClause -> forM_ [False, True] $ \patternGuard ->
      forM_ [0, 1, 2] $ \remaining ->
        Hspec.it
          ("reserves separator space after a "
            ++ (if patternGuard then "pattern" else "boolean")
            ++ " guard before " ++ (if caseClause then "an arrow" else "equals")
            ++ " at width " ++ show columns ++ " and indent " ++ show indent
            ++ " with " ++ show remaining ++ " columns remaining") $ do
            let source = binderBoundarySource columns indent caseClause patternGuard remaining
            _ <- checkWithinColumns columns indent source
            pure ()

  forM_ [2, 4] $ \indent -> forM_ [40, 80] $ \columns ->
    forM_ focusedClauses $ \(description, declarations) ->
      Hspec.it
        ("wraps " ++ description ++ " at width " ++ show columns
          ++ " and indent " ++ show indent) $ do
          output <- checkWithinColumns columns indent $ moduleSource declarations
          if description == "multiple guards before an arrow" && columns == 40 && indent == 4
            then do
              output `Hspec.shouldContain` "isSelected firstValue"
              output `Hspec.shouldContain` "isReady secondValue"
            else pure ()

  Hspec.it "keeps a fitting predicate application together when breaking the clause" $ do
    output <- checkWithinColumns 40 2 $ moduleSource
      [ "choose selectedValue"
      , "  | acceptsSelectedValue selectedValue = buildSelectedResult selectedValue"
      ]
    output `Hspec.shouldContain` "acceptsSelectedValue selectedValue"

  Hspec.it "keeps a fitting guarded RHS application together on a new line" $ do
    output <- checkWithinColumns 80 2 $ moduleSource
      [ "projectValue path value"
      , "  | Just atomValue <- atomicValue value = pure $ Just $ SemanticAtom typeName atomValue"
      , "  | otherwise = Nothing"
      ]
    output `Hspec.shouldContain` "pure $ Just $ SemanticAtom typeName atomValue"

  Hspec.it "wraps guarded equations in a local let binding" $ do
    _ <- checkWithinColumns 40 2 $ moduleSource
      [ "outer value ="
      , "  let choose selectedValue"
      , "        | acceptsValue selectedValue = buildSelectedResult selectedValue value"
      , "        | otherwise = value"
      , "  in choose value"
      ]
    pure ()

  Hspec.it "wraps guarded case clauses inside a nested where binding" $ do
    _ <- checkWithinColumns 40 4 $ moduleSource
      [ "outer value = nested"
      , "  where"
      , "    nested = case value of"
      , "      Candidate selectedValue"
      , "        | acceptsValue selectedValue, isReady value -> buildResult selectedValue value"
      , "      _ -> value"
      ]
    pure ()

  Hspec.it "keeps a where clause attached to a guarded equation" $ do
    _ <- checkWithinColumns 40 4 $ moduleSource
      [ "choose selectedValue"
      , "  | acceptsValue selectedValue = buildSelectedResult selectedValue result"
      , "  | otherwise = result"
      , "  where"
      , "    result = selectedValue"
      ]
    pure ()

  forM_ commentedClauses $ \(description, declarations) ->
    Hspec.it ("preserves " ++ description ++ " when wrapping guarded clauses") $ do
      _ <- checkWithinColumns 80 2 $ moduleSource declarations
      pure ()

  forM_ malformedClauses $ \(description, declarations) ->
    Hspec.it ("rejects " ++ description ++ " without changing inplace input") $ do
      let source = moduleSource declarations
      directory <- Directory.getTemporaryDirectory
      Exception.bracket
        (do
          (path, handle) <- IO.openTempFile directory "brittany-guarded-clause-invalid.hs"
          IO.hPutStr handle source
          IO.hClose handle
          pure path)
        Directory.removeFile
        $ \path -> do
          Brittany.mainWith "brittany"
            [ "--no-user-config"
            , "--write-mode", "inplace"
            , "--werror"
            , "--fail-on-fallback"
            , path
            ] `Hspec.shouldThrow` (== Exit.ExitFailure 60)
          TextIO.readFile path `Hspec.shouldReturn` Text.pack source

extractClause :: String -> String -> String -> IO [String]
extractClause start end output = case
  [ takeWhile (not . contains end) suffix
  | suffix@(first : _) <- List.tails $ lines output
  , contains start first
  ] of
    [] -> Hspec.expectationFailure ("missing guarded clause: " ++ start)
      >> fail "guarded clause missing"
    matches -> do
      let clause = last matches
      clause `Hspec.shouldSatisfy` any (List.isInfixOf "|")
      pure clause
 where
  contains marker line = compact marker `List.isInfixOf` compact line
  compact = filter (/= ' ')

checkWithinColumns :: Int -> Int -> String -> IO String
checkWithinColumns columns indent source = do
  let config = configWithLayout columns indent
  output <- formatChecked config source
  filter ((> columns) . length) (lines output) `Hspec.shouldBe` []
  assertStableAndEquivalent config source output
  pure output

formatChecked :: Config -> String -> IO String
formatChecked config source = parsePrintModule config (Text.pack source) >>= \case
  Left errors -> Hspec.expectationFailure
    ("formatting returned " ++ show (length errors) ++ " errors")
    >> fail "formatting failed"
  Right output -> pure $ Text.unpack output

assertStableAndEquivalent :: Config -> String -> String -> IO ()
assertStableAndEquivalent config original firstPass = do
  (inputAnns, inputParsed, ()) <- parseSource original
  (outputAnns, outputParsed, ()) <- parseSource firstPass
  compareSemanticSyntax inputParsed outputParsed `Hspec.shouldBe` Right Nothing
  case (normalizeCommentPlan inputAnns, normalizeCommentPlan outputAnns) of
    (Right inputPlan, Right outputPlan) ->
      commentPlanFingerprint outputPlan
        `Hspec.shouldBe` commentPlanFingerprint inputPlan
    _ -> Hspec.expectationFailure "input or output has an invalid comment plan"
  secondPass <- formatChecked config firstPass
  thirdPass <- formatChecked config secondPass
  secondPass `Hspec.shouldBe` firstPass
  thirdPass `Hspec.shouldBe` firstPass
 where
  parseSource source = do
    parsed <- ParseModule.parseModule ["-haddock"] "GuardedClauses.hs"
      (const $ pure $ Right ()) source
    case parsed of
      Left parseError -> Hspec.expectationFailure parseError >> fail parseError
      Right result -> pure result

configWithLayout :: Int -> Int -> Config
configWithLayout columns indent = staticDefaultConfig
  { _conf_layout = (_conf_layout staticDefaultConfig)
      { _lconfig_cols = Identity $ Last columns
      , _lconfig_indentAmount = Identity $ Last indent
      }
  , _conf_errorHandling = (_conf_errorHandling staticDefaultConfig)
      { _econf_Werror = Identity $ Last True
      , _econf_failOnExactSourceFallback = Identity $ Last True
      }
  }

moduleSource :: [String] -> String
moduleSource declarations = unlines $ ["module GuardedClauses where", ""] ++ declarations

binderBoundarySource :: Int -> Int -> Bool -> Bool -> Int -> String
binderBoundarySource columns indent caseClause patternGuard remaining =
  unlines $ ["module BinderBoundary where", ""] ++ declarations
 where
  base = if caseClause then 2 * indent else indent
  prefix = if patternGuard then "| Just x <- " else "| "
  suffix = " value"
  nameLength = columns - remaining - base - length prefix - length suffix
  name = 'p' : replicate (nameLength - 1) 'a'
  guardSource = prefix ++ name ++ suffix
  declarations = if caseClause
    then
      [ "choose value = case value of"
      , "  Candidate value"
      , "    " ++ guardSource ++ " -> True"
      , "  _ -> False"
      ]
    else ["choose (Candidate value)", "  " ++ guardSource ++ " = True"]

corpusOccurrences :: [(FilePath, String, String)]
corpusOccurrences =
  [ ("Delimiter/Types.hs", "validAttachment _ separator", "validAttachment profile separator")
  , ("Backend.hs", "OwnerRelativeIndent", "RenderedAnchorIndent")
  , ("Transformations/Alt.hs", "BDFExternal _ _ source", "BDFExternal{}")
  ]

focusedClauses :: [(String, [String])]
focusedClauses =
  [ ("a boolean guard before equals",
      [ "choose value"
      , "  | classifySelectedValue value == ExpectedValue = buildSelectedResult value value"
      ])
  , ("a pattern guard before equals",
      [ "choose value"
      , "  | [selected] <- extractCandidateValues value = buildSelectedResult selected value"
      ])
  , ("multiple guards before equals",
      [ "choose value"
      , "  | isSelected value, [selected] <- extractCandidateValues value = buildSelectedResult selected value"
      , "  | otherwise = value"
      ])
  , ("a boolean guard before an arrow",
      [ "choose value = case value of"
      , "  Candidate firstValue secondValue"
      , "    | classifySelectedValue firstValue == ExpectedValue -> buildSelectedResult firstValue secondValue"
      , "  _ -> value"
      ])
  , ("a pattern guard before an arrow",
      [ "choose value = case value of"
      , "  Candidate firstValue secondValue"
      , "    | [selected] <- extractCandidateValues firstValue -> buildSelectedResult selected secondValue"
      , "  _ -> value"
      ])
  , ("multiple guards before an arrow",
      [ "choose value = case value of"
      , "  Candidate firstValue secondValue"
      , "    | isSelected firstValue, isReady secondValue -> buildSelectedResult firstValue secondValue"
      , "    | otherwise -> secondValue"
      , "  _ -> value"
      ])
  ]

commentedClauses :: [(String, [String])]
commentedClauses =
  [ ("a line comment between a predicate and equals",
      [ "choose selectedValue"
      , "  | acceptsSelectedValue selectedValue -- predicate note"
      , "  = buildSelectedResult selectedValue selectedValue"
      ])
  , ("a block comment after equals",
      [ "choose selectedValue"
      , "  | acceptsSelectedValue selectedValue = {- result note -} buildSelectedResult selectedValue selectedValue"
      ])
  , ("a block comment between guards",
      [ "choose selectedValue"
      , "  | acceptsSelectedValue selectedValue {- first guard -}"
      , "  , isReady selectedValue = buildSelectedResult selectedValue selectedValue"
      ])
  , ("a line comment between guards",
      [ "choose selectedValue"
      , "  | acceptsSelectedValue selectedValue -- first guard"
      , "  , isReady selectedValue = buildSelectedResult selectedValue selectedValue"
      ])
  , ("a line comment before a case arrow",
      [ "choose value = case value of"
      , "  Candidate selectedValue"
      , "    | acceptsSelectedValue selectedValue -- predicate note"
      , "    -> buildSelectedResult selectedValue value"
      , "  _ -> value"
      ])
  ]

malformedClauses :: [(String, [String])]
malformedClauses =
  [ ("a guarded equation missing its RHS", ["choose value | otherwise ="])
  , ("a guarded case clause missing its RHS",
      [ "choose value = case value of"
      , "  Just selected | otherwise ->"
      ])
  ]
