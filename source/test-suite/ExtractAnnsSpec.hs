module ExtractAnnsSpec (spec) where

import qualified Data.List as List
import qualified Data.Map as Map
import GHC (GenLocated(L), unLoc)
import GHC.Hs (GhcPs, HsModule(..), LHsDecl)
import GHC.Parser.Annotation (getLocA)
import Language.Haskell.Brittany.Internal.ExactPrintCompat
import Language.Haskell.Brittany.Internal.ParseModule (parseModule)
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "declaration annotation extraction" $
  Hspec.it "assigns a final post-doc only to the preceding signature" $ do
    parsed <- parseModule [] "FinalResultHaddockAnnotation.hs"
      (const $ pure $ Right ()) source
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
 where
  source = unlines
    [ "module FinalResultHaddockAnnotation where"
    , ""
    , "convert"
    , "    :: Int"
    , "    -> Bool"
    , "    -- ^ conversion result"
    , "-- | Documents convert."
    , "convert = (> 0)"
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
allComments annotations = do
  annotation <- Map.elems annotations
  map commentContents
    ( map fst (annPriorComments annotation ++ annFollowingComments annotation)
    ++ [comment | (AnnComment comment, _) <- annsDP annotation]
    )
