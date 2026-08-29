{-# LANGUAGE LambdaCase #-}
module CompositionalPatternSelfHosted where

projectImportDecl path = \case
  ImportDecl
    { ideclExt        = XImportDeclPass { ideclImplicit = implicitImport }
    , ideclName       = importedModule
    , ideclPkgQual    = packageQualifier
    , ideclSource     = sourceImport
    , ideclLevelSpec  = levelStyle
    , ideclSafe       = safeImport
    , ideclQualified  = qualifiedStyle
    , ideclAs         = importAlias
    , ideclImportList = importList
    } ->
      project path
              importedModule
              packageQualifier
              sourceImport
              levelStyle
              safeImport
              qualifiedStyle
              importAlias
              implicitImport
              importList
