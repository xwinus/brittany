module VerticalRecordExpected where

fileSupport =
  FileSupport
    { fsSyntaxAnalysis      = syntaxAnalysis
    , fsExtractTemplateData = extractTemplateData
    , fsExtractVariables    = extractVariables
    , fsFileType            = Haskell
    }
