module StructuralSmallRecord where

syntaxAnalysis =
  SyntaxAnalysis
    { saIsCommentStart = isMatch startPattern
    , saIsCommentEnd   = isMatch endPattern
    }
