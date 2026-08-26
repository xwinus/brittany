module SignaturePostDoc where

fromText
  :: a
  -- ^ initial state of analyzing function
  -> (Text -> State a LineType)
  -- ^ function that analyzes currently processed line
  -> Text
  -- ^ raw source code to analyze
  -> SourceCode
  -- ^ analyzed 'SourceCode'
fromText = undefined
