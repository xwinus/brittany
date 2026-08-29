module CompositionalPatternInvalid where

broken Request
  { requestIdentifier = identifier
  , requestMetadata = Metadata { owner = owner, = missingField }
  } = result
