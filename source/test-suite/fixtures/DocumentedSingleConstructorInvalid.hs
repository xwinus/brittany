module DocumentedSingleConstructorInvalid where

-- | The record is missing its closing brace.
newtype Broken
    = Broken
        { brokenField :: Int
        -- ^ Broken field documentation.
