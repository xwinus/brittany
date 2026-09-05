module ClassFinalPostDoc where

class Template a where
    templateExtensions :: NonEmpty Text
        -- ^ list of supported file extensions

emptyTemplate :: Int
emptyTemplate = undefined
