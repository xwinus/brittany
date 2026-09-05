module ClassFinalPostDocEdge where

class Render a where
    render :: a -> Text
        -- ^ rendered value

-- | The default label.
defaultLabel :: Text
defaultLabel = undefined
