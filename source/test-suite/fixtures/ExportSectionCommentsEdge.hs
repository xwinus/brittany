module ExportSectionCommentsEdge
  ( -- * Public API
    Thing(..)
    -- ** Values
  , value
    -- Keep the operator export in this section.
  , (.+.)
    -- $named
  , documented
    {-| Multiline export documentation.
        stays with multiline.
    -}
  , multiline
  , finalValue
    -- End of exports.
  ) where
data Thing = Thing Int
value = 1
left .+. right = left + right
documented = 2
multiline = 3
finalValue = 4
