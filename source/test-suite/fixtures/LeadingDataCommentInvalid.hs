module LeadingDataCommentInvalid where

-- -----------------------------------------------------------------------------
-- Broken declaration
-- -----------------------------------------------------------------------------

-- | This declaration must remain unchanged after the parse error.
data Broken
  = Valid
  |
