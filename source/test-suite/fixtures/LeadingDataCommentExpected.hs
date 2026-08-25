module LeadingDataCommentExpected where

-- | Application command.
data Command
  = Run
      VeryLongFirstArgumentTypeName
      VeryLongSecondArgumentTypeName
      VeryLongThirdArgumentTypeName
  | Init

-- This ordinary comment stays with the declaration.
-- | Application result.
data Result
  = Succeeded
  | Failed VeryLongFailureDescriptionTypeName
