module StructuralSmallRecordInvalid where

brokenNested = Outer
  { outerValue = Inner
      { firstValue = valid
      , secondValue =
      }
  }

brokenUpdate settings = settings
  { firstValue = valid
  , secondValue =
  }
