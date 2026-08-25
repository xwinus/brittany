{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module StructuralSmallRecordEdge where

compactOne value = Tiny { one = value }

compactTwo one two = Tiny { one, two }

nestedOne argument =
  Outer
    { outerValue =
      InnerOne
        { innerValue =
          transformLongValue argument additionalArgument finalArgument
        }
    }

nestedTwo argument =
  Outer
    { outerValue =
      InnerTwo
        { firstValue  = transformLongValue argument
        , secondValue = transformAgain argument
        }
    }

nestedThree argument =
  Outer
    { outerValue =
      InnerThree
        { firstValue  = transformLongValue argument
        , secondValue = transformAgain argument
        , thirdValue  = transformFinally argument
        }
    }

updateSettings settings argument =
  settings
    { firstValue  = transformLongValue argument
    , secondValue = transformAgain argument
    }

punned firstConfigurationSettingLongField secondConfigurationSettingLongField =
  Settings
    { firstConfigurationSettingLongField
    , secondConfigurationSettingLongField
    }

wildcard firstValue argument =
  Settings
    { firstValue
    , secondValue = transformLongValue argument
    , thirdValue  = transformAgain argument
    , ..
    }

commentedRecordWithLongBindingName argument = Settings
  { -- Keep this comment with the first field.
    firstValue = transformLongValue argument
  , secondValue = transformAgain argument
  }
