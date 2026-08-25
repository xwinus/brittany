{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module VerticalRecordEdge where

smallOne value = Settings { settingOne = value }

smallTwo settingOne settingTwo = Settings { settingOne, settingTwo }

smallFour = Tiny { a = one, b = two, c = three, d = four }

smallThree =
  Settings { firstSetting  = someLongValue
           , secondSetting = anotherLongValue
           , thirdSetting  = finalLongValue
           }

headroomStyle =
  HeadroomStyle
    { field01 = undefined
    , field02 = undefined
    , field03 = undefined
    , field04 = undefined
    , field05 = undefined
    , field06 = undefined
    , field07 = undefined
    , field08 = undefined
    , field09 = undefined
    , field10 = undefined
    , field11 = undefined
    , field12 = undefined
    , field13 = undefined
    , field14 = undefined
    , field15 = undefined
    }

largeConstruct fieldOne fieldTwo =
  LargeRecord
    { fieldOne
    , fieldTwo
    , fieldThree   = nestedValue
        argumentOne
        argumentTwo
    , -- Keep this comment with field four.
      fieldFour    = undefined
    , fieldFive    = undefined
    , fieldSix     = undefined
    , fieldSeven   = undefined
    , fieldEight   = undefined
    , fieldNine    = undefined
    , fieldTen     = undefined
    , fieldEleven  = undefined
    , fieldTwelve  = undefined
    , fieldThirteen = undefined
    , fieldFourteen = undefined
    , fieldFifteen  = undefined
    , ..
    }

largeUpdate record =
  record
    { fieldOne   = undefined
    , fieldTwo   = undefined
    , fieldThree = nestedValue
        argumentOne
        argumentTwo
    , -- Keep this update comment with field four.
      fieldFour  = undefined
    }
