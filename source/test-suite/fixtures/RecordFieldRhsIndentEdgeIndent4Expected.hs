{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module RecordFieldRhsIndentEdge where

firstAndLater argument =
    Settings
        { firstField  =
              transformFirstValue argument
                                  additionalArgument
        , secondField =
              transformSecondValue
                  argument
                  additionalArgument
        }

oneField firstArgument secondArgument =
    Handler
        { runHandler =
              \inputValue -> combineHandlerValues
                  firstArgument
                  secondArgument
                  inputValue
        }

nested argument =
    Outer
        { outerField =
              Inner
                  { innerField =
                        transformNestedValue
                            argument
                            additionalArgument
                  }
        }

updated settings inputValue =
    settings
        { updateField   =
              case inputValue of
                  FirstInput firstValue ->
                      transformFirstInput
                          firstValue
                          additionalArgument
                  SecondInput secondValue ->
                      transformSecondInput
                          secondValue
                          additionalArgument
        , retainedField = retainedValue
        }

effectful firstInput secondInput =
    Container
        { runAction =
              do
                  firstResult  <- runFirstAction firstInput
                  secondResult <- runSecondAction
                      secondInput
                  combineResults firstResult secondResult
        }

listed firstValue secondValue =
    Container
        { values =
              [ transformFirstValue firstValue
                                    additionalArgument
              , transformSecondValue secondValue
                                     additionalArgument
              ]
        }

commented punnedField explicitValue =
    Settings
        { -- Keep this comment before the first field.
          commentedField =
              -- Keep this comment after the equals sign.
              commentedValue explicitValue extra
        , punnedField
        , explicitField = explicitValue
        , ..
        }
