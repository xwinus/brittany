module RecordFieldRhsIndent where

value =
  Outer
    { outerField =
        Inner
          { innerA =
              someVeryLongIdentifier
          , innerB =
              anotherVeryLongIdentifier
          }
    }
