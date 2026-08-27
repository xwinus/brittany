module RecordFieldRhsIndentInvalid where

broken = Outer { outerField = Inner { innerField = } }

brokenUpdate settings = settings { firstField = valid, secondField = }
