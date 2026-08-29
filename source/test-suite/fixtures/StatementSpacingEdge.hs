{-# LANGUAGE RecursiveDo #-}
module StatementSpacingEdge where

workflow value =
  do
    prepare
    validate

    result <- case value of
      Just item ->
        do
          first item
          second item

          -- Keep this comment with the final step.
          finalStep item


          cleanup item
      Nothing -> do
        fallback
        finish
    publish result
 where
  validate =
    do
      checkOne
      checkTwo

      -- Keep this comment with the third check.
      checkThree

recursiveWorkflow = mdo
  seed <- initialize

  rec first <- advance second

      second <- advance first
  complete seed first second
