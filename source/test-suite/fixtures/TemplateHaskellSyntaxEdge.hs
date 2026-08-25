{-# LANGUAGE TemplateHaskell #-}
module TemplateHaskellSyntaxEdge where
expressionQuote =
  [| -- Keep the expression quote comment.
     1 + 2
   |]
expressionSplice =
  $( -- Keep the expression splice comment.
     pure [| 1 |]
   )
declarations =
  [d|
  -- Keep the declaration quote comment.
  generated = 1
  |]
$( -- Keep the declaration splice comment.
   pure []
 )
