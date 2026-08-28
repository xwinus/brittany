module LetStatementBoundaryComments where

commented action = do
  let cleanup = prepare action
  -- Keep this comment with the following statement.
  (do
      first action
      -- Keep this nested comment.
      second action
    ) `onException` cleanup
  finish action
