module TopLevelSpacingInvalid where

import Data.List (sort)

first = 1

-- Keep the malformed declaration separated.
broken =

second = sort [2, 1]
