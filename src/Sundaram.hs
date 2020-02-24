module Sundaram where

import qualified Data.List as List

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
    let jmax i = (n - i) `div` (2 * i + 1)
        exclude = [i + j + 2 * i * j | i <- [1 .. n], j <- [i .. jmax i]]
        remaining = foldr List.delete [1 .. n] exclude
    in 2:[2 * x + 1 | x <- remaining]
