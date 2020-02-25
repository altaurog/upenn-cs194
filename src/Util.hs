module Util where

{-|
 - count occurrences of item in list
 -}
count :: Eq a => [a] -> a -> Int
count a x = foldl f 0 a
    where f agg v = if v == x then agg + 1 else agg

{-|
 - convert Maybe value to Either
 -}
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither = flip maybe Right . Left
