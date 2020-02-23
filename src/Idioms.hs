module Idioms where

import Control.Arrow ((>>>))

fun1' :: [Integer] -> Integer
fun1' = foldl f 1
    where f agg v
            | even v    = (v - 2) * agg
            | otherwise = agg


fun2' :: Integer -> Integer
fun2' =
    iterate f
    >>> takeWhile (/= 1)
    >>> filter even
    >>> sum
    where f v
            | even v    = v `div` 2
            | otherwise = 3 * v + 1


fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs


-- apparently this is what was intended
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)
