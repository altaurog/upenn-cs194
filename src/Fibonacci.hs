module Fibonacci where

import Data.Function.Memoize
import Matrix
import Stream

-- Homework 6
-- exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n - 1)) + (fib (n - 2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- exercise 2 - dumb approach
fib2 :: Integer -> Integer
fib2 = memoize fibm

fibm :: Integer -> Integer
fibm 0 = 0
fibm 1 = 1
fibm n = (fib2 (n - 1)) + (fib2 (n - 2))

fibs2 :: [Integer]
fibs2 = map fib2 [0..]

-- exercise 2 - handcrafted approach
{- fibonacci sequence is inherently recursive,
 - don’t need to compute as function of index,
 - but do need additional state (penultimate value)
 -}
fibs2' :: [Integer]
fibs2' = map fst $ iterate fm (0, 1)

fm :: (Integer, Integer) -> (Integer, Integer)
fm (a, b) = (b, a + b)

-- exercise 6
fibs3 :: Stream Integer
fibs3 = x / a
    where
        x = streamFromList [0, 1]
        a = streamFromList [1, -1, -1]

-- exercise 7
fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = matrixElem (f ^ n)

f :: Matrix Integer
f = Matrix 1 1 1 0

matrixElem :: Matrix a -> a
matrixElem (Matrix _ a _ _) = a
