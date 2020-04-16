module Fibonacci where

import Data.Function.Memoize

-- Homework 6
-- exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n - 1)) + (fib (n - 2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- exercise 2
fib2 = memoize fibm

fibm :: Integer -> Integer
fibm 0 = 0
fibm 1 = 1
fibm n = (fib2 (n - 1)) + (fib2 (n - 2))

fibs2 :: [Integer]
fibs2 = map fib2 [0..]
