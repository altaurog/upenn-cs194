module Luhn where

import Data.Function ((&))

-- validate Luhn check digit at the end of an Integer
validate :: Integer -> Bool
validate n =
    let s = n & toDigitsRev & doubleEven & sumDigits
    in mod s 10 == 0


toDigits :: Integer -> [Integer]
toDigits n = _digits (abs n) []
    where _digits n ds
            | n < 10    = n : ds
            | otherwise = _digits (div n 10) ((mod n 10) : ds)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

{- The assignment sheet says this function should double every other number
 - *beginning from the right*, but this is incorrect; since we will have
 - already reversed the order of the digits in toDigitsRev, we should double
 - alternating digits starting from the right.
 -}
doubleEven :: [Integer] -> [Integer]
doubleEven ds = _doubleEven ds [] False
    where
        cd x p = if p then 2 * x else x
        _doubleEven [] r _ = r
        _doubleEven (x:xs) r p = _doubleEven xs ((cd x p):r) (not p)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
    | x < 10    = x + sumDigits xs
    | otherwise = x - 9 + sumDigits xs
