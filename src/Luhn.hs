module Luhn where

import Data.Function ((&))
import Data.Tuple (swap)
import Data.List (unfoldr)

-- validate Luhn check digit at the end of an Integer
validate :: Integer -> Bool
validate n =
    let s = n & toDigits & luhnSum
    in mod s 10 == 0

toDigits :: Integer -> [Integer]
toDigits = (unfoldr digitF) . abs

digitF :: Integer -> Maybe (Integer, Integer)
digitF 0 = Nothing
digitF n = Just $ swap $ divMod n 10

luhnSum :: [Integer] -> Integer
luhnSum ds = sum $ map digitVal $ zip ds $ cycle [False, True]

digitVal :: (Integer, Bool) -> Integer
digitVal (x, False) = x
digitVal (x, True)
    | x > 4     = 2 * x - 9
    | otherwise = 2 * x
