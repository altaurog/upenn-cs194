{-# OPTIONS_GHC -Wall #-}
module Golf where

import Control.Arrow ((>>>))
import qualified Data.List.Split as Split
import qualified Data.List as List
import Data.Maybe

-- Exercise 1
{-|
 - use enumFromTo to construct a list with all the skip distances
 - use map to apply everyNth to each skip distance
 - (wouldn’t need flip if everyNth were defined as [a] -> Int -> [a]
 - but I believe Int -> [a] -> [a] is more sensible)
 -}
skips :: [a] -> [[a]]
skips a = map (flip everyNth a) (enumFromTo 1 $ length a)

{-|
 - use drop and chunksOf to break list up where we need it
 - use listToMaybe and catMaybes to safely get the first
 - item in each list
 -}
everyNth :: Int -> [a] -> [a]
everyNth n =
    drop (n - 1)
    >>> Split.chunksOf n
    >>> map listToMaybe
    >>> catMaybes


-- Exercise 2
{-|
 - use unfold to apply unfolding function repeatedly
 - then use catMaybes to collect the results
 -}
localMaxima :: Ord a => [a] -> [a]
localMaxima = catMaybes . List.unfoldr lmf

{-|
 - unfolding function: look for local maximum at beginning of list
 - and move to next position by returning list tail
 - if list isn’t long enough, signal termination
 -}
lmf :: Ord a => [a] -> Maybe (Maybe a, [a])
lmf (x1:x2:x3:xs)
    | x1 < x2 && x3 < x2  = Just (Just x2, x2:x3:xs)
    | otherwise           = Just (Nothing, x2:x3:xs)
lmf _ = Nothing


-- Exercise 3
{-|
 - make a numeric histogram
 - then get size of largest bin
 - construct histogram horizontally and transpose it
 -}
histogram :: [Integer] -> String
histogram =
    count
    >>> flip map [0 .. 9]
    >>> horiz
    >>> List.transpose
    >>> (++ ["==========", "0123456789", ""])
    >>> List.intercalate "\n"

{-|
 - count occurrences of item in list
 -}
count :: Eq a => [a] -> a -> Int
count a x = foldl f 0 a
    where f agg v = if v == x then agg + 1 else agg

{-|
 - make a horizontal histogram
 -}
horiz :: [Int] -> [String]
horiz hist = [ (replicate (peak - x) ' ') ++ (replicate x '*') | x <- hist ]
    where peak = maximum hist
