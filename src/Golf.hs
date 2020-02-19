{-# OPTIONS_GHC -Wall #-}
module Golf where

import Control.Arrow ((>>>))
import Data.List.Split (chunksOf)
import Data.List (unfoldr)
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
    >>> chunksOf n
    >>> map listToMaybe
    >>> catMaybes


-- Exercise 2
{-|
 - use unfold to apply unfolding function repeatedly
 - then use catMaybes to collect the results
 -}
localMaxima :: Ord a => [a] -> [a]
localMaxima = catMaybes . unfoldr lmf

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
