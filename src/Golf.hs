{-# OPTIONS_GHC -Wall #-}
module Golf where

import Control.Arrow ((>>>))
import qualified Data.Map.Strict as Map
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
 - get nonempty bins
 - then make a numeric histogram
 - then get size of largest bin
 - construct histogram horizontally and transpose it
 -}
histogram :: [Integer] -> String
histogram d =
    let bins = getBins d
        hist = [ fromMaybe 0 $ Map.lookup x bins | x <- [0 .. 9] ]
        peak = maximum hist
        horiz = [ (replicate (peak - x) ' ') ++ (replicate x '*') | x <- hist ]
        vert = (List.transpose horiz) ++ ["==========", "0123456789", ""]
    in List.intercalate "\n" vert

{-|
 - put nonempty histogram bins in Map Integer Int
 -}
getBins :: [Integer] -> Map.Map Integer Int
getBins =
    filter (\x -> x >= 0 && x < 10)  -- only capture values 0 - 9
    >>> List.sort
    >>> List.group                   -- split list into value groups
    >>> map mapEntry                 -- get (value, count) pairs
    >>> catMaybes
    >>> Map.fromList                 -- construct Map

{-|
 - (value, count) tuple for histogram bins
 -}
mapEntry :: [Integer] -> Maybe (Integer, Int)
mapEntry [] = Nothing
mapEntry list@(x:_) = Just (x, length list)
