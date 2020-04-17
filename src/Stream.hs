module Stream where

import qualified Data.List as L

-- Homework 6
-- exercise 3
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show s = "[" ++ showStream 20 s ++ "]"

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

streamTake :: Int -> Stream a -> [a]
streamTake n = take n . streamToList

showStream :: Show a => Int -> Stream a -> String
showStream n s =
    let xs = streamTake n s
        str = L.intercalate ", " $ map show xs
    in str ++ ", ..."

-- exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a b) = Cons (f a) (streamMap f b)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f $ f x)

-- exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = r 0
    where
        r n = interleaveStreams (streamRepeat n) (r $ n + 1)

-- pattern-match on both streams would force strictness
-- in recursive stream definition
-- see https://stackoverflow.com/a/37179658/519015
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a xs) ys =
    Cons a (interleaveStreams ys xs)
