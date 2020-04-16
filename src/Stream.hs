module Stream where

-- Homework 6
-- exercise 3
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show s = "[" ++ showStream 0 s ++ "]"

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

showStream :: Show a => Integer -> Stream a -> String
showStream n (Cons a b)
    | n < 20    = show a ++ ", " ++ showStream (n+1) b
    | otherwise = "..."

-- exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a b) = Cons (f a) (streamMap f b)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f $ f x)
