module TestUtil where

import Test.Tasty
import Test.Tasty.HUnit

-- parameterize tests of unary function
param1 :: (Show a, Show b, Eq b) => String -> (a -> b) -> [(a, b)] -> TestTree
param1 name f testParams =
    let
        test (a, b) = testCase (tname 55 name a) $ f a @?= b
    in testGroup name $ map test testParams


-- test that output of two functions is same
matchOutput :: (Show a, Show b, Eq b) => String -> (a -> b) -> (a -> b) -> [a] -> TestTree
matchOutput name f g testParams =
    let
        test a = testCase (tname 55 name a) $ f a @?= g a
    in testGroup name $ map test testParams


trunc :: Int -> String -> String
trunc len s
    | length s > len = (take len s) ++ "..."
    | otherwise     = s


tname :: Show a => Int -> String -> a -> String
tname len name a = trunc len $ name ++ " " ++ (show a)
