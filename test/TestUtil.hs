module TestUtil where

import Test.Tasty
import Test.Tasty.HUnit

-- parameterize tests of unary function
param1 :: (Show a, Show b, Eq b) => String -> (a -> b) -> [(a, b)] -> TestTree
param1 name f testParams =
    let
        trunc n s
            | length s > n = (take n s) ++ "..."
            | otherwise     = s
        tname a = trunc 55 $ name ++ " " ++ (show a)
        test (a, b) = testCase (tname a) $ f a @?= b
    in testGroup name $ map test testParams
