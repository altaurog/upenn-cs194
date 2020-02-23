module TestHomework04 where

import Test.Tasty

import TestUtil

import Idioms

tests :: TestTree
tests = testGroup "Homework 03"
    [ testEx01
    ]

testEx01 :: TestTree
testEx01 = testGroup "Exercise 1 - Wholemeal programming"
    [ matchOutput "fun1" fun1' fun1
        [ []
        , [1, 2]
        , [1 .. 10]
        , [1, 1, 1, 1, 1, 1]
        , [2, 2, 2, 2, 2, 2]
        , [4, 6, 8]
        , [3, 5, 7, 9]
        ]
    , matchOutput "fun2" fun2' fun2 $
        [ 1 .. 10 ] ++ [15, 18, 25]
    ]
