module TestHomework03 where

import Test.Tasty

import TestUtil

import Golf

tests :: TestTree
tests = testGroup "Homework 03"
    [ testEx01
    , testEx02
    ]

testEx01 :: TestTree
testEx01 = testGroup "Exercise 1 - Hopscotch"
    [ param1 "skips" skips
        [ ("ABCD", ["ABCD", "BD", "C", "D"])
        , ("hello!", ["hello!", "el!", "l!", "l", "o", "!"])
        ]
    , param1 "skips" skips
        [ ([True, False], [[True, False], [False]])
        , ([], [])
        ]
    , param1 "skips" skips [ ([1], [[1]]) ]
    , param1 "everyNth" (flip everyNth [1 .. 10])
        [ (1, [1 .. 10])
        , (2, [2, 4, 6, 8, 10])
        , (3, [3, 6, 9])
        , (4, [4, 8])
        , (5, [5, 10])
        , (6, [6])
        ]
    ]
