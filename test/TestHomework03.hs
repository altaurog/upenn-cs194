module TestHomework03 where

import Test.Tasty

import TestUtil

import Golf

tests :: TestTree
tests = testGroup "Homework 03"
    [ testEx01
    , testEx02
    , testEx03
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

testEx02 :: TestTree
testEx02 = testGroup "Exercise 2 - Local Maxima"
    [ param1 "localMaxima" localMaxima
        [ ([2, 9, 5, 6, 1], [9, 6])
        , ([2, 3, 4, 1, 5], [4])
        , ([1, 2, 3, 4, 5], [])
        ]
    , param1 "lmf" lmf
        [ ([2, 9, 5, 6, 1], Just (Just 9, [9, 5, 6, 1]))
        , ([2, 3, 4, 1, 5], Just (Nothing, [3, 4, 1, 5]))
        , ([2, 3], Nothing)
        ]
    ]

testEx03 :: TestTree
testEx03 = testGroup "Exercise 3 - Histogram"
    [ param1 "histogram" histogram
        [ ([3, 5],
            "   * *    \n==========\n0123456789\n")
        , ([1, 1, 1, 5],
            " *        \n *        \n *   *    \n==========\n0123456789\n")
        , ([1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9],
            "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n")
        , ([11, 15, -1],
            "==========\n0123456789\n")
        ]
    ]
