module TestHomework06 where

import Test.Tasty
import Test.Tasty.HUnit

import TestUtil

import Fibonacci

tests :: TestTree
tests = testGroup "Homework 06"
    [ testEx01
    , testEx02
    , testEx02a
    ]

testEx01 :: TestTree
testEx01 = testGroup "Exercise 1 - Fibonacci"
    [ param1 "fib" fib $ zip
        [0..5] [0, 1, 1, 2, 3, 5]
    , testCase "fibs1" $
        take 15 fibs1 @?=
            [ 0
            , 1
            , 1
            , 2
            , 3
            , 5
            , 8
            , 13
            , 21
            , 34
            , 55
            , 89
            , 144
            , 233
            , 377
            ]
    ]

testEx02 :: TestTree
testEx02 = localOption (mkTimeout 2) $ testGroup "Exercise 2 - Faster Fibonacci I"
    [ param1 "fib2" fib2 [(16, 987)]
    , testCase "fibs2" $
        take 15 fibs2 @?=
            [ 0
            , 1
            , 1
            , 2
            , 3
            , 5
            , 8
            , 13
            , 21
            , 34
            , 55
            , 89
            , 144
            , 233
            , 377
            ]
    ]

testEx02a :: TestTree
testEx02a = localOption (mkTimeout 1) $ testGroup "Exercise 2 - Faster Fibonacci II"
    [ testCase "fibs2'" $
        take 15 fibs2' @?=
            [ 0
            , 1
            , 1
            , 2
            , 3
            , 5
            , 8
            , 13
            , 21
            , 34
            , 55
            , 89
            , 144
            , 233
            , 377
            ]
    ]
