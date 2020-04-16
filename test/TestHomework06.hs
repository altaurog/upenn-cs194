module TestHomework06 where

import Test.Tasty
import Test.Tasty.HUnit

import TestUtil

import Fibonacci
import Stream

tests :: TestTree
tests = testGroup "Homework 06"
    [ testEx01
    , testEx02
    , testEx02a
    , testEx03
    , testEx04
    , testEx05
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

allOne :: Stream Integer
allOne = Cons 1 allOne

testEx03 :: TestTree
testEx03 = testGroup "Exercise 3 - Stream data type"
    [ testCase "streamToList" $
        take 3 (streamToList allOne) @?= [1, 1, 1]
    , testCase "show instance" $
        show allOne @?= "[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...]"
    ]

allTwo :: Stream Integer
allTwo = streamRepeat 2

testEx04 :: TestTree
testEx04 = testGroup "Exercise 4 - Stream manipulation"
    [ testCase "streamRepeat" $
        take 5 (streamToList allTwo) @?= [2, 2, 2, 2, 2]
    , testCase "streamMap" $
        take 5 (streamToList $ streamMap (+3) allTwo) @?= [5, 5, 5, 5, 5]
    , testCase "streamFromSeed" $
        take 5 (streamToList $ streamFromSeed (*2) 1) @?= [1, 2, 4, 8, 16]
    ]

alternating :: Stream Integer
alternating = interleaveStreams allOne allTwo

testEx05 :: TestTree
testEx05 = testGroup "Exercise 5 - Stream generation"
    [ testCase "nats" $
        take 5 (streamToList nats) @?= [0..4]
    , testCase "interleaveStreams" $
        take 6 (streamToList alternating) @?= [1, 2, 1, 2, 1, 2]
    , testCase "ruler" $
        take 64 (streamToList ruler) @?= [
            0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0,
            1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1,
            0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
            6
        ]
    ]
