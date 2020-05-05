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
    , testEx06
    ]

f15 = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]

testEx01 :: TestTree
testEx01 = testGroup "Exercise 1 - Fibonacci"
    [ param1 "fib" fib $ zip
        [0..5] [0, 1, 1, 2, 3, 5]
    , testCase "fibs1" $
        take 15 fibs1 @?= f15
    ]

testEx02 :: TestTree
testEx02 = localOption (mkTimeout 2) $ testGroup "Exercise 2 - Faster Fibonacci I"
    [ param1 "fib2" fib2 [(16, 987)]
    , testCase "fibs2" $
        take 15 fibs2 @?= f15
    ]

testEx02a :: TestTree
testEx02a = localOption (mkTimeout 1) $ testGroup "Exercise 2 - Faster Fibonacci II"
    [ testCase "fibs2'" $
        take 15 fibs2' @?= f15
    ]

allOne :: Stream Integer
allOne = Cons 1 allOne

testEx03 :: TestTree
testEx03 = testGroup "Exercise 3 - Stream data type"
    [ testCase "streamToList" $
        take 3 (streamToList allOne) @?= [1, 1, 1]
    , testCase "streamTake" $
        streamTake 3 allOne @?= [1, 1, 1]
    , testCase "showStream" $
        showStream 4 allOne @?= "1, 1, 1, 1, ..."
    , testCase "show instance" $
        show allOne @?= "[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...]"
    ]

allTwo :: Stream Integer
allTwo = streamRepeat 2

testEx04 :: TestTree
testEx04 = testGroup "Exercise 4 - Stream manipulation"
    [ testCase "streamRepeat" $
        streamTake 5 allTwo @?= [2, 2, 2, 2, 2]
    , testCase "streamMap" $
        streamTake 5 (streamMap (+3) allTwo) @?= [5, 5, 5, 5, 5]
    , testCase "streamFromSeed" $
        streamTake 5 (streamFromSeed (*2) 1) @?= [1, 2, 4, 8, 16]
    ]

alternating :: Stream Integer
alternating = interleaveStreams allOne allTwo

testEx05 :: TestTree
testEx05 = testGroup "Exercise 5 - Stream generation"
    [ testCase "nats" $
        streamTake 5 nats @?= [0..4]
    , testCase "interleaveStreams" $
        streamTake 6 alternating @?= [1, 2, 1, 2, 1, 2]
    , testCase "ruler" $
        streamTake 64 ruler @?= [
            0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0,
            1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1,
            0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
            6
        ]
    ]

testEx06 :: TestTree
testEx06 = testGroup "Exercise 6 - generating functions"
    [ testCase "Num fromInteger" $
        streamTake 5 (fromInteger 3) @?= [3, 0, 0, 0, 0]
    , testCase "Num negate" $
        streamTake 5 (negate allTwo) @?= [-2, -2, -2, -2, -2]
    , testCase "Num addition" $
        streamTake 5 (allOne + allTwo) @?= [3, 3, 3, 3, 3]
    , testCase "Num mulitplication" $
        streamTake 5 (a * b) @?= [-4, 0, 1, 0, 0]
    , testCase "Num mulitplication" $
        streamTake 5 (b * c) @?= [-6, -5, 2, 1, 0]
    , testCase "Fractional division" $
        streamTake 5 (d / b) @?= [2, 1, 0, 0, 0]
    , testCase "fibs3" $
        streamTake 15 fibs3 @?= f15
    ]
    where
        zero = streamRepeat 0
        a = Cons 2 (Cons 1 zero)
        b = Cons (-2) (Cons 1 zero)
        c = Cons 3 (Cons 4 (Cons 1 zero))
        d = Cons (-4) (Cons 0 (Cons 1 zero))
