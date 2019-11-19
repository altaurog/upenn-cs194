module TestHomework01 where

import Test.Tasty
import Test.Tasty.HUnit

import Luhn

tests :: TestTree
tests = testGroup "Homework 01"
    [ testEx01
    , testEx02
    , testEx04
    ]

testEx01 :: TestTree
testEx01 = testGroup "Exercise 1 - toDigits(Rev)"
    [ param1 "toDigits" toDigits
        [ (1234, [4, 3, 2, 1])
        , (0, [])
        , (-16, [6, 1])
        ]
    ]

testEx02 :: TestTree
testEx02 = testGroup "Refactor"
    [ param1 "digitVal" digitVal
        [ ((2, True), 4)
        , ((3, False), 3)
        , ((8, True), 7)
        ]
    , param1 "luhnSum" luhnSum
        [ ([7, 8, 5, 6], (7 + 1 + 6 + 5 + 1 + 2))
        , ([3, 5, 1], (3 + 1 + 1))
        ]
    ]

testEx04 :: TestTree
testEx04 = testGroup "Exercise 4 - validate"
    [ param1 "validate" validate
        [ (4012888888881881, True)
        , (4012888888881882, False)
        , (-4012888888881881, True)
        , (-4012888888881882, False)
        , (0, True)
        , (18, True)
        , (19, False)
        ]
    ]

-- parameterize tests of unary function
param1 :: (Show a, Show b, Eq b) => String -> (a -> b) -> [(a, b)] -> TestTree
param1 name f testParams =
    let
        tname a = name ++ " " ++ (show a)
        test (a, b) = testCase (tname a) $ f a @?= b
    in testGroup name $ map test testParams
