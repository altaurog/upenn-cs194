module TestHomework01 where

import Test.Tasty
import Test.Tasty.HUnit

import Luhn

tests :: TestTree
tests = testGroup "Homework 01"
    [ testEx01
    , testEx02
    , testEx03
    , testEx04
    ]

testEx01 :: TestTree
testEx01 = testGroup "Exercise 1 - toDigits(Rev)"
    [ param1 "toDigits" toDigits
        [ (1234, [1, 2, 3, 4])
        , (0, [0])
        , (-16, [1, 6])
        ]
    , param1 "toDigitsRev" toDigitsRev
        [ (1234, [4, 3, 2, 1])
        , (0, [0])
        , (-16, [6, 1])
        ]
    ]

testEx02 :: TestTree
testEx02 = testGroup "Exercise 2 - doubleEven"
    [ param1 "doubleEven" doubleEven
        [ ([4, 3, 2, 1], [2, 2, 6, 4])
        , ([1, 3, 5], [5, 6, 1])
        ]
    ]

testEx03 :: TestTree
testEx03 = testGroup "Exercise 3 - sumDigits"
    [ param1 "sumDigits" sumDigits
        [ ([16, 7, 12, 5], (1 + 6 + 7 + 1 + 2 + 5))
        , ([10, 3, 2], (1 + 3 + 2))
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
