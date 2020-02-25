module TestHomework05 where

import Test.Tasty

import TestUtil

import ExprT
import Parser
import Calc
import Util
import qualified StackVM as VM

tests :: TestTree
tests = testGroup "Homework 05"
    [ testEx01
    , testEx02
    , testEx03
    , testEx04
    , testEx05
    ]

testEx01 :: TestTree
testEx01 = testGroup "Exercise 1 - calc version 1"
    [ param1 "eval" eval
        [ (Lit 1, 1)
        , (Add (Lit 1) (Lit 7), 8)
        , (Mul (Lit 3) (Lit 4), 12)
        , (Mul (Add (Lit 2) (Lit 3)) (Lit 4), 20)
        ]
    ]

testEx02 :: TestTree
testEx02 = testGroup "Exercise 2 - with parser"
    [ param1 "evalStr" evalStr
        [ ("eval", Nothing)
        , ("2", Just 2)
        , ("1 + 2", Just 3)
        , ("3 * 5", Just 15)
        , ("1 + 9 * 2", Just 19)
        ]
    ]

testEx03 :: TestTree
testEx03 = testGroup "Exercise 3 - with typeclass"
    [ param1 "eval w/ typeclass" eval
        [ (lit 1, 1)
        , (add (lit 1) (lit 7), 8)
        , (mul (lit 3) (lit 4), 12)
        , (mul (add (lit 2) (lit 3)) (lit 4), 20)
        ]
    ]

testEx04 :: TestTree
testEx04 = testGroup "Exercise 4 - new instances"
    [ test "Integer instance"
        [ ("1", (Just 1)::Maybe Integer)
        , ("x", Nothing)
        , ("1 + 7", Just 8)
        , ("3 * 4", Just 12)
        , ("(3 * -4) + 5", Just $ -7)
        ]
    , test "Bool instance"
        [ ("1", Just True)
        , ("x", Nothing)
        , ("1 + 7", Just True)
        , ("3 * 4", Just True)
        , ("-7 + 2", Just True)
        , ("-7 * 2", Just False)
        , ("(3 * -4) + 5", Just True)
        ]
    , test "MinMax instance"
        [ ("1", Just (MinMax 1))
        , ("x", Nothing)
        , ("1 + 7", Just (MinMax 7))
        , ("3 * 4", Just (MinMax 3))
        , ("-7 + 2", Just (MinMax 2))
        , ("-7 * 2", Just (MinMax $ -7))
        , ("(3 * -4) + 5", Just (MinMax 5))
        ]
    , test "Mod7 instance"
        [ ("1", Just (Mod7 1))
        , ("x", Nothing)
        , ("9", Just (Mod7 2))
        , ("-5", Just (Mod7 2))
        , ("3 + 5", Just (Mod7 1))
        , ("13 * 4", Just (Mod7 3))
        , ("-7 + 2", Just (Mod7 2))
        , ("-7 * 2", Just (Mod7 0))
        , ("(3 * -4) + 5", Just (Mod7 0))
        ]
    ]
    where test s = param1 s (parseExp lit add mul)


testEx05 :: TestTree
testEx05 = testGroup "Exercise 5 - stack vm"
    [ param1 "stack vm" run
        [ ("1", Right (VM.IVal 1))
        , ("1 + 7", Right (VM.IVal 8))
        , ("3 * 4", Right (VM.IVal 12))
        , ("(3 * -4) + 5", Right (VM.IVal (-7)))
        ]
    ]
    where
        run s = do
            program <- maybeToEither "parse error" $ compile s
            VM.stackVM program
