module TestHomework04 where

import Test.Tasty

import TestUtil

import Idioms

tests :: TestTree
tests = testGroup "Homework 03"
    [ testEx01
    , testEx02
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



testEx02 :: TestTree
testEx02 = testGroup "Exercise 2 - AVL Tree"
    [ param1 "rotateRight" rotateRight
        [ (Leaf, Leaf)
        , (a, a)
        , ((Node 2 (Node 1 a 15 b) 25 c), (Node 2 a 15 (Node 1 b 25 c)))
        ]
    , param1 "rotateLeft" rotateLeft
        [ (Leaf, Leaf)
        , (a, a)
        , ((Node 2 a 15 (Node 1 b 25 c)), (Node 2 (Node 1 a 15 b) 25 c))
        ]
    , param1 "foldTree" foldTree
        [ ([], Leaf :: Tree Integer)
        , ([10], a)
        , ([10, 20], Node 1 a 20 Leaf)
        , ([10, 20, 30], Node 1 a 20 c)
        , ([20, 10, 30], Node 1 a 20 c)
        , ([30, 20, 10], Node 1 a 20 c)
        , ([30, 10, 20], Node 1 a 20 c)
        , ([10, 20, 30, 40], Node 2 (Node 1 a 20 Leaf) 30 d)
        , ([10, 20, 30, 40, 50], Node 2 (Node 1 a 20 c) 40 e)
        , ([20, 10, 30, 40, 50], Node 2 (Node 1 a 20 c) 40 e)
        ]
    ]
    where
        a = Node 0 Leaf 10 Leaf :: Tree Integer
        b = Node 0 Leaf 20 Leaf
        c = Node 0 Leaf 30 Leaf
        d = Node 0 Leaf 40 Leaf
        e = Node 0 Leaf 50 Leaf
