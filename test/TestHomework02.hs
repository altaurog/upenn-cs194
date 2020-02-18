module TestHomework02 where

import Test.Tasty

import TestUtil

import Log
import LogAnalysis

tests :: TestTree
tests = testGroup "Homework 02"
    [ testEx01
    , testEx02
    , testEx03
    , testEx04
    , testEx05
    ]

testEx01 :: TestTree
testEx01 = testGroup "Exercise 1 - parse a single message"
    [ param1 "parseMessageType" parseMessageType
        [ (["I", "8", "the", "sandbox"], Just (Info, ["8", "the", "sandbox"]))
        , (["W", "8", "the", "sand"], Just (Warning, ["8", "the", "sand"]))
        , (["E", "10", "sand"], Just (Error 10, ["sand"]))
        , (["E", "sand", "10"], Nothing)
        , (["X", "5", "hello", "world"], Nothing)
        , (["I"], Just (Info, []))
        , ([], Nothing)
        ]
    , param1 "parseTimestamp" parseTimestamp
        [ (["8", "the", "sandbox"], Just (8, ["the", "sandbox"]))
        , (["sand"], Nothing)
        , ([], Nothing)
        ]
    , param1 "maybeMessageParts" maybeMessageParts
        [ (["I", "8", "the", "sandbox"], Just (Info, 8, "the sandbox"))
        , (["W", "8", "the", "sand"], Just (Warning, 8, "the sand"))
        , (["E", "10", "200", "sand"], Just (Error 10, 200, "sand"))
        , (["E", "10", "sand"], Nothing)
        , (["E", "sand", "10"], Nothing)
        , (["X", "5", "hello", "world"], Nothing)
        , (["I"], Nothing)
        , ([], Nothing)
        ]
    , param1 "parseMessage" parseMessage
        [ ("E 2 562 help help", LogMessage (Error 2) 562 "help help")
        , ("I 29 la la la", LogMessage Info 29 "la la la")
        , ("This is not in the right format", Unknown "This is not in the right format")
        ]
    ]

msgA :: LogMessage
msgA = LogMessage Info 5 "alfalfa"

msgB :: LogMessage
msgB = LogMessage Warning 8 "cottage"

msgC :: LogMessage
msgC = LogMessage (Error 3) 9 "kipling"

msgD :: LogMessage
msgD = Unknown "depths"

tree :: MessageTree
tree = Node Leaf msgB Leaf

testEx02 :: TestTree
testEx02 = testGroup "Exercise 2 - binary tree insert"
    [ param1 "insert tree" (flip insert tree)
        [ (msgA, Node (Node Leaf msgA Leaf) msgB Leaf)
        , (msgB, Node (Node Leaf msgB Leaf) msgB Leaf)
        , (msgC, Node Leaf msgB (Node Leaf msgC Leaf))
        , (Unknown "foobar", tree)
        ]
    , param1 "insert leaf" (flip insert Leaf)
        [ (msgA, Node Leaf msgA Leaf)
        , (Unknown "loop", Leaf)
        ]
    , param1 "compareLog" (compareLog msgB)
        [ (msgA, Just GT)
        , (msgB, Just EQ)
        , (msgC, Just LT)
        , (Unknown "nada", Nothing)
        ]
    ]


testEx03 :: TestTree
testEx03 = testGroup "Exercise 3 - tree building"
    [ param1 "build" build
        [ ([msgB, msgA, msgC, msgD],
            Node (Node Leaf msgA Leaf) msgB (Node Leaf msgC Leaf))
        ]
    ]


testEx04 :: TestTree
testEx04 = testGroup "Exercise 4 - tree traversal"
    [ param1 "inOrder" (inOrder . build)
        [ ([msgB, msgA, msgC, msgD], [msgA, msgB, msgC])
        ]
    ]


testEx05 :: TestTree
testEx05 = testGroup "Exercise 5 - log ordering and filtering"
    [ param1 "whatWentWrong" (whatWentWrong . parse)
        [ (input, expected) ]
    , param1 "severe" severe
        [ (msgA, False)
        , (msgB, False)
        , (msgC, False)
        , (msgD, False)
        , (LogMessage (Error 50) 2 "hi", True)
        ]
    , param1 "messageBody" messageBody
        [ (LogMessage (Error 50) 2 "hi", "hi")
        , (LogMessage Info 2 "I love Haskell", "I love Haskell")
        ]
    ]
    where
        input = unlines
            [ "I 6 Completed armadillo processing"
            , "I 1 Nothing to report"
            , "E 99 10 Flange failed!"
            , "I 4 Everything normal"
            , "I 11 Initiating self-destruct sequence"
            , "E 70 3 Way too many pickles"
            , "E 65 8 Bad pickle-flange interaction detected"
            , "W 5 Flange is due for a check-up"
            , "I 7 Out for lunch, back in two time steps"
            , "E 20 2 Too many pickles"
            , "I 9 Back from lunch"
            ]
        expected =
            [ "Way too many pickles"
            , "Bad pickle-flange interaction detected"
            , "Flange failed!"
            ]
