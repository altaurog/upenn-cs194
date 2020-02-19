module TestHomework02 where

import Test.Tasty

import TestUtil

import Log
import LogAnalysis

tests :: TestTree
tests = testGroup "Homework 02"
    [ testEx01
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
