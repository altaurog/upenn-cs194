{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}
module LogAnalysis where

import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import Text.Read (readMaybe)

import Log

-- Exercise 1
parse :: String -> [LogMessage]
parse = lines >>> (map parseMessage)

parseMessage :: String -> LogMessage
parseMessage logMessage =
  case maybeMessageParts $ words logMessage of
    Just (msgType, timestamp, body) -> LogMessage msgType timestamp body
    _ -> Unknown logMessage


maybeMessageParts :: [String] -> Maybe (MessageType, TimeStamp, String)
maybeMessageParts xs = do
  (msgType, msg) <- parseMessageType xs
  (timestamp, body) <- parseTimestamp msg
  Just (msgType, timestamp, unwords body)


parseMessageType :: [String] -> Maybe (MessageType, [String])
parseMessageType ("I":xs) = Just (Info, xs)
parseMessageType ("W":xs) = Just (Warning, xs)
parseMessageType ("E":x:xs) =
  (readMaybe x) <&> (Error >>> (,xs))
parseMessageType _ = Nothing


parseTimestamp :: [String] -> Maybe (TimeStamp, [String])
parseTimestamp (x:xs) = (readMaybe x) <&> (,xs)
parseTimestamp _ = Nothing

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert a tree@(Node left b right) =
  case compareLog a b of
    Just GT -> Node left b (insert a right)
    Nothing -> tree
    _ -> Node (insert a left) b right
insert (Unknown _) Leaf = Leaf
insert a Leaf = Node Leaf a Leaf


compareLog :: LogMessage -> LogMessage -> Maybe Ordering
compareLog (LogMessage _ a _) (LogMessage _ b _) = Just $ compare a b
compareLog _ _ = Nothing


-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf


-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = (inOrder left) ++ (m : (inOrder right))


-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
  filter severe
  >>> build
  >>> inOrder
  >>> map messageBody


severe :: LogMessage -> Bool
severe (LogMessage (Error level) _ _) = level >= 50
severe _ = False


messageBody :: LogMessage -> String
messageBody (LogMessage _ _ body) = body
messageBody (Unknown body) = body
