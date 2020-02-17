{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}
module LogAnalysis where

import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import Text.Read (readMaybe)

import Log

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
