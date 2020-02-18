module Main where

import System.Environment

import Log
import LogAnalysis

main :: IO ()
main = do
    args <- getArgs
    contents <- mapM getLines args
    mapM_ putStrLn (concat contents)

getLines :: FilePath -> IO [String]
getLines path = do
    logLines <- testWhatWentWrong parse whatWentWrong path
    return $ "" : ("Log File: " ++ path) : logLines
