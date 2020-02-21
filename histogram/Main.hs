module Main where

import Data.Maybe
import System.Environment
import Text.Read

import Golf

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ histogram $ catMaybes $ map readMaybe args
