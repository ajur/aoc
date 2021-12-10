module Tools (
    module Data.List,
    module Data.Char,
    module Data.List.Split,
    readInput
) where

import System.Environment
import Data.Char
import Data.List
import Data.List.Split

readInput :: String -> IO String
readInput x = do
    args <- getArgs
    case args of
        [file] -> do
            contents <- readFile file
            return contents
        _ -> return x

