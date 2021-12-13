module Tools (
    module Data.List,
    module Data.Char,
    module Data.List.Split,
    readInput, asPair, readInts, pairOfInts, linesOf
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

asPair :: [a] -> (a,a)
asPair [x,y] = (x,y)

readInts :: String -> [Int]
readInts = map read . splitOneOf ";,| "

pairOfInts :: String -> (Int, Int)
pairOfInts = asPair . readInts

linesOf :: (String -> a) -> String -> [a]
linesOf f = map f . lines