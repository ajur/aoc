module Tools (
    module Data.Function,
    module Data.Maybe,
    module Data.Tuple,
    module Data.List,
    module Data.Char,
    module Data.List.Split,
    module Text.RawString.QQ,
    readInput, asPair, asTriplet, readInts, pairOfInts, linesOf, b2i, bs2i, trim
) where

import System.Environment ( getArgs )
import Data.Maybe
import Data.Tuple
import Data.Char
import Data.List
import Data.List.Split
import Data.Function
import Text.RawString.QQ

readInput :: String -> IO String
readInput x = do
    args <- getArgs
    case args of
        [file] -> readFile file
        _ -> return x

asPair :: [a] -> (a,a)
asPair [x,y] = (x,y)
asPair _     = error "Not two element list"

asTriplet :: [a] -> (a,a,a)
asTriplet [x,y,z] = (x,y,z)
asTriplet _       = error "Not three element list"

readInts :: String -> [Int]
readInts = map read . splitOneOf ";,| "

pairOfInts :: String -> (Int, Int)
pairOfInts = asPair . readInts

linesOf :: (String -> a) -> String -> [a]
linesOf f = map f . lines

b2i :: [Int] -> Int
b2i = foldl1 (\acc x -> acc * 2 + x)

bs2i :: String -> Int
bs2i = b2i . map digitToInt

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
