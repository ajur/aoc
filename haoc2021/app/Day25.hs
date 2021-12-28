{-# LANGUAGE QuasiQuotes #-}
-- Advent Of Code 2021 Day 25
-- https://adventofcode.com/2021/day/25

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools
import qualified Data.Matrix as Mx

main :: IO ()
main = do
    inputData <- readInput sampleData

    let seaMap = parseInput inputData

    putStrLn "--- part 1"
    print $ countStepsToEq seaMap

    putStrLn "--- part 2"
    --print $ sol2 actions

data Cucumber = East | South | Empty deriving (Show, Eq)

type SeaMap = Mx.Matrix Cucumber

showSeaMap :: SeaMap -> String
showSeaMap = unlines . Mx.toLists . fmap c2c
    where
        c2c East = '>'
        c2c South = 'v'
        c2c Empty = '.'

charToCucumber :: Char -> Cucumber
charToCucumber '>' = East
charToCucumber 'v' = South
charToCucumber '.' = Empty
charToCucumber _   = error "unknown Cucumber"

getNextEast mx (r,c) = mx Mx.! (r, wrap 1 (Mx.ncols mx) (c + 1))
getPrevEast mx (r,c) = mx Mx.! (r, wrap 1 (Mx.ncols mx) (c - 1))
getNextSouth mx (r,c) = mx Mx.! (wrap 1 (Mx.nrows mx) (r + 1), c)
getPrevSouth mx (r,c) = mx Mx.! (wrap 1 (Mx.nrows mx) (r - 1), c)

countStepsToEq :: SeaMap -> Int
countStepsToEq mx0 = let mx1 = stepCucumbers mx0 in 1 + (if mx0 == mx1 then 0 else countStepsToEq mx1)

stepCucumbers :: SeaMap -> SeaMap
stepCucumbers = stepAllSouth . stepAllEast

stepAllEast :: SeaMap -> SeaMap
stepAllEast mx = Mx.mapPos stepEast mx
    where
        stepEast p East = if Empty == getNextEast mx p then Empty else East
        stepEast p Empty = if East == getPrevEast mx p then East else Empty
        stepEast _ c = c

stepAllSouth :: SeaMap -> SeaMap
stepAllSouth mx = Mx.mapPos stepSouth mx
    where
        stepSouth p South = if Empty == getNextSouth mx p then Empty else South
        stepSouth p Empty = if South == getPrevSouth mx p then South else Empty
        stepSouth _ c = c


wrap :: Integral a => a -> a -> a -> a
wrap n0 n1 n = n0 + (n-n0) `mod` (n1-n0+1)

parseInput :: String -> SeaMap
parseInput = fmap charToCucumber . Mx.fromLists . lines . trim

sampleData :: String
sampleData = [r|
v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>
|]

sd = parseInput sampleData