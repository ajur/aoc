-- Advent Of Code 2021 Day 11
-- https://adventofcode.com/2021/day/11

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools

import qualified Data.Matrix as Mx

main :: IO ()
main = do
    inputData <- readInput sampleData
    
    let ls = lines inputData
    
    putStrLn "--- sample"
    putStrLn . show . parseInput $ sampleData

    putStrLn "--- part 1"
    -- putStrLn . show . 

    putStrLn "--- part 2"
    -- putStrLn . show . 


neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x,y) = [(x+xx,y+yy) | xx <- [-1..1], yy <- [-1..1]] \\ [(x,y)]

getDefault :: Mx.Matrix c -> c -> (Int, Int) -> c
getDefault m d (r, c) = maybe d id . Mx.safeGet r c $ m

parseInput :: String -> Mx.Matrix Int
parseInput = Mx.fromLists . map (map digitToInt) . lines

sampleData :: String
sampleData = "\
\5483143223\n\
\2745854711\n\
\5264556173\n\
\6141336146\n\
\6357385478\n\
\4167524645\n\
\2176841721\n\
\6882881134\n\
\4846848554\n\
\5283751526"
