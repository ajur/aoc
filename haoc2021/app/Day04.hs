-- Advent Of Code 2021 Day 4
-- https://adventofcode.com/2021/day/4

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools

main :: IO ()
main = do
    inputData <- readInput sampleData
    
    let draws = parseInput inputData
    
    putStr . unlines . map (\x -> (show x) ++ " ") $ take 10 input
    putStrLn "..."

    putStrLn "--- part 1"
    -- putStrLn . show . 

    putStrLn "--- part 2"
    -- putStrLn . show . 

parseInput :: String -> ([Int], [[Int]])
parseInput = (readDraws x, readBoards xs)
    where 
        (x:xs) = splitOn "\n\n"
        readDraws = map read . splitOn ","
        readBoards = map (map (map readInt . words) . lines)

-- TODO

sampleData :: String
sampleData = "\
    \7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
    \\n\
    \22 13 17 11  0\n\
    \ 8  2 23  4 24\n\
    \21  9 14 16  7\n\
    \ 6 10  3 18  5\n\
    \ 1 12 20 15 19\n\
    \\n\
    \ 3 15  0  2 22\n\
    \ 9 18 13 17  5\n\
    \19  8  7 25 23\n\
    \20 11 10 24  4\n\
    \14 21 16 12  6\n\
    \\n\
    \14 21 17 24  4\n\
    \10 16 15  9 19\n\
    \18  8 23 26 20\n\
    \22 11 13  6  5\n\
    \ 2  0 12  3  7"
