-- Advent Of Code 2021 Day 3
-- https://adventofcode.com/2021/day/3

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools

main :: IO ()
main = do
    inputData <- readInput sampleData
    
    let input = parseInput inputData
    
    putStr . unlines . map (\x -> (show x) ++ " ") $ take 10 input
    putStrLn "..."

    putStrLn "--- part 1"
    -- putStrLn . show . 

    putStrLn "--- part 2"
    -- putStrLn . show . 

parseInput :: String -> [String]
parseInput = lines

sampleData :: String
sampleData = "\
    \00100\n\
    \11110\n\
    \10110\n\
    \10111\n\
    \10101\n\
    \01111\n\
    \00111\n\
    \11100\n\
    \10000\n\
    \11001\n\
    \00010\n\
    \01010"
