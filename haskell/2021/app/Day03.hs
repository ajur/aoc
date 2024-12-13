-- Advent Of Code 2021 Day 3
-- https://adventofcode.com/2021/day/3

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools

main :: IO ()
main = do
    inputData <- readInput sampleData
    
    let input = parseInput inputData
    
    putStrLn "--- sample data"
    putStrLn . show . parseInput $ sampleData

    putStrLn "--- part 1"
    putStrLn . show $ (toBitsNum highBit input * toBitsNum lowBit input)

    putStrLn "--- part 2"
    putStrLn . show $ ((bitsToInt . findLastStanding highBit $ input) * (bitsToInt . findLastStanding lowBit $ input))


findLastStanding :: Eq b => ([b] -> b) -> [[b]] -> [b]
findLastStanding f rs = filterOnCol f 0 rs
    where
        filterOnCol f c (x:[]) = x
        filterOnCol f c xs = filterOnCol f (succ c) $ filterByColBit f c xs

filterByColBit :: Eq b => ([b] -> b) -> Int -> [[b]] -> [[b]]
filterByColBit f c rs = filter (hasFBit c) rs
    where
        colFBit x = f . getCol x $ rs
        hasFBit x row = (row !! x) == colFBit x

toBitsNum :: ([b] -> Int) -> [[b]] -> Int
toBitsNum f rs = bitsToInt . map fOnCol . take (length . head $ rs) $ [0..]
    where fOnCol c = f . getCol c $ rs

getCol :: Int -> [[b]] -> [b]
getCol n = map (!!n)

bitsToInt :: [Int] -> Int
bitsToInt = foldl1 (\acc x -> acc * 2 + x)

highBit :: (Integral c, Num a, Eq a) => [a] -> c
highBit = floor . (\xs -> onesInList xs / genericLength xs + 0.5)

lowBit :: (Integral c, Num a, Eq a) => [a] -> c
lowBit = (1-) . highBit

onesInList :: (Eq a, Num c, Num a) => [a] -> c
onesInList = genericLength . filter (==1)

parseInput :: String -> [[Int]]
parseInput = map (map digitToInt) . lines

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
