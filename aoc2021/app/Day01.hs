-- Advent Of Code 2021 Day 1
-- https://adventofcode.com/2021/day/1

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools

main :: IO ()
main = do
    inputData <- readInput sampleData
    
    let ls = parseLines $ lines inputData
    
    putStrLn "--- part 1"
    putStrLn $ show (simpleSonar ls)

    putStrLn "--- part 2"
    putStrLn $ show (betterSonar ls)



parseLines :: [String] -> [Int]
parseLines = map read


simpleSonar :: [Int] -> Int
simpleSonar []  = 0
simpleSonar (_:[]) = 0
simpleSonar (x:y:ys) = incrementIfIncreased x y (simpleSonar (y:ys))


incrementIfIncreased :: (Ord a, Enum b) => a -> a -> b -> b
incrementIfIncreased x y n
    | y > x     = succ n
    | otherwise = n


betterSonar :: [Int] -> Int
betterSonar xs = simpleSonar (windows xs)


windows :: [Int] -> [Int]
windows [] = []
windows (_:[]) = []
windows (_:_:[]) = []
windows (x:y:z:zs) = (x + y + z) : windows (y:z:zs)

sampleData :: String
sampleData = "\
    \199\n\
    \200\n\
    \208\n\
    \210\n\
    \200\n\
    \207\n\
    \240\n\
    \269\n\
    \260\n\
    \263"
