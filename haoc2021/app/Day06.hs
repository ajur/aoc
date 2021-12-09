-- Advent Of Code 2021 Day 6
-- https://adventofcode.com/2021/day/6

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools

main :: IO ()
main = do
    inputData <- readInput "3,4,3,1,2"

    let xs = parseInput inputData
    
    putStrLn "--- part 1"
    -- putStrLn . show . length . growFish 80 $ xs  -- watch out! unefficient!
    putStrLn . show . countAllFishes 80 $ xs

    putStrLn "--- part 2"
    putStrLn . show . countAllFishes 256 $ xs


parseInput :: String -> [Int]
parseInput = map read . wordsOn ','


wordsOn :: Char -> String -> [String]
wordsOn c s  =  case dropWhile (==c) s of
    "" -> []
    s' -> w : wordsOn c s''
        where (w, s'') = break (==c) s'


decrementWithReset :: (Eq p, Num p) => p -> p -> p
decrementWithReset x 0 = x
decrementWithReset x y = y - 1

growFish :: Int -> [Int] -> [Int]
growFish _ [] = []
growFish 0 xs = xs
growFish n xs = growFish (n-1) $ oldFish ++ newFish
    where dec = decrementWithReset 6
          countNew = length . filter (==0)
          oldFish = map dec xs
          newFish = replicate (countNew xs) 8



countAllFishes :: Int -> [Int] -> Integer
countAllFishes n = sum . map (\k -> fishes (n + 6 - k))

fishes :: Int -> Integer
fishes = (map f [0..] !!)
    where f n | n < 7  = 1
              | n >= 7 = (+1) . sum $ map (fishes . nk) [1..k]
              where 
                  k = n `div` 7
                  nk kk = (n - 2 - 7 * kk) `max` 0
