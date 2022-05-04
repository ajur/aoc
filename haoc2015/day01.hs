{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List (elemIndex, sort)
import Data.List.Split (splitOn)



main = do
    putStrLn "Advent of Code 2015"

    d01 <- readFile "data/d01"
    putStrLn $ "day 01 | " ++ show (foldl countFloors 0 d01) ++ " | " ++ (show . elemIndex (-1) $ scanl countFloors 0 d01)

    d02 <- map (map readInt . splitOn "x") . lines <$> readFile "data/d02"
    let sAreas = map $ \dims -> let sa = sort . surfaceArea $ dims in head sa + 2 * sum sa
    let ribons = map (\a -> cubeVolume a + smallestPerimeter a)
    putStrLn $ "day 02 | " ++ (show . sum . sAreas $ d02) ++ " | " ++ (show . sum . ribons $ d02)


countFloors :: Num a => a -> Char -> a
countFloors acc '(' = acc + 1
countFloors acc ')' = acc - 1

surfaceArea :: Num a => [a] -> [a]
surfaceArea [w,h,l] = [w*h,w*l,h*l]
cubeVolume :: Num a => [a] -> a
cubeVolume = product
smallestPerimeter :: (Num a, Ord a) => [a] -> a
smallestPerimeter [w,h,l] = 2 * minimum [w+h, w+l, h+l]

readInt :: String -> Int
readInt = read
