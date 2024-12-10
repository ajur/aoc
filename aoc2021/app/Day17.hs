
-- Advent Of Code 2021 Day 17
-- https://adventofcode.com/2021/day/17

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools

import Text.ParserCombinators.ReadP


main :: IO ()
main = do
    inputData <- readInput sampleData
    let input = parseInput inputData

    putStrLn "--- part 1"
    putStrLn . show . sol1 $ input

    putStrLn "--- part 2"
    putStrLn . show . sol2 $ input

type TargetDimensions = ((Int, Int), (Int, Int))

sol1 :: TargetDimensions -> ((Int, Int), Int)
sol1 ((x0,_), (y0,_)) = ((minVx, maxVy), maxY)
    where
        maxVy = findMaxVy y0     -- minVx is just minY
        minVx = findMinVx x0   -- maxVx is just maxX
        maxY = sumN maxVy

-- sol2 :: TargetDimensions -> Int
sol2 td@((minX,maxX),(minY,maxY)) = length $ nub joint   -- dunno why nub.. but whateva, i dont want to go into it anymore -_-
    where                                                -- should have done brute force simulation search in min/max
        minVx = findMinVx minX
        maxVx = maxX
        minVy = minY
        maxVy = findMaxVy minY
        allHitsX = findHitsAndGroupByIteration td minVx maxVx findItersHitOnVx0
        allHitsY = findHitsAndGroupByIteration td minVy maxVy findItersHitOnVy0
        matchingIters = intersect (map fst allHitsX) (map fst allHitsY)
        filterMatchingIters = sortOn fst . filter ((`elem` matchingIters) . fst)
        joint = concat $ zipWith (allCombinations `on` snd) (filterMatchingIters allHitsX) (filterMatchingIters allHitsY)


allCombinations :: [a] -> [b] -> [(a, b)]
allCombinations xs ys = [(x,y) | x <- xs, y <- ys]

findHitsAndGroupByIteration :: TargetDimensions -> Int -> Int -> (TargetDimensions -> Int -> [Int]) -> [(Int, [Int])]
findHitsAndGroupByIteration td min max fIHOV = regroupByIter allHits
    where
        allHits = concatMap (pairAllWithArg $ fIHOV td) [min .. max]
        regroupByIter = map (foldl (\(_,ns) (n,v) -> (v,n:ns)) (0,[])) . groupBy ((==) `on` snd) . sortOn snd

pairAllWithArg :: (a -> [b]) -> a -> [(a, b)]
pairAllWithArg f = \x -> map (\y -> (x, y)) $ f x

-- so... it takes target dimensions and some initial Vx and finds all iterations when x is in range of target minX and maxX
findItersHitOnVx0 :: TargetDimensions -> Int -> [Int]
findItersHitOnVx0 ((minX, maxX), (minY, _)) = map fst . dropWhile ((< minX) . snd) . zip [1..] . takeWhile (<= maxX) . take (maxItersForTarget minY) . scanl1 (+) . iterate nextVx

-- basically same as Vx
findItersHitOnVy0 :: TargetDimensions -> Int -> [Int]
findItersHitOnVy0 (_, (minY, maxY)) = map fst . dropWhile ((> maxY) . snd) . zip [1..] . takeWhile (>= minY) . take (maxItersForTarget minY) . scanl1 (+) . iterate pred

nextVx :: Int -> Int
nextVx vx
    | vx == 0 = 0
    | vx < 0  = vx + 1
    | vx > 0  = vx - 1

nextVy :: Int -> Int
nextVy = pred

-- as vx is series going to 0, find first N so that sum of elements is within target range
-- note, maxVx is just maxX
findMinVx :: Int -> Int
findMinVx minX = fromJust . findIndex (>= minX) . scanl1 (+) $ [0..]

-- Vy has const acc, thus after initial going up, it hits y=0 with Vy = -V0, thus next hit will be on y = -V0 - 1
--    and as it should be minY, minY = -V0 - 1 => V0 = -1 - minY
-- note, minVy is just minY
findMaxVy :: Int -> Int
findMaxVy minY = (-1) - minY

-- max possible iterations depends only on max Vy, that is basically almost same as minimum target y
maxItersForTarget :: Int -> Int
maxItersForTarget minY = 2 * abs minY

sumN :: Int -> Int
sumN n = (n+1) * n `div` 2

pair :: [a] -> (a,a)
pair [x,y] = (x,y)

parseInput :: String -> TargetDimensions
parseInput = pair . map (pair . map read . splitOn "..") . splitOn ", y=" . drop 15

sampleData :: String
sampleData = "target area: x=20..30, y=-10..-5"

