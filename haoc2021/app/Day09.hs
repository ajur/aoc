-- Advent Of Code 2021 Day 9
-- https://adventofcode.com/2021/day/9

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools

import qualified Data.Matrix as Matrix
import Data.Char

main :: IO ()
main = do
    inputData <- readInput sampleData
    
    let input = parseInput inputData
    
    putStrLn "--- sample"
    putStrLn . show . parseInput $ sampleData
    -- putStrLn . unwords . ($input) $ [show . Matrix.nrows, show . Matrix.ncols, show . (Matrix.!(1,1))]

    putStrLn "--- part 1"
    putStrLn . show . sum . map succ . localMinsVals $ input

    putStrLn "--- part 2"
    putStrLn . show . product . take 3 . reverse . sort . map (length . localMinBasin input) . localMins $ input



localMinBasin :: (Eq a, Num a) => Matrix.Matrix a -> (Int, Int) -> [(Int, Int)]
localMinBasin m p = growBasin [p] [] []
    where
        growBasin [] basinPs _ = basinPs
        growBasin (p:ps) basinPs checkedPs
            | pv == 9   = growBasin ps basinPs (p:checkedPs)
            | otherwise = growBasin toCheck (p:basinPs) (p:checkedPs)
            where 
                pv = getDefault m 9 p
                toCheck = (++ps) . (\\ps) . (\\checkedPs) . adjecentPositions $ p

localMinsVals :: (Ord a, Num a) => Matrix.Matrix a -> [a]
localMinsVals m = map (m Matrix.!) . localMins $ m

localMins :: (Ord a, Num a) => Matrix.Matrix a -> [(Int, Int)]
localMins m = filter (isLocalMin m) .  Matrix.toList . Matrix.mapPos (\p a -> p) $ m

isLocalMin :: (Ord b, Num b) => Matrix.Matrix b -> (Int, Int) -> Bool
isLocalMin m p = all (isMoreThanPos m p) . map (getOr9 m) . adjecentPositions $ p
    where getOr9 m = getDefault m 9
          isMoreThanPos m x = (>(getOr9 m x))

adjecentPositions :: (Num a, Num b) => (a, b) -> [(a, b)]
adjecentPositions x = map (addPos x) [(-1,0), (1,0), (0,-1), (0,1)]


addPos :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

getDefault :: Matrix.Matrix c -> c -> (Int, Int) -> c
getDefault m d (r, c) = maybe d id . Matrix.safeGet r c $ m

parseInput :: String -> Matrix.Matrix Int
parseInput = Matrix.fromLists . map (map digitToInt) . lines

sampleData :: String
sampleData = "\
    \2199943210\n\
    \3987894921\n\
    \9856789892\n\
    \8767896789\n\
    \9899965678"
