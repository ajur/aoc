-- Advent Of Code 2021 Day 3
-- https://adventofcode.com/2021/day/3

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools

import qualified Data.MultiMap as MMap
import qualified Data.Set as Set

main :: IO ()
main = do
    inputData <- readInput sample3
    
    let input = parseInput inputData
    
    putStrLn "--- sample1"
    putStrLn . show . MMap.toMap . parseInput $ sample1
    putStrLn "--- sample2"
    putStrLn . show . MMap.toMap . parseInput $ sample2
    putStrLn "--- sample3"
    putStrLn . show . MMap.toMap . parseInput $ sample3

    putStrLn "--- part 1"
    putStrLn ("samples: " ++ (show . map (countPaths isDeadEnd . parseInput) $ [sample1, sample2, sample3]) ++ " ought to be " ++ show [10, 19, 226])
    putStrLn . show . countPaths isDeadEnd $ input

    putStrLn "--- part 2"
    putStrLn ("samples: " ++ (show . map (countPaths isDeadEndDespiteRevisit . parseInput) $ [sample1, sample2, sample3]) ++ " ought to be " ++ show [36, 103, 3509])
    putStrLn . show . countPaths isDeadEndDespiteRevisit $ input


countPaths deadEndCheck nodes = length $ findSubPaths deadEndCheck nodes [] "start"

findSubPaths p nodes path node
    | isStartAgain  = []
    | node == "end" = [reverse (node:path)]
    | p path node   = []
    | otherwise     = concatMap (findSubPaths p nodes (node:path)) $ nodes MMap.! node
    where isStartAgain = node == "start" && nodeVisited node path

isDeadEnd p n = isSmallCave n && nodeVisited n p
isDeadEndDespiteRevisit p n = isSmallCave n && nodeVisited n p && someSmallCaveAlreadyVisitedTwice p

isSmallCave = all isLower
nodeVisited n p = n `elem` p
someSmallCaveAlreadyVisitedTwice p = length onlySmall /= (length . Set.fromList $ onlySmall)
    where onlySmall = filter (not . isUpper . head) p


parseInput :: String -> MMap.MultiMap String String
parseInput = MMap.fromList . concatMap toBiDirPair . map (splitOn "-") . lines
    where
        toBiDirPair [x,y] = [(x,y),(y,x)]


sample1 = "\
\start-A\n\
\start-b\n\
\A-c\n\
\A-b\n\
\b-d\n\
\A-end\n\
\b-end"

sample2 = "\
\dc-end\n\
\HN-start\n\
\start-kj\n\
\dc-start\n\
\dc-HN\n\
\LN-dc\n\
\HN-end\n\
\kj-sa\n\
\kj-HN\n\
\kj-dc"

sample3 = "\
\fs-end\n\
\he-DX\n\
\fs-he\n\
\start-DX\n\
\pj-DX\n\
\end-zg\n\
\zg-sl\n\
\zg-pj\n\
\pj-he\n\
\RW-he\n\
\fs-DX\n\
\pj-RW\n\
\zg-RW\n\
\start-pj\n\
\he-WI\n\
\zg-he\n\
\pj-fs\n\
\start-RW"
