-- Advent Of Code 2021 Day 3
-- https://adventofcode.com/2021/day/3

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools

main :: IO ()
main = do
    inputData <- readInput sampleData
    
    let (points,folds) = parseInput inputData
    
    putStrLn "--- sample"
    let (sPoints, sFolds) = parseInput sampleData
    putStrLn . sheetStr $ sPoints
    putStrLn . sheetStr $ foldAll sPoints sFolds

    putStr "points: "
    putStrLn . show . fst . parseInput $ sampleData
    putStr "folds:  "
    putStrLn . show . snd . parseInput $ sampleData

    putStrLn "--- part 1"
    putStrLn . show . length . foldSheet (head folds) $ points

    putStrLn "--- part 2"
    putStrLn . sheetStr $ foldAll points folds


foldAll points folds = foldl (flip foldSheet) points folds

foldSheet :: (String, Int) -> [(Int, Int)] -> [(Int, Int)]
foldSheet ("x", n) = nub . map (foldPointOnX n)
foldSheet ("y", n) = nub . map (foldPointOnY n)

foldPointOnY :: Int -> (Int, Int) -> (Int, Int)
foldPointOnY n (x,y)
    | y <= n = (x,y)
    | y > n  = (x, foldNum n y)

foldPointOnX :: Int -> (Int, Int) -> (Int, Int)
foldPointOnX n (x,y)
    | x <= n = (x,y)
    | x > n  = (foldNum n x, y)

foldNum :: Int -> Int -> Int
foldNum k x = 2 * k - x

parseInput :: String -> ([(Int, Int)],[(String, Int)])
parseInput s = (points, folds)
    where 
        [pl,fl] = splitOn "\n\n" s
        points = linesOf pairOfInts pl
        foldSpec s = let [_,_,x,y] = splitOneOf " =" s in (x, (read y ::Int))
        folds = linesOf foldSpec fl


sheetStr :: [(Int, Int)] -> String
sheetStr points = unlines [[c x y | x <- [0..w]] | y <- [0..h]]
    where
        w = maximum . map fst $ points
        h = maximum . map snd $ points
        c x y = if (x,y) `elem` points then '#' else ' '

sampleData :: String
sampleData = "\
\6,10\n\
\0,14\n\
\9,10\n\
\0,3\n\
\10,4\n\
\4,11\n\
\6,0\n\
\6,12\n\
\4,1\n\
\0,13\n\
\10,12\n\
\3,4\n\
\3,0\n\
\8,4\n\
\1,10\n\
\2,14\n\
\8,10\n\
\9,0\n\
\\n\
\fold along y=7\n\
\fold along x=5"
