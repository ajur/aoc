-- Advent Of Code 2021 Day 11
-- https://adventofcode.com/2021/day/11

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools

import qualified Data.Matrix as Mx
import qualified Data.Vector as V

main :: IO ()
main = do
    inputData <- readInput sampleData
    
    let mxIn = parseInput inputData
    
    putStrLn "--- sample"
    putStrLn . show . parseInput $ sampleData

    putStrLn "--- part 1"
    putStrLn . show $ blinksAfterNProgresses 100 mxIn

    putStrLn "--- part 2"
    putStrLn . show $ blinkUntillAllFlash mxIn


blinksAfterNProgresses :: Int -> Mx.Matrix Int -> Int
blinksAfterNProgresses nIn mxIn = go nIn mxIn 0
    where
        go 0 mx count = count
        go n mx count = go (n-1) mxUp (count + length bs)
            where (mxUp, bs) = progressLevels mx


blinkUntillAllFlash :: Mx.Matrix Int -> Int
blinkUntillAllFlash mxIn = go 1 mxIn
    where
        nMax = Mx.nrows mxIn * Mx.ncols mxIn
        allFlashed bs = nMax == length bs
        go n mx
            | allFlashed bs = n
            | otherwise     = go (n+1) mxUp
            where (mxUp, bs) = progressLevels mx

progressLevels :: Mx.Matrix Int -> (Mx.Matrix Int, [(Int, Int)])
progressLevels mxIn = (let mxUp = fmap fUp mxIn in go mxUp [] . mxFilter (>9) $ mxUp)
    where
        fUp = (+1)
        blinkable mx bs p = (mxTest (>9) mx p) && (not (p `elem` bs))
        go mx bs [] = (mxUpdateAll (const 0) mx bs, bs)
        go mx bs (p:ps)
            | blinkable mx bs p = go (mxUpdateAll fUp mx nb) (p:bs) (ps ++ nb)
            | otherwise         = go mx bs ps
            where nb = neighbours mx p

mxTest :: (c -> Bool) -> Mx.Matrix c -> (Int, Int) -> Bool
mxTest p mx (i,j) = p $ Mx.unsafeGet i j mx

mxUpdateAll :: (c -> c) -> Mx.Matrix c -> [(Int, Int)] -> Mx.Matrix c
mxUpdateAll f = foldl (mxUpdate f)

mxUpdate :: (c -> c) -> Mx.Matrix c -> (Int, Int) -> Mx.Matrix c
mxUpdate f mx (i,j) = Mx.unsafeSet (f $ Mx.unsafeGet i j mx) (i, j) mx

mxFilter :: (c -> Bool) -> Mx.Matrix c -> [(Int, Int)]
mxFilter p mx = [ (i,j) | i <- [1 .. Mx.nrows mx] , j <- [1 .. Mx.ncols mx], let v = Mx.unsafeGet i j mx, p v ]

neighbours :: Mx.Matrix Int -> (Int, Int) -> [(Int, Int)]
neighbours mx p0 = [p | p@(i,j) <- adjecentPositions p0, inRange 1 rows i, inRange 1 cols j]
    where
        inRange x1 x2 x = x1 <= x && x <= x2
        rows = Mx.nrows mx
        cols = Mx.ncols mx

adjecentPositions :: (Int, Int) -> [(Int, Int)]
adjecentPositions (x,y) = [(x+xx,y+yy) | xx <- [-1..1], yy <- [-1..1]] \\ [(x,y)]

parseInput :: String -> Mx.Matrix Int
parseInput = Mx.fromLists . map (map digitToInt) . lines

sampleData :: String
sampleData = "\
\5483143223\n\
\2745854711\n\
\5264556173\n\
\6141336146\n\
\6357385478\n\
\4167524645\n\
\2176841721\n\
\6882881134\n\
\4846848554\n\
\5283751526"
