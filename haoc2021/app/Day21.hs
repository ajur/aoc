{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
-- Advent Of Code 2021 Day 21
-- https://adventofcode.com/2021/day/21

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools
import Control.Monad.Memo (for4, memo, MonadMemo, startEvalMemo)
import Control.Monad (forM)
import Debug.Trace (traceShowM, traceM)

main :: IO ()
main = do
    inputData <- readInput sampleData

    let [p1s, p2s] = parseInput inputData

    putStrLn "--- part 1"
    print $ sol1 p1s p2s

    putStrLn "--- part 2"
    print $ sol2 (fromIntegral p1s) (fromIntegral p2s)


sol1 :: Int -> Int -> Int
sol1 start1 start2
    | w1i <= w2i = (p2 !! (w1i-1)) * (w1i * 6 + 3)
    | otherwise  = (p1 !! w2i) * (w2i * 6 + 6)
    where
        p1 = points draws1 start1
        p2 = points draws2 start2
        w1i = wonAfter p1
        w2i = wonAfter p2


wonAfter :: [Int] -> Int
wonAfter = fromJust . findIndex (>=1000)

points :: ([Int] -> [[Int]]) -> Int -> [Int]
points draws startPos = scanl1 (+) . map board . tail . scanl (+) startPos . map sum . draws $ dice100

dice100 :: [Int]
dice100 = cycle [1..100]

board :: Integral a => a -> a
board n = 1+((n-1) `mod` 10)

draws1 :: [a] -> [[a]]
draws1 [] = []
draws1 xs = take 3 xs : draws1 (drop 6 xs)
draws2 :: [a] -> [[a]]
draws2 [] = []
draws2 xs = take 3 (drop 3 xs) : draws2 (drop 6 xs)


parseInput :: String -> [Int]
parseInput = map (read . last . words) . lines . trim

sd = parseInput sampleData

sampleData :: [Char]
sampleData = [r|
Player 1 starting position: 4
Player 2 starting position: 8
|]

-- below is code with some fancy memoization that i merelly understand at this point xD

sol2 :: Integer -> Integer -> Integer
sol2 pos1 pos2 = max p1wins p2wins
    where (p1wins, p2wins) = evalGoM pos1 0 pos2 0

test = let w@(a,b) = evalGoM 4 0 8 0 in (w == (444356092776315, 341960390180808), w, 444356092776315+341960390180808 - a -b)

evalGoM :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
evalGoM a b c d = startEvalMemo $ goM a b c d

goM :: MonadMemo (Integer, Integer, Integer, Integer) (Integer, Integer) m => Integer -> Integer -> Integer -> Integer -> m (Integer, Integer)
goM pos1 pts1 pos2 pts2 = do
    results <- forM (drawResult pos1 pts1) (\(npos, npts) -> do
        if npts >= 21 then
            return (1,0)
        else do
            resp <- for4 memo goM pos2 pts2 npos npts
            return (swap resp)
        )
    return $ sumWins results

drawResult :: Integer -> Integer -> [(Integer, Integer)]
drawResult pos pts = map (\p -> let npos = board (pos + p) in (npos, pts+npos)) unisDraws

unisDraws :: [Integer]
unisDraws = concat . concat $ [[[a+b+c | a <- l] | b <- l] | c <- l]
    where l = [1..3]

sumWins :: [(Integer, Integer)] -> (Integer, Integer)
sumWins = foldl1 sumPairs

sumPairs :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
sumPairs (a,b) (c,d) = (a+c, b+d)

-- for fun, leaving here basic, not memoized version of second solution
go :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
go pos1 pts1 pos2 pts2
    | npts1 >= 21 = (1,0)
    | otherwise  = swap . sumWins . map mepper $ unisDraws
    where
        npts1 = pts1 + pos1
        mepper :: Integer -> (Integer, Integer)
        mepper d2 = go (board (pos2 + d2)) pts2 pos1 npts1

