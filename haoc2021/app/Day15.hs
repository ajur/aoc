-- Advent Of Code 2021 Day 15
-- https://adventofcode.com/2021/day/15

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools
import qualified Data.Map as M
import qualified Data.Matrix as Mx

import Debug.Trace

main :: IO ()
main = do
    inputData <- readInput sampleData
    
    let inputMx = parseInput inputData
    
    putStrLn "--- sample"
    putStrLn . show $ parseInput sampleData
    
    putStrLn "--- part 1"
    putStrLn . show $ dijkstraFind inputMx (1,1) (getEnd inputMx)

    putStrLn "--- part 2"
    let blownInputMx = blowInputMx 5 inputMx
    putStrLn . show $ dijkstraFind blownInputMx (1,1) (getEnd blownInputMx)


-- dijkstra
-- probably could be faster, with faster data or something

dijkstraFind :: Mx.Matrix Int -> (Int, Int) -> (Int, Int) ->Int
dijkstraFind mx start end = snd $ go M.empty [(start, 0)]
    where
        newNeighbours fm n = addValToAll n . withoutFound fm . neighbours mx $ n
        go _ [] = error "cannot find end node"
        go fm (n:npq)
            -- | trace ("n = " ++ show n) False = undefined
            | fst n == end = n
            | M.member (fst n) fm = go fm npq
            | otherwise = go (addToFound n fm) (pqInsertAll npq $ newNeighbours fm n)


addValToAll :: (a, Int) -> [(a, Int)] -> [(a, Int)]
addValToAll (_,x) = map $ \(p, y) -> (p, x+y)

addToFound :: Ord k => (k, a) -> M.Map k a -> M.Map k a
addToFound (p, x) = M.insert p x

withoutFound :: Ord k => M.Map k a -> [(k, b)] -> [(k, b)]
withoutFound mf = filter (notInMap mf)
    where
        notInMap m (p, _) = not $ M.member p m

-- pq == minimal priority queue on lists
pqInsert :: Ord a => (b, a) -> [(b, a)] -> [(b, a)]
pqInsert = insertBy (compare `on` snd)

pqInsertAll :: Ord a => [(b, a)] -> [(b, a)] -> [(b, a)]
pqInsertAll = foldr pqInsert

-- data parsing and access


blowInputMx :: Int -> Mx.Matrix Int -> Mx.Matrix Int
blowInputMx n inMx = Mx.flatten . fmap (\v -> fmap (vUp9By v) inMx) $ upsMx n

upsMx :: Int -> Mx.Matrix Int
upsMx n = Mx.matrix n n $ \(i,j) -> i + j - 2

vUp9By :: Integral a => a -> a -> a
vUp9By n v = 1 + ((v - 1 + n) `mod` 9)

vUp9 :: Integral a => a -> a
vUp9 v = 1 + (v `mod` 9)

getEnd :: Mx.Matrix Int -> (Int, Int)
getEnd mx = (Mx.nrows mx, Mx.ncols mx)

neighbours :: Mx.Matrix Int -> ((Int, Int), Int) -> [((Int, Int), Int)]
neighbours mx (p, _) = catMaybes . map (getWraped mx) . adjecentPositions $ p
    where
        wrapPose r c x = ((r,c),x)
        getWraped m (r,c) = fmap (wrapPose r c) $ Mx.safeGet r c m

adjecentPositions :: (Num a, Num b) => (a, b) -> [(a, b)]
adjecentPositions x = map (addPos x) [(-1,0), (1,0), (0,-1), (0,1)]

addPos :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

getDefault :: Mx.Matrix c -> c -> (Int, Int) -> c
getDefault m d (r, c) = maybe d id . Mx.safeGet r c $ m

parseInput :: String -> Mx.Matrix Int
parseInput = Mx.fromLists . map (map digitToInt) . lines

sampleData :: String
sampleData = "\
\1163751742\n\
\1381373672\n\
\2136511328\n\
\3694931569\n\
\7463417111\n\
\1319128137\n\
\1359912421\n\
\3125421639\n\
\1293138521\n\
\2311944581"
