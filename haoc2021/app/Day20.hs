{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Advent Of Code 2021 Day 20
-- https://adventofcode.com/2021/day/20

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools
import qualified Data.Matrix as Mx
import qualified Data.Vector as V

main :: IO ()
main = do
    inputData <- readInput sampleData

    let (iea, mx) = parseInput inputData

    putStrLn "--- part 1"
    print . countLit . process iea . process iea $ mx

    putStrLn "--- part 2"
    print . countLit $ (iterate (process iea) mx !! 50)


type IEA = V.Vector Int   -- Image Enchencement Algorithm
data Image = Img (Mx.Matrix Int) Int

instance Show Image where
    show = pprint

countLit :: Image -> Int
countLit (Img mx _) = sum mx

process :: IEA -> Image -> Image
process iea img@(Img mx d) = Img nmx nd
    where
        rows = Mx.nrows mx + 2
        cols = Mx.ncols mx + 2
        getAt (i,j) = getNext iea img (i-1,j-1)
        nmx = Mx.matrix rows cols getAt
        nd = nextDefault iea d

getNext :: IEA -> Image -> (Int, Int) -> Int
getNext iea img = (iea V.!) . b2i . map (get img) . adjecentPositions

nextDefault :: IEA -> Int -> Int
nextDefault iea 0 = iea V.! 0    -- all 9 of 0
nextDefault iea 1 = iea V.! 511  -- all 9 of 1

get :: Image -> (Int, Int) -> Int
get (Img mx d) (r,c) = fromMaybe d $ Mx.safeGet r c mx

pprint :: Image -> String
pprint img@(Img mx d) = unlines [[ij2c (i,j) | j <- [0 .. cols]] | i <- [0 .. rows]]  -- show one more line on borders
    where
        cols = Mx.ncols mx + 1
        rows = Mx.nrows mx + 1
        i2c 0 = '.'
        i2c 1 = '#'
        ij2c = i2c . get img

adjecentPositions :: (Int, Int) -> [(Int, Int)]
adjecentPositions (x,y) = [(x+xx,y+yy) | xx <- [-1..1], yy <- [-1..1]]

parseInput :: String -> (IEA, Image)
parseInput input =
    let c2i '#' = 1
        c2i '.' = 0
        [spec, dat] = splitOn "\n\n" input
        iea = V.fromList . map c2i . concat . lines $ spec
        mx = fmap c2i . Mx.fromLists . lines $ dat
    in  (iea, Img mx 0)


sampleData = "\
\..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##\n\
\#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###\n\
\.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.\n\
\.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....\n\
\.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..\n\
\...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....\n\
\..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#\n\
\\n\
\#..#.\n\
\#....\n\
\##..#\n\
\..#..\n\
\..###"

(siea, si) = parseInput sampleData