{-# LANGUAGE QuasiQuotes #-}
-- Advent Of Code 2021 Day 23
-- https://adventofcode.com/2021/day/23

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools
import qualified Data.Set as S

main :: IO ()
main = do
    inputData <- readInput sampleData

    --let actions = parseInput inputData

    putStrLn "--- part 1"
    -- print $ sol1 actions
    print $ sum [5,20,7,700,500,50,2000,70,3000,9000,3,3]  -- pen & pencil computing xD

    putStrLn "--- part 2"
    --print $ sol2 actions



{-

ok, moliwy algo rekurencyjny, zachłanny:

go positions moved
    if isFinished -> return just moved
    movable = all that can move
    if no one can move -> return nothing
    for each movable
        moves = possible moves
        if no possible moves ->  return nothing
        for each move in moves
            newPositions = do move on positions
            newMoved - add move to moved
            results ++ go newPositions newMoved
    return min results

moved nie musi być posyłane w głąb..

go positions
    isFinished -> return 0
    noMoreMoves -> return Infinity
    return minimum . map (move cost + go with this cost) $ [each possible move]
-}

sampleData :: [Char]
sampleData = [r|
#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########

#############
#...........#
###B#C#B#D###
  #D#C#B#A#
  #D#B#A#C#
  #A#D#C#A#
  #########
|]

{-
s1


############# ############# ############# ############# ############# ############# ############# ############# ############# ############# ############# ############# #############
#...........# #A..........# #A......B...# #AA.....B...# #AA.....B...# #AA.....B...# #AA.........# #AA.......D.# #AA.......D.# #AA.........# #AA.........# #A..........# #...........#
###C#A#B#D### ###C#.#B#D### ###C#.#.#D### ###C#.#.#D### ###C#.#.#D### ###.#.#C#D### ###.#.#C#D### ###.#.#C#.### ###.#B#C#.### ###.#B#C#.### ###.#B#C#D### ###.#B#C#D### ###A#B#C#D###
  #D#C#A#B#     #D#C#A#B#     #D#C#A#B#     #D#C#.#B#     #D#.#C#B#     #D#.#C#B#     #D#B#C#B#     #D#B#C#B#     #D#B#C#.#     #D#B#C#D#     #.#B#C#D#     #A#B#C#D#     #A#B#C#D#  
  #########     #########     #########     #########     #########     #########     #########     #########     #########     #########     #########     #########     #########  
  
-}