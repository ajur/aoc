{-# LANGUAGE QuasiQuotes #-}
-- Advent Of Code 2021 Day 22
-- https://adventofcode.com/2021/day/22

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools
import qualified Data.Set as S

main :: IO ()
main = do
    inputData <- readInput sampleData

    let actions = parseInput inputData

    putStrLn "--- part 1"
    -- print $ sol1 actions
    print $ sol1' actions

    putStrLn "--- part 2"
    print $ sol2 actions


-- sol2 treating cuboids kinda like sets - so operating only on intersections

sol2 :: [Action] -> Integer
sol2 = diffListVolume . actionsToDiffList

sol1' :: [Action] -> Integer
sol1' = diffListVolume . actionsToDiffList . filter (rangeWithin 50)

type Volume = Integer

data Op = Add | Rem deriving (Eq, Show)

data Diff = Diff Op Cuboid deriving (Eq, Show)

type DiffList = [Diff]

negOp :: Op -> Op
negOp Add = Rem
negOp Rem = Add

volume :: Cuboid -> Integer
volume (Cuboid rx ry rz) = product . map rangeSize $ [rx, ry, rz]

rangeSize :: Range -> Integer
rangeSize (x,y) = max 0 (y - x + 1)   -- +1 as we include both bounds, max with 0 will cut out negatives, usefull for intersection

rangesIntersection :: Range -> Range -> Range
rangesIntersection (x1,y1) (x2,y2) = (max x1 x2, min y1 y2)

cuboidsIntersection :: Cuboid -> Cuboid -> Maybe Cuboid
cuboidsIntersection (Cuboid rx1 ry1 rz1) (Cuboid rx2 ry2 rz2)
    = if volume ic > 0 then Just ic else Nothing
    where
        irx = rangesIntersection rx1 rx2
        iry = rangesIntersection ry1 ry2
        irz = rangesIntersection rz1 rz2
        ic = Cuboid irx iry irz

negDiffIntersection :: Cuboid -> Diff -> Maybe Diff
negDiffIntersection c (Diff op dc) = Diff (negOp op) <$> cuboidsIntersection c dc

addActionToDiffList :: DiffList -> Action -> DiffList
addActionToDiffList dl (Action cmd c) = dl ++ diffsFromCmd cmd c ++ mapMaybe (negDiffIntersection c) dl
    where
        diffsFromCmd On c = [Diff Add c]
        diffsFromCmd _  _ = []

actionsToDiffList :: [Action] -> DiffList
actionsToDiffList = foldl addActionToDiffList []

diffToVolume :: Diff -> Volume
diffToVolume (Diff Add c) = volume c
diffToVolume (Diff Rem c) = -(volume c)

diffListVolume :: DiffList -> Volume
diffListVolume = sum . map diffToVolume

-- ok, heare is sol1 - seeing data I already knew it wont work, but hey, it was easy to do :P

sol1 :: [Action] -> Int
sol1 = S.size . foldl apply S.empty . filter (rangeWithin 50)
    where
        apply s a = op s $ toPositions a
            where
                op :: S.Set Pos -> S.Set Pos -> S.Set Pos
                op = case toCommand a of On  -> S.union
                                         Off -> S.difference

type Range = (Integer, Integer)
data Cuboid = Cuboid Range Range Range deriving (Show, Eq)
data Cmd = On | Off deriving (Show, Eq)
data Action = Action Cmd Cuboid deriving (Show, Eq)

type Pos = (Integer, Integer, Integer)

toPositions :: Action -> S.Set Pos
toPositions (Action _ (Cuboid (x1,x2) (y1,y2) (z1,z2))) = S.fromList [(x,y,z) | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]]

toCommand :: Action -> Cmd
toCommand (Action c _) = c

rangeWithin :: Integer -> Action -> Bool
rangeWithin n (Action _ (Cuboid (x1,x2) (y1,y2) (z1,z2))) = all ((<=n) . abs) [x1,x2,y1,y2,z1,z2]


parseInput :: String -> [Action]
parseInput = map readAction . lines . trim

readAction :: String -> Action
readAction s = Action (cmdFromStr c) (Cuboid x y z)
    where
        [c, r] = words s
        [x,y,z] = map rangeFromStr . splitOn "," $ r

cmdFromStr :: [Char] -> Cmd
cmdFromStr "on"  = On
cmdFromStr "off" = Off
cmdFromStr _     = error "unknown command"

rangeFromStr :: [Char] -> Range
rangeFromStr = asPair . sort . map read . splitOn ".." . drop 2

sampleData :: [Char]
sampleData = [r|
on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682
|]

sd = parseInput sampleData
