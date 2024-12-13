{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Advent Of Code 2021 Day 24
-- https://adventofcode.com/2021/day/24

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools
import qualified Data.Map.Strict as M
import Data.List.NonEmpty (sortWith)

main :: IO ()
main = do
    inputData <- readInput sampleData

    let prog = parseInput inputData

    putStrLn "--- part 1"
    -- putStrLn . ppState . aluProgRun prog $ map digitToInt "93579246899999"
    -- printAluProgSteps prog $ map digitToInt "99999999999999"
    putStrLn . map (intToDigit  . snd . snd) . maximizer . parseMONAD $ inputData

    putStrLn "--- part 2"
    putStrLn . map (intToDigit  . fst . snd) . maximizer . parseMONAD $ inputData


newtype Var = Var Char deriving (Show, Eq)

newtype Const = Const Int deriving (Show, Eq)

data Op = Inp | Add | Mul | Div | Mod | Eql deriving (Show, Eq)

type Value = Either Var Const

data Expr = Expr1 Op Var
          | Expr2 Op Var Value
          deriving (Show, Eq)

newtype State = State (M.Map Char Int) deriving (Show, Eq)

data ALUProg = ALUProg [Expr] State deriving (Show, Eq)

readOp :: String -> Op
readOp "inp" = Inp
readOp "add" = Add
readOp "mul" = Mul
readOp "div" = Div
readOp "mod" = Mod
readOp "eql" = Eql
readOp _ = error "Unknown operation"

readVar :: String -> Var
readVar = Var . head

readConst :: String -> Const
readConst = Const . read

readExpr :: String -> Expr
readExpr = parts . words
  where
    parts [o,v] = Expr1 (readOp o) (readVar v)
    parts [o,v,c] = Expr2 (readOp o) (readVar v) (readEither c)
    readEither c = if isDigit $ last c then Right $ readConst c else Left $ readVar c

ppExpr :: Expr -> String
ppExpr (Expr1 op (Var c)) = show op ++ [' ', c, ' ', ' ']
ppExpr (Expr2 op (Var c) (Left (Var v))) = show op ++ [' ', c, ' ', v]
ppExpr (Expr2 op (Var c) (Right (Const i))) = show op ++ [' ', c, ' '] ++ show i

getState :: State -> Var -> Int
getState (State m) (Var c) = m M.! c

setState :: State -> Var -> Int -> State
setState (State m) (Var c) i = State $ M.insert c i m

ppState :: State -> String
ppState (State m) = intercalate ", " . map (\(c,i) -> c:"=" ++ show i) . M.toList $ m

getValue :: State -> Value -> Int
getValue _ (Right (Const i)) = i
getValue st (Left v) = getState st v

evalOp2 :: Op -> Int -> Int -> Int
evalOp2 Add x y = x + y
evalOp2 Mul x y = x * y
evalOp2 Div x y = x `div` y
evalOp2 Mod x y = x `mod` y
evalOp2 Eql x y = if x == y then 1 else 0


aluProgRun :: Foldable t => t Expr -> [Int] -> State
aluProgRun es xs = fst . foldl evalExpr (zeroState "wxyz", xs) $ es

printAluProgSteps :: [Expr] -> [Int] -> IO ()
printAluProgSteps es xs = putStrLn . unlines $ zipWith (\e s -> e ++ " | " ++ s) exprs states
  where
    exprs = "init   " : map ppExpr es
    states = map (ppState . fst) . scanl evalExpr (zeroState "wxyz", xs) $ es

zeroState :: [Char] -> State
zeroState cs = State . M.fromList $ zip cs [0,0..]

evalExpr :: (State, [Int]) -> Expr -> (State, [Int])
evalExpr (st, xs) (Expr1 Inp v) = (setState st v $ head xs, tail xs)
evalExpr (st, xs) (Expr2 op x y) = (setState st x $ evalOp2 op (getState st x) (getValue st y), xs)


parseInput :: String -> [Expr]
parseInput = map readExpr . lines . trim

sampleData :: String
sampleData = [r|
inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2
|]

sd = parseInput sampleData


--------------------------------------------------------------------------------
-- impl for explicit input MONAD program...

parseInFile = maximizer . parseMONAD <$> readFile "data/day_24_input"

parseMONAD = remapper . zip [0..] . map taker . cutter 18 . lines . trim

cutter :: Int -> [a] -> [[a]]
cutter n [] = []
cutter n xs = take n xs : cutter n (drop n xs)

taker :: [String] -> (Int, Int)
taker xs = (grabber $ xs !! 5, grabber $ xs !! 15)

grabber :: String -> Int
grabber = read . last . words

--sortBy (compare `on` fst) . concatMap minMax . 

remapper = go []
  where
    go _ [] = []
    go st ((i, (a, b)):xs)
      | a > 0 = go ((i,b):st) xs
      | a < 0 = let ((ip, bp):ss) = st in (ip,bp,i,a) : go ss xs

minMax :: (Int, Int, Int, Int) -> [(Int, (Int, Int))]
minMax (dpush, b, dpop, a)
  | c >= 0 = [(dpush, (1, 9 - c)), (dpop, (1 + c, 9))]
  | c < 0  = [(dpush, (1 - c, 9)), (dpop, (1, 9 + c))]
  where
    c = b + a

maximizer = sortBy (compare `on` fst) . concatMap minMax

{-

so, after reading explenation on this here:
https://github.com/kemmel-dev/AdventOfCode2021/blob/master/day24/AoC%20Day%2024.pdf

I'm trying to came up with own understanding and impl

inp w     inp w         D
mul x 0   mul x 0  
add x z   add x z  
mod x 26  mod x 26    
div z 1   div z 26      1 if A > 0 or 26 if A < 0
add x 15  add x -7      A, allways >10 or <0
eql x w   eql x w  
eql x 0   eql x 0  
mul y 0   mul y 0  
add y 25  add y 25    
mul y x   mul y x  
add y 1   add y 1  
mul z y   mul z y  
mul y 0   mul y 0  
add y w   add y w  
add y 15  add y 6       B
mul y x   mul y x  
add z y   add z y  



z = A > 0 ? z : z / 26
x = z % 26 + A
z = x == D ? z : z * 26 + (D + B)

if A > 0
  z = z * 26 + D + B       -> push D+B  -> Dpush + Bpush
else
  assert z % 26 + A == D   -> pop == D-A  -> Dpop - Apop
  z = z / 26

from this we get
Dpush + Bpush == Dpop - Apop
Dpush + Bpush + Apop == Dpop

now, with this, lets consider pairs:
d0 with B == 15
d13 with A == -10
so
d0 + 15 - 10 == d13   -> d0 + 5 == d13    

or
d2, B = 6     
c3, A = -14   
d2 + 6 - 14 == c3     -> d2 - 8 == d3

to maximize number, we need to have 9 for d13, (or 9 for d2, as b+a is < 0)
to minimize number, we need to put 1 in d0, (or 1 in d3, as b+a is < 0)
so

maxed num -> a+b > 0 | Dpush = 9 - (a+b); Dpop = 9
             a+b < 0 | Dpush = 9; Dpop = 9 + (a+b)
mined num -> a+b > 0 | Dpush = 1 ; Dpop = 1 + (a+b)
             a+b < 0 | Dpush = 1 - (a+b); Dpop = 1

-}