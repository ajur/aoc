-- Advent Of Code 2021 Day 18
-- https://adventofcode.com/2021/day/18

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools

import Text.ParserCombinators.ReadP
import Control.Applicative

main :: IO ()
main = do
    inputData <- readInput sampleData
    
    let input = parseInput inputData
    
    putStrLn "--- part 1"
    putStrLn . show . magnitude . sumTrees $ input

    putStrLn "--- part 2"
    putStrLn . show . sol2 $ input

sol2 :: [Tree] -> Int
sol2 ts = maximum . map magnitude . concat $ [[t1 <+> t1, t2 <+> t1] | t1 <- ts, t2 <- ts]


data Tree = TVal Int | TNode Tree Tree

instance Show Tree where
    show (TVal v) = show v
    show (TNode l p) = "{" ++ show l ++ "," ++ show p ++ "}"

infixl 6 <+>
(<+>) :: Tree -> Tree -> Tree
t1 <+> t2 = reduceTree $ TNode t1 t2 

sumTrees :: [Tree] -> Tree
sumTrees = foldl1 (<+>)

magnitude :: Tree -> Int
magnitude (TVal v) = v
magnitude (TNode l p) = 3 * magnitude l + 2 * magnitude p

reduceTree :: Tree -> Tree
reduceTree t = case explodeOne t of Just rt -> reduceTree rt
                                    Nothing -> case splitOne t of Just st -> reduceTree st
                                                                  Nothing -> t

explodeOne :: Tree -> Maybe Tree
explodeOne = fmap middle . go 1
    where
        middle (_, x, _) = x
        go _ (TVal v) = Nothing
        go d (TNode (TVal v1) (TVal v2)) = if d <= 4 then Nothing else Just (v1, TVal 0, v2)
        go d (TNode l p) = case go (d+1) l of Just (v1, ln, v2) -> Just (v1, TNode ln (addToLastChildOnLeft v2 p), 0)
                                              Nothing -> case go (d+1) p of Just (v1, pn, v2) -> Just (0, TNode (addToLastChildOnRight v1 l) pn, v2)
                                                                            Nothing -> Nothing

addToLastChildOnLeft :: Int -> Tree -> Tree
addToLastChildOnLeft 0 n           = n
addToLastChildOnLeft x (TVal v)    = TVal (v+x)
addToLastChildOnLeft x (TNode l p) = TNode (addToLastChildOnLeft x l) p

addToLastChildOnRight :: Int -> Tree -> Tree
addToLastChildOnRight 0 n           = n
addToLastChildOnRight x (TVal v)    = TVal (v+x)
addToLastChildOnRight x (TNode l p) = TNode l (addToLastChildOnRight x p)

splitOne :: Tree -> Maybe Tree
splitOne (TNode l p) = if isJust ml then fmap (\ll -> TNode ll p) ml else fmap (\pp -> TNode l pp) (splitOne p)
    where ml = splitOne l
splitOne (TVal v)
    | v < 10    = Nothing
    | otherwise = Just (TNode (TVal . halfDown $ v) (TVal . halfUp $ v))

halfDown :: Int -> Int
halfDown v = floor (fromIntegral v / 2.0)
halfUp :: Int -> Int
halfUp v = ceiling (fromIntegral v / 2.0)

shouldSplit :: Int -> Tree -> Bool
shouldSplit _ (TVal v) = v >= 10
shouldSplit _ _ = False

shouldExplode :: Int -> Tree -> Bool
shouldExplode d (TNode _ _) = d > 4
shouldExplode _ _ = False

testT :: (Int -> Tree -> Bool) -> Tree -> Bool
testT = go 1
    where
        go d p n@(TVal _) = p d n
        go d p n@(TNode nl np) = p d n || go (d+1) p nl || go (d+1) p np

readTree :: ReadP Tree
readTree = readTVal <|> readTNode

readTVal :: ReadP Tree
readTVal = fmap (TVal . read) $ munch1 isDigit

readTNode :: ReadP Tree
readTNode = do
    satisfy (=='[')
    ln <- readTree
    satisfy (==',')
    pn <- readTree
    satisfy (==']')
    return (TNode ln pn)

parseInput :: String -> [Tree]
parseInput = map asTree . lines

asTree :: String -> Tree
asTree = fst . head . readP_to_S readTree

cRed = "\ESC[31m"
cReset = "\ESC[0m"

sampleData = "\
\[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n\
\[[[5,[2,8]],4],[5,[[9,9],0]]]\n\
\[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n\
\[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n\
\[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n\
\[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n\
\[[[[5,4],[7,7]],8],[[8,3],8]]\n\
\[[9,3],[[9,9],[6,[4,9]]]]\n\
\[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n\
\[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"

sd = parseInput sampleData

sl1 = "[1,1]\n[2,2]\n[3,3]\n[4,4]"
sl2 = "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]"
sl3 = "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]\n[6,6]"