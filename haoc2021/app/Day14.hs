-- Advent Of Code 2021 Day 14
-- https://adventofcode.com/2021/day/14

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
    inputData <- readInput sampleData
    
    let (tmpl, spec) = parseInput inputData
    
    putStrLn "--- sample"
    let (sTmpl, sSpec) = parseInput sampleData
    putStr "template: "
    putStrLn $ sTmpl
    putStr "spec map: "
    putStrLn $ show sSpec
    
    putStrLn "--- part 1"
    putStrLn . show $ betterSolution spec 10 tmpl

    putStrLn "--- part 2"
    putStrLn . show $ betterSolution spec 40 tmpl


-- recurency with memoization...
-- yeah, i know there should be better way.. but hey, it works

betterSolution :: Map.Map String Char -> Int -> String -> Int
betterSolution spec n tmpl = last counts - head counts
    where
        afterN = getCounts spec n tmpl
        counts = sort . Map.elems $ afterN

getCounts :: Map.Map String Char -> Int -> String -> Map.Map Char Int
getCounts spec nSteps tmpl = Map.unionsWith (+) . (firstMap:) . map (countsMemo nSteps) $ splitToPairs tmpl
    where
        firstMap :: Map.Map Char Int
        firstMap = Map.fromListWith (+) [(c,1) | c <- tmpl]
        mab :: String -> Char
        mab = (spec Map.!)
        basePairs :: [String]
        basePairs = Map.keys spec
        countsMemo :: Int -> String -> Map.Map Char Int
        countsMemo n ab = (allCountsForInf !! n) Map.! ab
        allCountsForInf :: [Map.Map String (Map.Map Char Int)]
        allCountsForInf = map allCountsForN [0..]
        allCountsForN :: Int -> Map.Map String (Map.Map Char Int)
        allCountsForN n = Map.fromList [(ab, counts n ab) | ab <- basePairs]
        counts :: Int -> String -> Map.Map Char Int
        counts 0 _  = Map.empty
        counts 1 ab = Map.singleton (mab ab) 1
        counts n [a,b] = Map.unionsWith (+) [Map.singleton c 1, (countsMemo predN [c,b]), (countsMemo predN [a,c])]
            where
                c :: Char
                c = mab [a,b]
                predN :: Int
                predN = pred n


splitToPairs :: String -> [String]
splitToPairs (_:[]) = []
splitToPairs (a:b:bs) = [a,b] : splitToPairs (b:bs)


-- direct algo brute force - dont do this above 10 ;P 

solution :: Map.Map String Char -> Int -> String -> Int
solution spec n tmpl = last counts - head counts
    where
        afterN = polymerizeNTimes spec n tmpl
        counts = sort . map length . group . sort $ afterN

polymerizeNTimes :: Map.Map String Char -> Int -> String -> String
polymerizeNTimes spec n = foldr (.) id $ replicate n (polymerize spec)

polymerize :: Map.Map String Char -> String -> String
polymerize spec = foldr ins ""
    where
        ins c [] = [c]
        ins c (a:as) = c:(spec Map.! [c,a]):a:as

-- parse input

parseInput :: String -> (String, Map.Map String Char)
parseInput s = (a, m)
    where 
        [a,b] = splitOn "\n\n" s
        toKV l = let [k, (v:[])] = splitOn " -> " l in (k, v)
        m = Map.fromList . map toKV . lines $ b

sampleData :: String
sampleData = "\
\NNCB\n\
\\n\
\CH -> B\n\
\HH -> N\n\
\CB -> H\n\
\NH -> C\n\
\HB -> C\n\
\HC -> B\n\
\HN -> C\n\
\NN -> C\n\
\BH -> H\n\
\NC -> B\n\
\NB -> B\n\
\BN -> B\n\
\BB -> N\n\
\BC -> B\n\
\CC -> N\n\
\CN -> C"

{-

My pure thought process for turning part 2 into recurence with memoization...
... im sure there is easier way - but whateva - that worked ok

NNCB 1 -> NCNBCHB == b:2 c:2 h:1 n:2
NNCB 1 -> NN 1 + NC 1 + CB 1  -> NCN 0 + NBC 0 + NHB 0 == c:1 + b:1 + h:1 (+ n:2 c:1 b:1)

NNCB 2 -> NBCCNBBBCBHCB == b:6 c:4 h:1 n:2

NNCB 2                                                  => n:2 c:1 b:1
-> NN 2 + NC 2 + CB 2
-> NCN 1 + NBC 1 + CHB 1                                => c:1 b:1 h:1
    -> NC 1 + CN 1 + NB 1 + BC 1 + CH 1 + HB 1
    -> NBC 0 + CCN 0 + NBB 0 + BBC 0 + CBH 0 + HCB 0    => b:1 c:1 b:1 b:1 b:1 c:1
                                                       ==> b:6 c:4 h:1 n:2          !!!

-}
