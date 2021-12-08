import System.Environment
import Data.List
import Data.List.Split
import qualified Data.Map as Map
-- import qualified Data.IntMap.Strict as IntMap

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            contents <- readFile file
            processInput contents
        _ -> processInput sampleData

processInput :: String -> IO ()
processInput contents = do
    let xs = parseInput contents
    
    putStrLn "--- input"
    putStr . unlines . map show . take 4 $ xs
    putStrLn "..."
    
    -- putStrLn . show . numbersMap . fst . head $ xs

    putStrLn "--- part 1"
    putStrLn . show . countSimpleDigits $ xs

    putStrLn "--- part 2"
    putStrLn . show . sum . map numFromLine $ xs


countSimpleDigits :: [([String], [String])] -> Int
countSimpleDigits = length . filter (`elem` [2,3,4,7]) . map length . concatMap snd    


numbersMap :: (Ord a1, Num a2, Enum a2, Eq a2) => [[a1]] -> Map.Map [a1] a2
numbersMap xs = Map.fromList [(num x, x) | x <- [0..9]]
    where
        allWithLen l = filter ((==l) . length) xs
        pickWithLen = head . allWithLen

        pickIntersectingWithLenOnNum l n = head . filter (isIntersectingWithLength l n)
            where isIntersectingWithLength l n = (==l) . length . intersect (num n)

        num 0 = pickIntersectingWithLenOnNum 3 4 . delete (num 6) $ allWithLen 6
        num 1 = pickWithLen 2
        num 2 = pickIntersectingWithLenOnNum 4 6 . delete (num 3) $ allWithLen 5
        num 3 = pickIntersectingWithLenOnNum 2 1 $ allWithLen 5
        num 4 = pickWithLen 4
        num 5 = pickIntersectingWithLenOnNum 5 6 . delete (num 3) $ allWithLen 5
        num 6 = pickIntersectingWithLenOnNum 1 1 $ allWithLen 6
        num 7 = pickWithLen 3
        num 8 = pickWithLen 7
        num 9 = pickIntersectingWithLenOnNum 4 4 . delete (num 6) $ allWithLen 6
        
translate :: (Num c, Ord k) => Map.Map k c -> [k] -> c
translate m = listToNum . map (m Map.!)

numFromLine :: (Num c, Ord a1, Enum c, Eq c) => ([[a1]], [[a1]]) -> c
numFromLine (x, y) = translate (numbersMap x) y

listToNum :: (Foldable t, Num a) => t a -> a
listToNum = foldl1 (\acc x -> acc * 10 + x)

parseInput :: String -> [([String], [String])]
parseInput = map parseLine . lines
    where parseLine = asTuple . map parseWords . splitOn " | "
          parseWords = map sort . words
          asTuple [x,y] = (x,y)

sampleData = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n\
             \edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n\
             \fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\n\
             \fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\n\
             \aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\n\
             \fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\n\
             \dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\n\
             \bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\n\
             \egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\n\
             \gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"


{-

  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg

1 = length == 2
4 = length == 4
7 = length == 3
8 = length == 7

235 = length == 5
069 = length == 6

6 = length (069 intersect 1) == 1
9 = length (09 intersect 4) == 4
0 = length (09 intersect 4) == 3

3 = length (253 intersect 1) == 2
2 = length (25 intersect 6) == 4
5 = length (25 intersect 6) == 5
-}
