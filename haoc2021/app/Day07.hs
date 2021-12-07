import System.Environment
import Data.List
import Data.List.Split

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            contents <- readFile file
            processInput contents
        _ -> processInput "16,1,2,0,4,2,7,1,2,14"


processInput :: String -> IO ()
processInput contents = do
    let xs = parseInput contents
    
    putStrLn "--- part 1"
    putStrLn . show . minimum . map (flip posCost xs) . adjacent 5 . mean $ xs

    putStrLn "--- part 2"
    putStrLn . show . minimum . map (flip realBurnForPos xs) . adjacent 5 . avgInt $ xs



posCost :: Num a => a -> [a] -> a
posCost p = sum . map (abs . (p-))

mean :: Integral a => [a] -> a
mean xs
    | odd  l = ys !! k
    | even l = avgInt [ys !! k, ys !! (k+1)]
    where ys = sort xs
          l = length ys
          k = l `div` 2

avgInt :: Integral a => [a] -> a
avgInt xs = (sum xs) `div` (genericLength xs)

adjacent :: (Num a, Enum a) => a -> a -> [a]
adjacent r x = [(x-r)..(x+r)]

realBurnForPos :: Integral a => a -> [a] -> a
realBurnForPos p = sum . map (s . abs . (p-))
    where s c = (1 + c) * c `div` 2

parseInput :: String -> [Int]
parseInput = map read . wordsOn ','

wordsOn :: Char -> String -> [String]
wordsOn c s  =  case dropWhile (==c) s of
    "" -> []
    s' -> w : wordsOn c s''
        where (w, s'') = break (==c) s'