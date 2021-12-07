import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            contents <- readFile file
            processInput contents
        _ -> processInput "3,4,3,1,2"


processInput :: String -> IO ()
processInput contents = do
    let xs = parseInput contents
    
    putStrLn "--- part 1"
    -- putStrLn . show . length . growFish 80 $ xs  -- watch out! unefficient!
    putStrLn . show . countAllFishes 80 $ xs

    putStrLn "--- part 2"
    putStrLn . show . countAllFishes 256 $ xs


parseInput :: String -> [Int]
parseInput = map read . wordsOn ','


wordsOn :: Char -> String -> [String]
wordsOn c s  =  case dropWhile (==c) s of
    "" -> []
    s' -> w : wordsOn c s''
        where (w, s'') = break (==c) s'


decrementWithReset :: (Eq p, Num p) => p -> p -> p
decrementWithReset x 0 = x
decrementWithReset x y = y - 1

growFish :: Int -> [Int] -> [Int]
growFish _ [] = []
growFish 0 xs = xs
growFish n xs = growFish (n-1) $ oldFish ++ newFish
    where dec = decrementWithReset 6
          countNew = length . filter (==0)
          oldFish = map dec xs
          newFish = replicate (countNew xs) 8



countAllFishes :: Int -> [Int] -> Integer
countAllFishes n = sum . map (\k -> fishes (n + 6 - k))

fishes :: Int -> Integer
fishes = (map f [0..] !!)
    where f n | n < 7  = 1
              | n >= 7 = (+1) . sum $ map (fishes . nk) [1..k]
              where 
                  k = n `div` 7
                  nk kk = (n - 2 - 7 * kk) `max` 0

(+1) . sum $ map (fishes . max 0 .(n - 2 - 7 * kk)) [1..(n `div` 7)]
