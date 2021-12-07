import System.Environment

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile $ head args
    let ls = parseLines $ lines contents
    
    putStrLn "--- part 1"
    putStrLn $ show (simpleSonar ls)

    putStrLn "--- part 2"
    putStrLn $ show (betterSonar ls)



parseLines :: [String] -> [Int]
parseLines = map read


simpleSonar :: [Int] -> Int
simpleSonar []  = 0
simpleSonar (_:[]) = 0
simpleSonar (x:y:ys) = incrementIfIncreased x y (simpleSonar (y:ys))


incrementIfIncreased :: (Ord a, Enum b) => a -> a -> b -> b
incrementIfIncreased x y n
    | y > x     = succ n
    | otherwise = n


betterSonar :: [Int] -> Int
betterSonar xs = simpleSonar (windows xs)


windows :: [Int] -> [Int]
windows [] = []
windows (_:[]) = []
windows (_:_:[]) = []
windows (x:y:z:zs) = (x + y + z) : windows (y:z:zs)
