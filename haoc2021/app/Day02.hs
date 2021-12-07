import System.Environment

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile $ head args
    let ls = parseLines $ lines contents
    
    -- putStrLn $ unlines (["> " ++ x ++ " " ++ (show y) | (x,y) <- ls])

    putStrLn "--- part 1"
    putStrLn . show . simpleMoveProd $ ls

    putStrLn "--- part 2"
    putStrLn . show . aimedMoveProd $ ls



parseLines :: [String] -> [(String, Int)]
parseLines = map $ mapWords . words
    where mapWords (x:y:[]) = (x, (read y :: Int))



simpleMoveProd :: [(String, Int)] -> Int
simpleMoveProd = prod . simpleMove (0, 0)

simpleMove :: (Int, Int) -> [(String, Int)] -> (Int, Int)
simpleMove x [] = x
simpleMove x ((y, z):zs) = simpleMove (step y z x) zs

step :: String -> Int -> (Int, Int) -> (Int, Int)
step "forward" x (p, d) = (p + x, d)
step "down"    x (p, d) = (p, d + x)
step "up"      x (p, d) = (p, d - x)

prod :: (Int, Int) -> Int
prod (a,b) = a * b



aimedMoveProd :: [(String, Int)] -> Int
aimedMoveProd = prod . dropLast . aimedMove (0, 0, 0)

aimedMove :: (Int, Int, Int) -> [(String, Int)] -> (Int, Int, Int)
aimedMove x [] = x
aimedMove x ((y, z):zs) = aimedMove (aimedStep y z x) zs

aimedStep :: String -> Int -> (Int, Int, Int) -> (Int, Int, Int)
aimedStep "forward" x (p, d, a) = (p + x, d + a * x, a)
aimedStep "down"    x (p, d, a) = (p, d, a + x)
aimedStep "up"      x (p, d, a) = (p, d, a - x)

dropLast :: (Int, Int, Int) -> (Int, Int)
dropLast (x,y,z) = (x,y)
