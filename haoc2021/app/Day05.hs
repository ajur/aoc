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
        _ -> processInput sampleData

processInput :: String -> IO ()
processInput contents = do
    let xs = parseInput contents
    
    putStrLn . unlines . map (\x -> (show x) ++ " " ++ (lineType x)) $ xs

    putStrLn "--- part 1"
    -- putStrLn . show . 

    putStrLn "--- part 2"
    -- putStrLn . show . 


lineType :: (Int, Int, Int, Int) -> String
lineType x
    | isHorizontal x = "H"
    | isVertical x   = "V"
    | isDiagonal   x = "D"

isHorizontal :: (Int, Int, Int, Int) -> Bool
isHorizontal (x1,_,x2,_) = x1 == x2

isVertical :: (Int, Int, Int, Int) -> Bool
isVertical (_,y1,_,y2) = y1 == y2

isDiagonal :: (Int, Int, Int, Int) -> Bool
isDiagonal x = not (isHorizontal x || isVertical x)

range :: (Ord a, Enum a) => a -> a -> [a]
range x y
    | x <= y  = [x..y]
    | otherwise = reverse [y..x]

dist :: Num a => a -> a -> a
dist x y = abs (y - x)

parseInput :: String -> [(Int, Int, Int, Int)]
parseInput = map parseLine . lines
    where parseLine = asTuple . map read . concatMap parsePair . splitOn " -> "
          parsePair = splitOn ","
          asTuple [x1,y1,x2,y2] = (x1, y1, x2, y2)

sampleData = "0,9 -> 5,9\n\
             \8,0 -> 0,8\n\
             \9,4 -> 3,4\n\
             \2,2 -> 2,1\n\
             \7,0 -> 7,4\n\
             \6,4 -> 2,0\n\
             \0,9 -> 2,9\n\
             \3,4 -> 1,4\n\
             \0,0 -> 8,8\n\
             \5,5 -> 8,2"