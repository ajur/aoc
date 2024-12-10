
import System.Environment
import Control.Monad
import System.IO
import System.IO.Error
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as B
import Control.Exception (catch, displayException)

-- lets try to fit all examples into one exec
main = do
    args <- getArgs
    case args!!0 of "reverse" -> main1
                    "reverse2" -> main2
                    "mapM" -> main3
                    "fromM" -> main4
                    "palindroms" -> palindromsMain
                    "copyFile" -> copyFileMain
                    "rpn" -> rpnMain
                    "rpns" -> rpnSafeMain
                    _ -> error "unknown program"


-- basic usage of main
main1 = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main1

reverseWords :: String -> String
reverseWords = unwords . map reverse . words


-- when tkes predicate and IO and returns the same IO if predicate passes, or returns empty IO ()
main2 = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main2


main3 = do
    -- sequence :: [IO a] -> IO [a]
    rs <- sequence [getLine, getLine, getLine]
    print rs

    -- or even:
    sequence $ map print [1..5]
    -- same effect
    mapM print [1..5]
    -- or with ditching result
    mapM_ print [1..5]

main4 = do
    -- forM is kinda like mapM, just flipped, so we can use it with lambdas and do to mimic creazy looking forEach ;P
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        getLine)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM_ putStrLn colors

palindromsMain = interact respondPalindromes

respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines
    where
        isPalindrome xs = xs == reverse xs



copyFileMain = do
    (fileName1:fileName2:_) <- tail <$> getArgs
    copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
    contents <- B.readFile source
    B.writeFile dest contents



mainWithExceptionHandling = toTry `catch` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | otherwise = ioError e



rpnMain = interact $ unlines . map (show . rpnEval) . lines


rpnEval :: (Num a, Read a, Integral a) => String -> a
rpnEval = head . foldl go [] . words
    where
        go (x:y:zs) "+" = (y+x) : zs
        go (x:y:zs) "-" = (y-x) : zs
        go (x:y:zs) "*" = (y*x) : zs
        go (x:y:zs) "/" = (y `div` x) : zs
        go (x:y:zs) "%" = (y `mod` x) : zs
        go zs n = read n : zs


rpnSafeMain = interact $ unlines . map (show . rpnSafeEval) . lines

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of ((x,""):_) -> Just x
                              _ -> Nothing

-- rpnSafeEval :: String -> Maybe Double
rpnSafeEval :: String -> Maybe Double
rpnSafeEval s = do
    [x] <- foldM rpnSafeEvalOne [] (words s)
    pure x

rpnSafeEvalOne :: [Double] -> String -> Maybe [Double]
rpnSafeEvalOne (x:y:zs) "+" = pure $ (y+x) : zs
rpnSafeEvalOne (x:y:zs) "-" = pure $ (y-x) : zs
rpnSafeEvalOne (x:y:zs) "*" = pure $ (y*x) : zs
rpnSafeEvalOne (x:y:zs) "/" = pure $ (y/x) : zs
rpnSafeEvalOne zs n = (:zs) <$> readMaybe n
