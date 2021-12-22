
import System.Environment
import Control.Monad   

-- lets try to fit all examples into one exec
main = do
    args <- getArgs
    case args!!0 of "reverse" -> main1
                    "reverse2" -> main2
                    "mapM" -> main3
                    "fromM" -> main4
                    "palindroms" -> palindroms


-- basic usage of main
main1 = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main1


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
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM_ putStrLn colors

palindroms = interact respondPalindromes


reverseWords :: String -> String  
reverseWords = unwords . map reverse . words 

respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines  
    where
        isPalindrome xs = xs == reverse xs  