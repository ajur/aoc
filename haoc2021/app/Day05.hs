import Data.List
import Data.List.Split

main :: IO ()
main = do
    -- args <- getArgs
    -- case args of
    --     [file] -> do
    --         contents <- readFile file
    --         processInput contents
    --     _ -> processInput "16,1,2,0,4,2,7,1,2,14"
    putStrLn . intercalate "\n" . splitOn "," $ "16,1,2,0,4,2,7,1,2,14"