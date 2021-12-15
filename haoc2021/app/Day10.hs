-- Advent Of Code 2021 Day 10
-- https://adventofcode.com/2021/day/10

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools

main :: IO ()
main = do
    inputData <- readInput sampleData
    
    let ls = lines inputData
    
    putStr . unlines . map (\x -> (show x) ++ " " ++ (show (parenChecker [] x))) $ take 10 ls
    putStrLn "..."

    putStrLn "--- part 1"
    putStrLn . show . sum . map (missingParenPoints . pickMissingChar . parenChecker []) $ ls

    putStrLn "--- part 2"
    putStrLn . show . midVal . sort . map (incompleteParenScore 0 . pickUnclosedParens) . filter isIncomplete . map (parenChecker []) $ ls

midVal :: [a] -> a
midVal xs = xs !! ((length xs) `div` 2)

incompleteParenScore :: Int -> [Char] -> Int
incompleteParenScore x [] = x
incompleteParenScore x (c:cs) = incompleteParenScore (5 * x + incompleteParenPoints c) cs

incompleteParenPoints :: Char -> Int
incompleteParenPoints '(' = 1
incompleteParenPoints '[' = 2
incompleteParenPoints '{' = 3
incompleteParenPoints '<' = 4

pickUnclosedParens :: (String, String, String) -> String
pickUnclosedParens (_,_,a) = a

isIncomplete :: (String, String, String) -> Bool
isIncomplete ("","",a) = True
isIncomplete _         = False

missingParenPoints :: String -> Int
missingParenPoints "" = 0
missingParenPoints ")" = 3
missingParenPoints "]" = 57
missingParenPoints "}" = 1197
missingParenPoints ">" = 25137

pickMissingChar :: (String, String, String) -> String
pickMissingChar (a,_,_) = a

parenChecker :: [Char] -> [Char] -> ([Char], [Char], [Char])
parenChecker st [] = ("", "", st)
parenChecker st (x:xs)
    | x `elem` "([{<"   = parenChecker (x:st) xs
    | length st == 0    = ([x], xs, st)
    | pair x == head st = parenChecker (tail st) xs
    | otherwise         = ([x], xs, st)

pair :: Char -> Char
pair ')' = '('
pair '}' = '{'
pair ']' = '['
pair '>' = '<'
pair '(' = ')'
pair '[' = ']'
pair '{' = '}'
pair '<' = '>'

sampleData :: String
sampleData = "\
\[({(<(())[]>[[{[]{<()<>>\n\
\[(()[<>])]({[<{<<[]>>(\n\
\{([(<{}[<>[]}>{[]{[(<()>\n\
\(((({<>}<{<{<>}{[]{[]{}\n\
\[[<[([]))<([[{}[[()]]]\n\
\[{[{({}]{}}([{[{{{}}([]\n\
\{<[[]]>}<{[{[{[]{()[[[]\n\
\[<(<(<(<{}))><([]([]()\n\
\<{([([[(<>()){}]>(<<{{\n\
\<{([{{}}[<[[[<>{}]]]>[]]"
