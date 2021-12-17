-- fallowing tutorial in https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners.html

import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative


sample = "BIRK 281500Z 09014KT CAVOK M03/M06 Q0980 R13/910195"


airport :: ReadP String
airport = do
    code <- many1 (satisfy isUpper)
    satisfy isSpace
    return code


digit :: ReadP Char
digit = satisfy isDigit

numbers :: Int -> ReadP Int
numbers n = fmap read $ count n digit

toMPS :: String -> Int -> Int
toMPS "KT" speed  = div speed 2
toMPS "MPS" speed = speed

timestamp :: ReadP (Int, Int, Int)
timestamp = do
    day <- numbers 2
    hour <- numbers 2
    minute <- numbers 2
    string "Z "
    if day < 1 || day > 31 || hour > 23 || minute > 59 then
        pfail
    else
        return (day, hour, minute)

gustParser :: ReadP Int
gustParser = do
    satisfy (=='G')
    numbers 2 <|> numbers 3

data WindInfo = WindInfo
    { dir :: Int
    , speed :: Int
    , gusts :: Maybe Int
    }
    deriving Show

windInfo :: ReadP WindInfo
windInfo = do
    direction <- numbers 3
    speed <- numbers 2 <|> numbers 3
    gust <- option Nothing (fmap Just gustParser)
    unit <- string "KT" <|> string "MPS"
    string " "
    return (WindInfo 
        direction 
        (toMPS unit speed)
        (fmap (toMPS unit) gust))

data Report = Report
    { station :: String
    , time :: (Int, Int, Int)
    , wind :: WindInfo
    }
    deriving Show


metar :: ReadP Report
metar = do
    code <- airport
    time <- timestamp
    wind <- windInfo
    return (Report code time wind)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

