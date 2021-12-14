-- Just some random collection of reimplementations and other functions
-- that I've wrote while reading http://learnyouahaskell.com/

itsMe = "It's a-me, Adam Jurczyk!"

doubleMe x = x + x

getAtIdx :: [a] -> Int -> a
getAtIdx xs i = xs !! i            -- bang! bang! and we have list item at index

removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]    -- list comprehension, works similar to python

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"                           -- pattern matching is interesting way of doing things
lucky 666 = error "GO BURN IN HELL!!!"
lucky x = "Sorry, out of luck."


factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: Num a => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1+x2, y1+y2)

len :: (Num b) => [a] -> b
len [] = 0
len (_:xs) = 1 + len xs

map' :: (a -> b) -> [a] -> [b]                  -- my early attempt at highier order func
map' f [] = []
map' f (x:xs) = f x : map' f xs

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height                                          -- guards for more flexible checks
    | bmi <= skinny = "You're underweight, you emo, you!"                    
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"
    where calcBmi w h = w / h ^ 2                                -- kinda like function local scope? ^^
          bmi = calcBmi weight height
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

max' :: Ord a => a -> a -> a
max' a b | a > b     = a
         | otherwise = b

myCompare :: Ord a => a -> a -> Ordering
a `myCompare` b
  | a > b     = GT
  | a < b     = LT
  | otherwise = EQ

maximum' :: Ord a => [a] -> a
maximum' [] = error "Empty list has no maximum"
maximum' [x] = x
maximum' (x:xs) = x `max` (maximum' xs)

replicate' :: (Num i, Ord i) => i -> b -> [b]
replicate' 0 x = []
replicate' n x
    | n < 0 = error "Number of replications should be positive"
    | otherwise = x : replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _  = []
zip' _ []  = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [a | a <- xs, a <= x] ++ [x] ++ quicksort [a | a <- xs, a > x]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ []          = []
zipWith' _ [] _          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

quicksort' :: Ord a => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = quicksort (filter (<=x) xs) ++ [x] ++ quicksort (filter (>x) xs)


collatz :: Integral a => a -> [a]
collatz 1 = [1]
collatz x
    | x < 1  = error "Only natural numbers allowed"
    | odd x  = x : collatz (x * 3 + 1)
    | even x = x : collatz (x `div` 2)

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x                -- this lambda syntax is bit strange ^^

-- FOLDS

sum' :: Num a => [a] -> a
sum' = foldl (+) 0

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x acc -> f x : acc) []

sum'' :: Num a => [a] -> a
sum'' = foldl1 (+)        -- fold left with first element of list as accumulator

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []

reverse''' :: [a] -> [a]
reverse''' = foldl (flip (:)) []

triangularNumbers :: Integral a => a -> [a]
triangularNumbers x = scanl1 (+) [0..x]     -- scan executes fold but reports list of all acc states 

sqrtSumsToGoOver :: (Ord a, Floating a, Enum a) => a -> Int                            -- takeWhile is usefull for infinite lists
sqrtSumsToGoOver x = (+1) $ length $ takeWhile (<x) $ scanl1 (+) $ map sqrt [1..]      -- also application function $ rocks

sqrtSumsToGoOver' :: (Ord a, Floating a, Enum a) => a -> Int
sqrtSumsToGoOver' x = (+1) . length . takeWhile (<x) . scanl1 (+) . map sqrt $ [1..]    -- function composition is also dope

sqrtSumsToGoOver'' :: (Ord a, Floating a, Enum a) => a -> Int
sqrtSumsToGoOver'' x =                                                                  -- somewhat more readabl version
    let squaresSums = scanl1 (+) . map sqrt $ [1..]                                     -- arguably :P
        belowLimit  = takeWhile (<x) squaresSums
    in  length belowLimit + 1


-- Types

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) dx dy = Circle (Point (x+dx) (y+dy)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) dx dy = Rectangle (Point (x1+dx) (y1+dy)) (Point (x2+dx) (y2+dy))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

