-- adv -> functors, applicative functors and monoids, monads

import Control.Applicative
import Data.Monoid
import Control.Monad
import Data.Function
import qualified Data.Foldable as F
import Data.Vector.Generic.Mutable (move)
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import System.Random
import Data.Ratio
import Data.Bifunctor
import Data.List

-- Functor is typeclass, with fmap
fmap'' :: (Functor f) => (a -> b) -> f a -> f b
fmap'' = fmap

-- also, not that that there is infix fmap operator <$> 

-- functor is kinda a computational context

-- IO is a functor
{-
@
instance Functor IO where  
    fmap f action = do  
        result <- action  
        return (f result)  
@
-}

{-
function is a functor too!

>>> f = fmap (*3) (+3)
>>> :t f
f :: Num b => b -> b

>>> f 3
18

what is fmap over func? well, its basically composition!

>>> fmap (*3) (+3) 3 == ((*3) . (+3) $ 3)
True
-}



{-
Haskell has curried functions by default, so
a -> b -> c       called as f x y
is more more like
a -> (b -> c)     and could be called as  (f x) y

so, actually all functions are single argument, and possibly returning another function
f x y = x * y    <=>   f x = \y -> x * y
so, we can take any argument function and map over functor

>>> fs = (*) <$> [1..5]
>>> :t fs
fs :: (Num a, Enum a) => [a -> a]
>>> map ($10) fs
[10,20,30,40,50]
-}

{-
what if we have both function and arg in functors? eg:
>>> jf = Just (*5)
>>> j5 = Just 5

so, how to combine them?
Here comes Applicative type class, it implements 'pure' and '<*>', especially sencond one is exactly what we need:
>>> :t (<*>)
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
>>> jf <*> j5
Just 25

something more fancy
>>> pure (+) <*> Just 5 <*> Just 3
Just 8

oh, it also has <$> witch is basically combination of pure and <*>
>>> (+) <$> Just 5 <*> Just 3
Just 8
-}

-- but! its more! consider this:

myAction :: IO String
myAction = do
    a <- getLine
    b <- getLine
    return $ a ++ b

-- it can be replaced easily by:

myAction' :: IO String
myAction' = (++) <$> getLine <*> getLine

-- its pretty common, so there is func for this...

myAction'' :: IO String
myAction'' = liftA2 (++) getLine getLine


{-
Ok, so lets introduce Monoid
its basicaly type class for types that have some binary function that can combine two Monoids
and an 'empty' value that when combined with other, returns unchanged other
this empty value is returned by `mempty` and combining is done with `mappend`.
There is also `mconcat` to combine all Monoids in a list.

class Monoid m where  
    mempty :: m  
    mappend :: m -> m -> m  
    mconcat :: [m] -> m  
    mconcat = foldr mappend mempty

laws:
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

Lists are Monoids:
instance Monoid [a] where  
    mempty = []  
    mappend = (++)

But Numbers also, e.g. with pairs `*,1` or `+,0`. 
But as there is more than one such func, if we would like to implement Monoid with number, wee need some wrapper newtype.
Data.Monoid has such wrappers - Product and Sum:

newtype Product a =  Product { getProduct :: a }  
    deriving (Eq, Ord, Read, Show, Bounded) 

instance Num a => Monoid (Product a) where  
    mempty = Product 1  
    Product x `mappend` Product y = Product (x * y)

>>> getProduct $ Product 10 `mappend` Product 4
40
>>> getProduct $ Product 5 `mappend` mempty
5

Bool is similar with newtypes Any and All
>>> getAny $ Any True `mappend` Any False
True
>>> getAny . mconcat . map Any $ [False, False, False, True]
True

but thats all pretty tedious...
more usefull is Ordering, that on 'x `mappend` y' returns x if its not EQ, or y otherwise
and it works kinda like sorting alphabetically, first one letter, if its equal, then next and so on...
e.g. func for sorting by length, than number of vovels, and finally tries alphabetically:
-}
lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vovels x `compare` vovels y) `mappend`
                    (x `compare` y)
  where
    vovels = length . filter (`elem` "aeiou")
{-
>>> lengthCompare "zen" "anna"
LT
>>> lengthCompare "zen" "ana"
LT
>>> lengthCompare "zen" "ann"
GT
-}


{-
Now, lets introduce Foldable - stuff we can fold on, like lists ofc.. or Maybe

>>> F.foldl1 (+) [1..10]
55

>>> F.foldr (+) 2 (Just 9)
11

We can aslo implement Foldable for our own types, like Trees (see intro.hs)
-}

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- and we can make it an instance of Foldable, by implementing either foldr, or.. foldMap, using Monoid

instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x `mappend`
                             F.foldMap f r

{-
we can do same folds as on lists
>>> t = Node 1 (Node 2 Empty Empty) (Node 4 Empty (Node 5 Empty Empty))
>>> F.foldl1 (+) t
12

or we can reduce to some Monoid
>>> getAny $ F.foldMap (\x -> Any $ x == 5) t
True

we can use it for some helpers implementations, like 'toList'
>>> treeToList t
[2,1,4,5]
-}

treeToList :: Tree a -> [a]
treeToList = F.foldMap (:[])



{-
OK, so after all that.. let step into Monad swamp...

Monad is Applicative typeclass with '>>=', '>>' and 'return', defined as:

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b   (called bind)
  (>>) :: m a -> m b -> m b
  return :: a -> m a

so, it kinda says, how to apply lifting function to other, already lifted value... or something like that?

>>> [1,2,3] >>= (take 3 . repeat)
[1,1,1,2,2,2,3,3,3]

-}

-- how bind (>>=) would look like for a Maybe?
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing  `bindMaybe` _ = Nothing
(Just x) `bindMaybe` f = f x

-- How it can be usefull? Lets see lyah example with "Piere on a line"
type Birds = Int              -- number of birds
type Pole = (Birds, Birds)    -- on left or right side of a pole
                              -- if diff is more than 3, Piere falls.. on a net, ofc :P

landLeftUnsafe :: Birds -> Pole -> Pole
landLeftUnsafe n (l, r) = (l + n, r)

landRightUnsafe :: Birds -> Pole -> Pole
landRightUnsafe n (l, r) = (l, r + n)

-- >>> landLeftUnsafe 3 (1,2)
-- (4,2)
-- >>> landRightUnsafe (-1) (3,3)
-- (3,2)
-- >>> (0,0) & landLeftUnsafe 1 & landLeftUnsafe 1 & landRightUnsafe 3
-- (2,3)
-- >>> (0,3) & landLeftUnsafe 10   -- that should fail
-- (10,3)
-- >>> (0,0) & landLeftUnsafe 1 & landRightUnsafe 4 & landLeftUnsafe (-1) & landRightUnsafe (-2)     -- and this seems ok, but it also should fail along the road!
-- (0,2)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (l, r)
    | abs (l + n - r) < 4 = Just (l + n, r)
    | otherwise           = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (l, r)
    | abs (r + n - l) < 4 = Just (l, r + n)
    | otherwise           = Nothing

-- >>> (1,2) & landLeft 3
-- Just (4,2)
-- >>> (1,2) & landRight 3
-- Nothing
-- >>> (0,0) & landLeft 1 & landLeft 2
-- Couldn't match type ‘Maybe Pole’ with ‘(Birds, Birds)’
-- Expected type: Maybe Pole -> Maybe Pole
--   Actual type: Pole -> Maybe Pole

-- uups, no more chaining with &.. lets try something else...
-- >>> (0,0) & landLeft 1 >>= landLeft 1 >>= landRight 3
-- Just (2,3)
-- >>> return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)
-- Nothing

-- OK, but what about some case, where we have nested >>= ?
-- >>> Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
-- Just "3!"
-- ok, lets write it some other way...
foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))
-- >>> foo
-- Just "3!"
-- better, but all those lambdas... we have to DO something about it
foodo :: Maybe String
foodo = do
    x <- Just 3
    y <- Just "!"
    return (show x ++ y)
-- >>> foodo
-- Just "3!"
-- works like a charm
fooboo :: Maybe String
fooboo = do
    x <- Just 3
    y <- Nothing
    return (show x ++ y)
-- >>> fooboo
-- Nothing


-- note, lists implements Monad also
listOfTuples :: [(Int, Char)]
listOfTuples = do
    n <- [1, 2]
    c <- ['a', 'b']
    return (n,c)
-- >>> listOfTuples
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

-- note how similar it is to list comprehension:
-- >>> [ (n,ch) | n <- [1,2], ch <- ['a','b'] ]
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

-- its same, as list comprehension is just syntatic sugar over monadic list... 
-- but how it does filtering? whith guard:
-- >>> guard (3 > 2) >> pure "foo" :: [String]
-- ["foo"]
-- >>> guard (3 > 5) >> pure "foo" :: [String]
-- []

sevensOnly :: [Int]
sevensOnly = do
    x <- [1..50]
    guard ('7' `elem` show x)
    pure x -- same as return x
-- >>> sevensOnly
-- [7,17,27,37,47]
-- >>> [ x | x <- [1..50], '7' `elem` show x ]
-- [7,17,27,37,47]


-- "Knight's quest problem:"
type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (r,c) = do
    -- we could use filter instead of monads, but just for 'funzies'
    (r',c') <- [(r-2, c-1), (r-2, c+1), (r+2, c-1), (r+2, c+1)
               ,(r-1, c-2), (r-1, c+2), (r+1, c-2), (r+1, c-2)
               ]
    guard (r' `elem` [1..8] && c' `elem` [1..8])
    pure (r',c')

in3 :: KnightPos -> [KnightPos]
in3 start = pure start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

-- >>> (6,2) `canReachIn3` (6,1)
-- True
-- >>> (6,2) `canReachIn3` (7,3)
-- False


-- now lest see at other Monad - Writer

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number " ++ show x])

-- >>> logNumber 3
-- WriterT (Identity (3,["Got number 3"]))

multWithLog :: Int -> Int -> Writer [String] Int
multWithLog x y = do
    x' <- logNumber x
    y' <- logNumber y
    tell ["Apply multiplication"]
    pure (x * y)

-- >>> runWriter $ multWithLog 3 4
-- (12,["Got number 3","Got number 4","Apply multiplication"])


-- as we see, we can use this to kinda add logging to programs

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        pure a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

-- >>> runWriter $ gcd' 8 3
-- (1,["8 mod 3 = 2","3 mod 2 = 1","2 mod 1 = 0","Finished with 1"])

-- ofc we could write the same without do notation, using '>>' after 'tell' (as we do not care what value tell returns, only context it brings)
gcd'' :: Int -> Int -> Writer [String] Int
gcd'' a b
    | b == 0    = tell ["Finished with " ++ show a] >> pure a
    | otherwise = tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)] >> gcd' b (a `mod` b)

-- >>> runWriter $ gcd'' 8 3
-- (1,["8 mod 3 = 2","3 mod 2 = 1","2 mod 1 = 0","Finished with 1"])



{-
lists can be inefficient, when doing ++, so we think about using something better for concatenating
lest look at 'difference list' defined, as a function

[1,2,3]  -->   \xs -> [1,2,3] ++ xs    or just   ([1,2,3]++)
-}

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

-- >>> fromDiffList $ toDiffList "bonkers"
-- "bonkers"

instance Semigroup (DiffList a) where
    (DiffList f) <> (DiffList g) = DiffList (f . g)

-- >>> fromDiffList $ toDiffList "bon" <> toDiffList "kers"
-- "bonkers"

instance Monoid (DiffList a) where
  mempty = DiffList ([]++)

-- >>> fromDiffList . mconcat . map toDiffList $ ["bo", "nk", "ers"]
-- "bonkers"

-- now we could use it instead of list in gcd' for storing logs... but i skip it :P


{-
Lets talk about state...

for now, lets have some state as snd in tuple, kinda like random and StdGet, but example will use stack
-}

type Stack = [Int]

pop' :: Stack -> (Int, Stack)
pop' []     = error "trying to pop from empty stack"
pop' (x:xs) = (x, xs)

push' :: Int -> Stack -> ((), Stack)
push' x st = ((), x:st)

stackManip' :: Stack -> (Int, Stack)
stackManip' stack = let
    ((),newStack1) = push' 3 stack
    (a ,newStack2) = pop' newStack1
    in pop' newStack2

-- >>> stackManip' [5,4,3,6]
-- (5,[4,3,6])

-- now lets use State monad

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push x = state $ \xs -> ((), x:xs)

stackManip :: State Stack Int
stackManip = do
    push 3
    pop  -- ignoring returned val, as its not used
    pop

-- >>> runState stackManip [5,4,3,6]
-- (5,[4,3,6])


-- we can use state for handling Random generators

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool,Bool,Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a,b,c)

-- >>> runState threeCoins $ mkStdGen 33
-- ((True,False,True),StdGen {unStdGen = SMGen 17473681083660280586 3174492301114349737})


-- using this, and Maybe monad, se safe RPN calc in helloworld.hs

-- now lets go back to knight and use some monad composition <=<

inMany :: Int -> KnightPos -> [KnightPos]
inMany n start = pure start >>= foldr (<=<) pure (replicate n moveKnight)

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn n start end = end `elem` inMany n start

-- >>> canReachIn 3 (6,2) (6,1)
-- True


-- lets create some custom Monad for storing events probablility

newtype Prob a = Prob
    { unProb :: [(a, Rational)]
    } deriving (Show)

-- >>> Prob [(1, 1%2),(2, 1%2)]
-- Prob {unProb = [(1,1 % 2),(2,1 % 2)]}

probEven :: [a] -> Prob a
probEven xs = Prob $ zip xs $ replicate n nth
  where
    n = length xs
    nth = 1 % fromIntegral n

-- >>> probEven [1,2,3]
-- Prob {unProb = [(1,1 % 3),(2,1 % 3),(3,1 % 3)]}

probCounts :: [(a, Integer)] -> Prob a
probCounts xns = Prob $ map (\(x,n) -> (x, n % nn)) xns
  where
    nn = sum . map snd $ xns

-- >>> probCounts [(1,3),(4,2),(5,1)]
-- Prob {unProb = [(1,1 % 2),(4,1 % 3),(5,1 % 6)]}

probUniq :: Ord a => [a] -> Prob a
probUniq = probCounts . map count . group . sort
  where
    count ys = (head ys, genericLength ys)

-- >>> probUniq [1,2,1,1,3,5,5,3,2,6]
-- Prob {unProb = [(1,3 % 10),(2,1 % 5),(3,1 % 5),(5,1 % 5),(6,1 % 10)]}

instance Functor Prob where
    fmap f (Prob xps) = Prob $ fmap (first f) xps   -- (first f) here is same as (\(x,y) -> (f x, y))

-- >>> fmap (*5) $ probEven [1,2,3]
-- Prob {unProb = [(5,1 % 3),(10,1 % 3),(15,1 % 3)]}

instance Applicative Prob where
    pure x = Prob [(x, 1%1)]
    (Prob fs) <*> (Prob xs) = Prob $ [(f x, pf * px) | (f, pf) <- fs, (x, px) <- xs]

-- >>> (*2) <$> probEven [1,2,3]
-- Prob {unProb = [(2,1 % 3),(4,1 % 3),(6,1 % 3)]}

-- >>> probEven [(*2),(*10)] <*> probEven [1,2,3]
-- Prob {unProb = [(2,1 % 6),(4,1 % 6),(6,1 % 6),(10,1 % 6),(20,1 % 6),(30,1 % 6)]}

instance Monad Prob where
    (Prob xs) >>= f = Prob $ [(y, px * py)| (x, px) <- xs, (y, py) <- unProb (f x)]

-- >>> Prob [(1, 1 % 4), (2, 3 % 4)] >>= (\x -> probEven [-x, x])
-- Prob {unProb = [(-1,1 % 8),(1,1 % 8),(-2,3 % 8),(2,3 % 8)]}


-- now, having that, last play with it


data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = probEven [Heads, Tails]

loadedCoin :: Prob Coin
loadedCoin = probCounts [(Heads, 1), (Tails, 9)]

allThreeFlipsTails :: Prob Bool
allThreeFlipsTails = do
    a <- coin
    b <- coin
    c <- loadedCoin
    pure (all (==Tails) [a,b,c])
-- >>> allThreeFlipsTails
-- Prob {unProb = [(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),(False,1 % 40),(True,9 % 40)]}

probSqueeze :: Ord a => Prob a -> Prob a
probSqueeze = Prob . map sumProbs . groupBy eqFst . sortOn fst . unProb
  where
    eqFst = (==) `on` fst
    sumProbs ys = (fst $ head ys, sum $ map snd ys)

-- >>> probSqueeze allThreeFlipsTails
-- Prob {unProb = [(False,31 % 40),(True,9 % 40)]}



-- zippers
-- lets use our Tree

freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        )

-- to traverse bit easier we do
data Direction = L | R deriving (Show)  
type Directions = [Direction]


changeAt :: Directions -> Char -> Tree Char -> Tree Char  
changeAt (L:ds) c (Node x l r) = Node x (changeAt ds c l) r  
changeAt (R:ds) c (Node x l r) = Node x l (changeAt ds c r)  
changeAt [] c (Node _ l r) = Node c l r
changeAt _ _ Empty = Empty

elemAt :: Directions -> Tree a -> Maybe a  
elemAt (L:ds) (Node _ l _) = elemAt ds l  
elemAt (R:ds) (Node _ _ r) = elemAt ds r  
elemAt [] (Node x _ _) = Just x 
elemAt _ Empty = Nothing

-- >>> elemAt [R,L] freeTree
-- Just 'W'
-- >>> freeTree & changeAt [R,L] 'P' & elemAt [R,L]
-- Just 'P'

-- cool, but it can be inefficient - lets se on other ways

type Breadcrumbs = [Direction]
