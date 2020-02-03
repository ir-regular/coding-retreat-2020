{-# LANGUAGE ViewPatterns #-}

module Chapter3.MoreModules where

import Data.List
import Data.Maybe
import Chapter2.SimpleFunctions (Client(..), Person(..), Gender(..), clientName2)

-- example from the book
--
-- permutations of a string, but only those starting with a specific letter

permutationsStartingWith :: Char -> String -> [String]
permutationsStartingWith letter = filter (\l -> head l == letter) . permutations

-- let's rewrite library functions! ok
-- I'm rrrreally missing juxt here
-- which ofc you can't do using sequence because the return types differ!

filter' :: (a -> Bool) -> [a] -> [a]
filter' p []                 = []
filter' p (x:xs) | p x       = x : filter p xs
                 | otherwise = filter p xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f init []     = init
foldr' f init (x:xs) = f x $ foldr' f init xs

-- hmm... it should be possible to do this differently, but ok, this is copied
-- "how to fake an infinity"

data InfNumber a = MinusInfinity
                 | Number a
                 | PlusInfinity
                 deriving Show

infMax :: (Ord a) => InfNumber a -> InfNumber a -> InfNumber a
infMax MinusInfinity x       = x
infMax x MinusInfinity       = x
infMax PlusInfinity x        = PlusInfinity
infMax x PlusInfinity        = PlusInfinity
infMax (Number x) (Number y) = Number (max x y)

maximum' :: (Ord a) => [a] -> InfNumber a
maximum' = foldr' (\x y -> infMax (Number x) y) MinusInfinity

-- or if the list is guaranteed to have 1 element
--maximum' :: [Integer] -> Integer
--maximum' = foldr1 max

-- exercise 3.3

-- Consider the functions product, minimumClient, and all.
--
-- the product function computes the product of a list of integers.
-- the minimumClient function computes the cClient with the shortest name.
-- Finally, the all function computes the conjunction (&&) of a list of Boolean values.
--
-- given these functions, do the following:
-- - Write the functions using pattern matching, without resorting to any higher-order function.
-- - Write the functions as folds. in each case, first try to find the aggregation operation,
--    and from that derive a sensible initial value.
-- Can you find the structure that all these functions share when written in the first style?
-- in which cases is it true that using foldr and foldl give the same results?
--
-- extra: try to write a minimumBy function such that the order is taken by first applying
-- a function g on the result.
-- For example, minimumBy (\x -> -x) [1,2,3] should return 3.

product' :: (Num a) => [a] -> a
product' [x]    = x
product' (x:xs) = x * (product' xs)

minimumClient :: [Client] -> Client
minimumClient [x]    = x
minimumClient (x:xs) = if (length . clientName2 $ x) < (length . clientName2 $ otherClient)
                         then x
                         else otherClient
                       where otherClient = minimumClient xs

conjunction :: [Bool] -> Bool
conjunction [x]        = x
conjunction (False:xs) = False
conjunction (True:xs)  = conjunction xs

productFold :: (Num a) => [a] -> a
productFold = foldl (*) 1

shorterName :: Maybe Client -> Client -> Maybe Client
shorterName Nothing x                                        = Just x
shorterName (Just y) x
  | (length . clientName2 $ x) <= (length . clientName2 $ y) = Just x
  | otherwise                                                = Just y

minimumClientFold :: [Client] -> Maybe Client
minimumClientFold xs = foldl shorterName Nothing xs

conjunctionFold :: [Bool] -> Bool
conjunctionFold = foldl (&&) True

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' p = let update x (yes,no) = if p x then (x:yes,no) else (yes,x:no)
               in foldr update ([], [])

find' :: (a -> Bool) -> [a] -> Maybe a
find' p []     = Nothing
find' p (x:xs) = if p x then Just x else find' p xs

-- OR

find'' :: (a -> Bool) -> [a] -> Maybe a
find'' p = listToMaybe . dropWhile (\x -> not (p x))

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p []         = []
dropWhile' p all@(x:xs) = if p x then dropWhile' p xs else all

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p []     = []
takeWhile' p (x:xs) = if p x then x : takeWhile' p xs else []

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' p [] = ([], [])
span' p all@(x:xs) = if p x then (x : taken, rest) else ([], all)
                     where (taken, rest) = span' p xs

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' p = span' (not . p)

any' :: (a -> Bool) -> [a] -> Bool
any' p = isJust . find' p

all' :: (a -> Bool) -> [a] -> Bool
all' p = isNothing . find' (not . p)

elem' :: (Eq a) => a -> [a] -> Bool
elem' x xs = isJust (find' (== x) xs)

-- this doesn't preserve the order
nub' :: (Eq a) => [a] -> [a]
nub' []     = []
nub' (x:xs) = if elem' x xs then nub' xs else x : nub' xs

-- this does, but also is slower
nub'' :: (Eq a) => [a] -> [a]
nub'' []     = []
nub'' (x:xs) = if elem' x xs then x : nub'' (filter (/= x) xs) else x : nub'' xs

nubBy' :: (a -> a -> Bool) -> [a] -> [a]
nubBy' c []     = []
nubBy' c (x:xs) = if isJust $ find' (c x) xs then nubBy' c xs else x : nubBy' c xs

--  - `insert` - adds one element to set if not exists
--  - `insertBy` - adds one element to set if not exists, using custom comparator

union' :: (Eq a) => [a] -> [a] -> [a]
union' l []  = l
union' [] l  = l
union' l1 l2 = l1 ++ (filter (\x -> not $ elem' x l1) l2)

intersect' :: (Eq a) => [a] -> [a] -> [a]
intersect' l1 l2 = filter (\x -> elem' x l1) l2

(\\\) :: (Eq a) => [a] -> [a] -> [a]
(\\\) l1 [] = l1
(\\\) l1 l2 = filter (\x -> not $ elem' x l2) l1

insert' :: (Ord a) => a -> [a] -> [a]
insert' e all@(x:xs)
  | e > x     = x : (insert' e xs)
  | otherwise = e : all

compare' :: (Ord a) => a -> a -> Ordering
compare' x y = if x < y then LT
               else if x > y then GT
               else EQ

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' c [] = []
groupBy' c xs = let x             = head xs
                    (xlike, rest) = span' (c x) xs
                in
                    (xlike) : groupBy' c rest

zip' :: [a] -> [b] -> [(a,b)]
zip' [] []         = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

delete' :: (Eq a) => a -> [a] -> [a]
delete' x xs = preX ++ postX
              where (preX, _:postX) = break' (== x) xs