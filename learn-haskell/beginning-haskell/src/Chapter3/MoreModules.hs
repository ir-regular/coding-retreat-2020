{-# LANGUAGE ViewPatterns #-}

module Chapter3.MoreModules where

import Data.List
import Chapter2.SimpleFunctions (Client(..), clientName2)

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
