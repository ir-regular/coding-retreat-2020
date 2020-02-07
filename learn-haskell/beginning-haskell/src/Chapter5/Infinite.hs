module Chapter5.Infinite where

-- fibonacci numbers as infinite sequence

fibonacci :: [Int]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

fibonacci' :: [Int]
fibonacci' = map fst $ iterate (\(i,i_1) -> (i_1, i_1 + i)) (0,1)

-- test with: import Data.List; fibonacci !! 20

repeat' :: a -> [a]
repeat' x = x : repeat' x

cycle' :: [a] -> [a]
cycle' xs = xs ++ cycle' xs

iterate' :: (a -> a) -> a -> [a]
iterate' f x = iterate' f $ f x

take' :: Int -> [a] -> [a]
take' _ []     = []
take' n (x:xs) = x : take' (n - 1) xs

-- I tried to re-implement find and the comments in hackage took me
-- on a merry circle ride.
-- original code for find uses Data.Monoid.First but that's deprecated
-- for Data.Semigroup.First. This says to use Option (First) to replicate
-- Monoid.First, but Option says it's going to be deprecated and removed, because
-- Maybe was wrong, but is now fixed. Aaaargh.

find' :: (a -> Bool) -> [a] -> a
find' p = head . filter p

-- Ex. 5-1: Sieve of Eratosthenes

primes :: [Int]
primes = let composite_of x y = (y `mod` x) == 0
             sieve (x:xs)     = x : sieve (filter (not . composite_of x) xs)
         in sieve [2..]

head' 