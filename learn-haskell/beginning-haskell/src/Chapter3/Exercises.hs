{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Chapter3.Exercises where

import Chapter2.SimpleFunctions (ClientR(..), PersonR(..))

-- example from the book

maybeString :: Maybe t -> String
maybeString (Just _) = "Just"
maybeString Nothing  = "Nothing"

-- exercise 3.1: figure out signatures of these functions, parametrised

swapTriple :: (a, b, c) -> (b, c, a)
swapTriple (x,y,z) = (y,z,x)

duplicate :: a -> (a, a)
duplicate x = (x,x)

nothing :: a -> Maybe b
nothing _ = Nothing

-- ghci says: Num a => [b] -> [(a, b)] but n will always be Integer >= 0
index :: [a] -> [(Integer, a)]
index []     = []
index [x]    = [(0,x)]
index (x:xs) = let indexed@((n,_):_) = index xs
              in  (n+1,x):indexed

maybeA :: [a] -> Char
maybeA [] = 'a'

-- exercise 3.2

filterOnes :: Integral a => [a] -> [a]
filterOnes xs = filter (\x -> x == 1) xs

filterANumber :: Integral a => a -> [a] -> [a]
filterANumber n xs = filter (\x -> x == n) xs

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot p xs = filter (\x -> not $ p x) xs

isGovOrg :: ClientR -> Bool
isGovOrg GovOrgR { .. } = True
isGovOrg _ = False

filterGovOrgs :: [ClientR] -> [ClientR]
filterGovOrgs xs = filter isGovOrg xs

filterGovOrgs' :: [ClientR] -> [ClientR]
filterGovOrgs' xs = filter (\case GovOrgR _ -> True
                                  _         -> False
                           ) xs

-- I am strongly unconvinced by the example given of the point-free style.
-- This formula: 3x + 7(x + 2) would normally be:
--    3 * x + 7 * (x + 2)
-- But no, let's rewrite it!
--    uncurry (+) . ( ((*7) . (+2)) *** (*3) ) . duplicate

-- So, like, I *know* why this is valuable, but the example makes me froth.

