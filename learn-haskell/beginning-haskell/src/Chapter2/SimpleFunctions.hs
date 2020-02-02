{-# LANGUAGE ViewPatterns, NamedFieldPuns, RecordWildCards #-}

module Chapter2.SimpleFunctions where

import Data.Char (toUpper)

-- example from book
firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

-- concatenation: converted the example if/else into pattern matching
-- because l1 +++ [] would have scrolled through the whole l1
(+++) :: [a] -> [a] -> [a]
[] +++ l = l
l +++ [] = l
(x:xs) +++ l = x:(xs +++ l)

-- list reversing: converted the example into pattern matching
-- so that it automatically deconstructs head/tail
reverse2 :: [a] -> [a]
reverse2 []  = []
reverse2 [x] = [x]
reverse2 (x:xs) = reverse2 xs +++ [x]

maxmin :: (Num a, Ord a) => [a] -> (a, a)
maxmin [] = error "empty list"
maxmin [x] = (x, x)
maxmin (x:xs) = ( if x > xs_max then x else xs_max,
                  if x < xs_min then x else xs_min )
                where (xs_max, xs_min) = maxmin xs

-- Client is a data type
-- GovOrg, Company, Individual are constructors
data Client = GovOrg String
            | Company String Integer String String
            | Individual Person Bool
            deriving Show
-- data types and constructors have separate symbols tables: repeat it as convention
data Person = Person String String Gender
            deriving Show
data Gender = Male | Female | Other | Unknown
            deriving (Eq, Show)

-- exercise

-- price as integer in the lowest denomination because FUCK FLOATING POINTS FOR FINANCIAL DATA
type PriceInPence = Integer
type Manufacturer = String

data TimeMachine = TimeMachine TimeMachineModel PriceInPence
  deriving Show
-- manufacturer + model number + model name + past travel + future travel
data TimeMachineModel = TimeMachineModel Manufacturer Integer String Bool Bool
  deriving Show

-- example from the book

clientName :: Client -> String
clientName client = case client of
                    GovOrg name -> name
                    Company name _ _ _ -> name
                    Individual person _ ->
                      case person of Person fName lName _ -> fName ++ " " ++ lName

-- test it with: clientName2 (Individual (Person "Jack" "Smith" Male) False)

-- what I did with the example

clientName2 :: Client -> String
clientName2 (GovOrg name) = name
clientName2 (Company name _ _ _) = name
clientName2 (Individual (Person fName lName _) _) = fName ++ " " ++ lName

companyName :: Client -> Maybe String
companyName (Company name _ _ _) = Just name
companyName _ = Nothing

-- completely made up example to demo destructuring in let and where

announceCompany :: Client -> Maybe String
announceCompany c@(Company _ _ _ _) =
  let Just name = companyName c in
    Just ("Our newest customer is " ++ name ++ " (reg.no. " ++ (show number) ++ ")")
    where Company _ number _ _ = c
announceCompany _ = Nothing

-- Write a function that given a list of time machines, decreases their price
-- by some percentage. Use the TimeMachine data type you defined in the previous
-- set of exercises.

discount :: TimeMachine -> Integer -> TimeMachine
discount (TimeMachine model price) percent =
  let discount_price = price - ((price * percent) `div` 100) in
  TimeMachine model discount_price

-- For statistical purposes, write a function that returns the number of clients
-- of each gender. You may need to define an auxiliary data type to hold
-- the results of this function.

type CounterPair a = (a, Integer)
type Counter a = [CounterPair a]

getCount :: Eq a => Counter a -> a -> Integer
getCount [] _ = 0
getCount (x@(y',i):xs) y = if y' == y then i else getCount xs y

incrementCounter :: Eq a => Counter a -> a -> Counter a
incrementCounter [] y = [(y, 1)]
incrementCounter (x@(y',i):xs) y =
  if y' == y
  then (y',i+1):xs
  else x:(incrementCounter xs y)

clientGender :: Client -> Maybe Gender
clientGender (Individual (Person _ _ gender) _) = Just gender
clientGender _ = Nothing

clientGenderStats :: [Client] -> [(Gender, Integer)]
clientGenderStats clients =
  foldl (\ct c -> case (clientGender c) of
    Just gender -> incrementCounter ct gender
    Nothing -> ct) [] clients

-- test data:
-- clients = [(Individual (Person "Adam" "Smith" Male) True), (Individual (Person "Eve" "Smith" Female) False), (Individual (Person "Abe" "Lincoln" Male) False), (Individual (Person "EDI" "" Other) True), (GovOrg "NASA")]

-- challenge: can you redefine empty, head, tail?

empty' :: [a] -> Bool
empty' [] = True
empty' _ = False

head' :: [a] -> a
head' [] = error "empty list"
head' (x:xs) = x

tail' :: [a] -> [a]
tail' [] = error "empty list"
tail' (x:xs) = xs

-- example from book

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:xs@(y:_)) = x <= y && sorted xs

-- example from book

binom :: Integer -> Integer -> Integer
binom _ 0          = 1
binom n k | n == k = 1
binom n k          = (binom (n - 1) (k - 1)) + (binom (n - 1) k)

fibonacci :: Integer -> Maybe Integer
fibonacci n | n < 0     = Nothing
fibonacci 0             = Just 1
fibonacci 1             = Just 1
fibonacci n | otherwise = let Just n_1 = fibonacci (n - 1)
                              Just n_2 = fibonacci (n - 2) in
                            Just (n_1 + n_2)

-- example from book, I modified

multipleOf :: Integer -> Integer -> Bool
multipleOf x y = (x `mod` y) == 0

fizzBuzz :: Integer -> Maybe String
fizzBuzz n
  | multipleOf n 3 && multipleOf n 5 = Just "FizzBuzz"
  | multipleOf n 3                   = Just "Fizz"
  | multipleOf n 5                   = Just "Buzz"
  | otherwise                        = Nothing

-- Exercise 2.6

-- Define Ackermann function

ackermann :: Integer -> Integer -> Integer
ackermann m n
  | m == 0    = n + 1
  | n == 0    = ackermann (m - 1) 1
  | otherwise = ackermann (m - 1) (ackermann m (n - 1))

-- examples, ackermann 1 2 == 4 , ackermann 3 1 == 13 , ackermann 3 4 == 125

-- Define the unzip function, which takes a list of tuples and returns two lists,
-- one with all the first components and other one with the seconds.

unzip' :: [(a,b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((x,y):l) = (x:xs, y:ys)
                     where (xs, ys) = unzip' l

-- example from the book: ViewPattern

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _                 = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director") = True
specialClient _ = False

-- example from the book: record

data ClientR = GovOrgR { clientRName :: String }
             | CompanyR { clientRName :: String
                        , companyId :: Integer
                        , person :: PersonR
                        , duty :: String }
             | IndividualR { person:: PersonR }
             deriving Show

data PersonR = PersonR { firstName :: String
                       , lastName :: String
                       } deriving Show

-- example of GHC features

greet :: ClientR -> String
greet CompanyR { clientRName } = "Hello, " ++ clientRName
greet IndividualR { person = PersonR { .. }} = "Hi, " ++ firstName
greet GovOrgR {} = "Welcome"

-- example of applying pattern matching and destructuring to record fields

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR { firstName = initial:rest }) =
        let newName = (toUpper initial):rest
        in  p { firstName = newName }
nameInCapitals p@(PersonR { firstName = "" }) = p

-- exercise 2.7 - rewrite time machine records and functions

data TimeMachineR = TimeMachineR { model :: TimeMachineModelR
                                 , price :: PriceInPence
                                 }
                                 deriving Show
data TimeMachineModelR = TimeMachineModelR { manufacturer :: Manufacturer
                                           , modelNumber :: Integer
                                           , modelName :: String
                                           , travelsToPast :: Bool
                                           , travelsToFuture :: Bool
                                           }
                                           deriving Show

discount2 :: TimeMachineR -> Integer -> TimeMachineR
discount2 TimeMachineR { .. } percent =
  let discount_price = price - ((price * percent) `div` 100) in
  TimeMachineR { model = model, price = discount_price }
