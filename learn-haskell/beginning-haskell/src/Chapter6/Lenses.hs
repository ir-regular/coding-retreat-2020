{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Chapter6.Lenses where

import Control.Lens

--  -- repeat some definitions from an earlier chapter
--
--  data Client i = GovOrg     i String
--                | Company    i String Person String
--                | Individual i Person
--
--  data Person = Person String String
--
--  -- approach 1: write getters and setters by hand
--
--  firstName :: Lens' Person String
--  firstName = lens (\(Person f _) -> f)
--                   (\(Person _ l) newF -> Person newF l)
--
--  lastName :: Lens' Person String
--  lastName = lens (\(Person _ l) -> l)
--                  (\(Person f _) newL -> Person f newL)
--
--  -- approach 2, full lens
--  -- eg. you want to change from Client Int to Client Double
--
--  identifier :: Lens (Client i) (Client j) i j
--  identifier = lens (\case (GovOrg i _)      -> i
--                           (Company i _ _ _) -> i
--                           (Individual i _)  -> i)
--                    (\client newId -> case client of
--                        GovOrg _ n      -> GovOrg newId n
--                        Company _ n p r -> Company newId n p r
--                        Individual _ p  -> Individual newId p)

-- alternative, auto-create lenses by starting fields with an underscore
-- HEY HASKELL, PYTHON CALLED, IT WANTS ITS CONVENTIONS BACK

data Client i = GovOrg { _identifier :: i, _name :: String }
               | Company { _identifier :: i, _name :: String, _person :: Person, _duty :: String }
               | Individual { _identifier :: i, _person :: Person }
              deriving Show

data Person = Person { _firstName :: String, _lastName :: String }
               deriving Show

fullName :: Lens' Person String
fullName = lens (\(Person f l) -> f ++ " " ++ l)
                (\_ newFullName -> case words newFullName of
                                     f:l:_ -> Person f l
                                     _     -> error "Incorrect name")

makeLenses ''Client
makeLenses ''Person

-- test:
{-

let p = Person "John" "Smith" in (view firstName p, p^.lastName)
let c = Individual 3 (Person "John" "Smith") in view (person . lastName) c
let c = Individual 3 (Person "John" "Smith") in c^.person.fullName

Update:

let c = Individual 3 (Person "John" "Smith") in set identifier 4 c
let c = Individual 3 (Person "John" "Smith") in person.lastName .~ "Kox" $ c
let c = Individual 3 (Person "John" "Smith") in c & person.fullName .~ "Marianne Kox"
let c = Individual 3 (Person "John" "Smith") in c & identifier +~ 2

Generic application of a function

let c = Individual 3 (Person "John" "Smith") in c & over identifier (+2)
import Data.Char
let c = Individual 3 (Person "John" "Smith") in c & person.fullName %~ (map toUpper)

Library has lenses for tuples:

("a","b") & set _1 "c"

and lists:

"abc"^?_head
"abc"^?!_tail

and make modifications on lists

"abc" & (_head .~ 'd')
"abc" & (_tail %~ map toUpper)

or access "butlast" and "last"

"abc"^?_init
"abc" & (_last %~ toUpper)

or update each element of a Traversable

people & traversed.firstName %~ map toUpper
-}

-- exercise 6-2

type PriceInPence = Int
type Manufacturer = String

data TimeMachine = TimeMachine { _model :: TimeMachineModel
                               , _price :: PriceInPence
                               }
                               deriving Show
data TimeMachineModel = TimeMachineModel { _manufacturer :: Manufacturer
                                         , _modelNumber :: Int
                                         , _modelName :: String
                                         , _travelsToPast :: Bool
                                         , _travelsToFuture :: Bool
                                         }
                                         deriving Show

makeLenses ''TimeMachineModel
makeLenses ''TimeMachine

markup :: Int -> [TimeMachine] -> [TimeMachine]
markup percent tms = tms & traversed.price %~ (\x -> x * (100 + percent) `div` 100)
