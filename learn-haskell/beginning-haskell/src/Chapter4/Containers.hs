{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Chapter4.Containers where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Tree as T
import qualified Data.Foldable as DF
import Data.Function

-- copy client; forgot this earlier

data Client i = GovOrg { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                        , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Eq)

isGovOrg :: Client i -> Bool
isGovOrg GovOrg {..} = True
isGovOrg _ = False

isCompany :: Client i -> Bool
isCompany Company {..} = True
isCompany _ = False

isIndividual :: Client i -> Bool
isIndividual Individual {..} = True
isIndividual _ = False

data Person = Person { firstName :: String, lastName  :: String }
              deriving (Show, Eq, Ord)

data ClientKind = GovOrgKind | CompanyKind | IndividualKind
                  deriving (Show, Eq, Ord)

classifyClient :: Client i -> ClientKind
classifyClient GovOrg {..} = GovOrgKind
classifyClient Company {..} = CompanyKind
classifyClient Individual {..} = IndividualKind

class Nameable n where
  name :: n -> String

instance Nameable (Client i) where
  name Individual { person = Person { .. }} = firstName ++ " " ++ lastName
  name c = clientName c

instance (Ord i) => Ord (Client i) where
  compare x y
    | on compare name x y == LT = LT
    | on compare name x y == GT = GT
    -- otherwise, name is equal; check kind
    | isIndividual x && not (isIndividual y) = LT
    | isIndividual y && not (isIndividual y) = GT
    | isCompany    x && isGovOrg y = LT
    | isCompany    y && isGovOrg x = GT
    -- otherwise, kind is equal; check some other attribute
    | isCompany x = on compare duty x y
    | otherwise   = EQ

-- Exercise 4.2: reimplement insert, delete, and adjust using alter

insert' :: (Ord k) => k -> v -> M.Map k v -> M.Map k v
insert' k v = M.alter (\x -> Just v) k

delete' :: (Ord k) => k -> M.Map k v -> M.Map k v
delete' = M.alter (const Nothing)

adjust' :: (Ord k) => (v -> v) -> k -> M.Map k v -> M.Map k v
adjust' f = M.alter (\case Nothing -> Nothing
                           Just x  -> Just (f x))

-- exercise 4.3

-- traverse the list element by element, and perform on each element
-- the classification, decide which of the map items to modify, and then add
-- itself to the set

classifyClients :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients xs = let classify = \case GovOrg {..} -> GovOrgKind
                                          Company {..} -> CompanyKind
                                          Individual {..} -> IndividualKind
                         classifyClient m c = M.adjust (S.insert c) (classify c) m
                         results = M.fromList [(GovOrgKind, S.empty), (CompanyKind, S.empty), (IndividualKind, S.empty)]
                     in
                     foldl classifyClient results xs

-- the second should first create lists corresponding to the three different
-- kinds, and at the end convert those lists to sets and generate the mentioned
-- map from them

classifyClients' :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients' xs = let govOrgs = filter isGovOrg xs
                          companies = filter isCompany xs
                          individuals = filter isIndividual xs
                      in
                      M.fromList [(GovOrgKind, S.fromList govOrgs),
                                  (CompanyKind, S.fromList companies),
                                  (IndividualKind, S.fromList individuals)]

--  data Tree   a = Node { rootLabel :: a, subForest :: Forest a }
--  type Forest a = [Tree a]

preOrder :: (a -> b) -> T.Tree a -> [b]
preOrder f T.Node { .. } = foldl (\l t -> l ++ preOrder f t) [f rootLabel] subForest

postOrder :: (a -> b) -> T.Tree a -> [b]
postOrder f T.Node { .. } = foldl (\l t -> l ++ postOrder f t) [] subForest ++ [f rootLabel]

-- this will be more complex
--breadthFirst :: (a -> b) -> T.Tree a -> [b]

-- t = T.Node 1 [ T.Node 2 [ T.Node 3 [], T.Node 4 []], T.Node 5 [ T.Node 6 [] ]]

flatten' :: T.Tree a -> [a]
flatten' = preOrder id

flatten'' :: T.Tree a -> [a]
flatten'' = DF.foldr (:) []

levels' :: T.Tree a -> [[a]]
levels' (T.Node r [])            = [[r]]
levels' (T.Node r [T.Node s []]) = [[r], [s]]
levels' T.Node { .. }            = let c = map levels' subForest
                                   in [[rootLabel], concatMap head c, concatMap (!! 1) c]

class Priceable t where
  price :: t -> Integer

totalPrice :: Priceable p => [p] -> Integer
totalPrice = sum . map price