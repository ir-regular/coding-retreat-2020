module Chapter4.BinaryTrees where

import Data.Function (on)

-- let's have this in a different module, so the price doesn't conflict with
-- Priceable

data TravelGuide = TravelGuide { title :: String, authors :: [String], price :: Integer }
                 deriving (Show, Eq)

instance Ord TravelGuide where
  compare t1 t2
    | on compare title t1 t2 == LT = LT
    | on compare title t1 t2 == GT = GT
    -- else they're both equal
    | otherwise = on compare authors t1 t2

data BinaryTree a = Node a (BinaryTree a) (BinaryTree a)
                | Leaf
                deriving Show

treeFind :: Ord a => a -> BinaryTree a -> Maybe a
treeFind _   Leaf                = Nothing
treeFind g1 (Node g2 left right) = case compare g1 g2 of
                                     EQ -> Just g2
                                     LT -> treeFind g1 left
                                     GT -> treeFind g1 right

treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert g     Leaf                = Node g Leaf Leaf
treeInsert g1 n@(Node g2 left right) = case compare g1 g2 of
                                         EQ -> n
                                         LT -> Node g2 (treeInsert g1 left) right
                                         GT -> Node g2 left (treeInsert g1 right)

treeConcat :: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
treeConcat t Leaf = t
treeConcat Leaf t = t
treeConcat t1 (Node x l r) = treeConcat (treeConcat (treeInsert x t1) l) r

-- alternative way to order travel guides (by price)

newtype TravelGuidePrice = TravelGuidePrice TravelGuide deriving (Eq, Show)

instance Ord TravelGuidePrice where
  (TravelGuidePrice (TravelGuide t1 a1 p1)) <= (TravelGuidePrice (TravelGuide t2 a2 p2)) =
    p1 < p2 || (p1 == p2 && (t1 < t2 || (t1 == t2 && a1 < a2)))

guides = [ TravelGuide "Europe" ["Adam Smith"] 100
         , TravelGuide "USA" ["Adam Smith"] 40
         , TravelGuide "Australia" ["Edna Smith"] 90
         , TravelGuide "Russia" ["Edna Smith"] 96
         , TravelGuide "Somalia" ["Edna Smith"] 90
         , TravelGuide "France" ["Edna Smith"] 50
         ]

guideTree = foldr (treeInsert . TravelGuidePrice) Leaf guides

-- cache for a minimum price

data BinaryTree3 v c = Node3 v c (BinaryTree3 v c) (BinaryTree3 v c)
                     | Leaf3
                    deriving (Show, Eq, Ord)

treeInsert3 :: (Ord v, Ord c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert3 v c (Node3 v2 c2 l r) = case compare v v2 of
                                      EQ -> Node3 v2 c2 l r
                                      LT -> Node3 v2 (min c c2) (treeInsert3 v c l) r
                                      GT -> Node3 v2 (min c c2) l (treeInsert3 v c r)
treeInsert3 v c Leaf3             = Node3 v c Leaf3 Leaf3

treeInsert4 :: (Ord v, Monoid c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert4 v c Leaf3 = Node3 v c Leaf3 Leaf3
treeInsert4 v c n@(Node3 v2 c2 l r) = case compare v v2 of
                                      EQ -> n
                                      LT -> let newLeft = treeInsert4 v c l
                                                newCache = c <> cached newLeft <> cached r
                                            in Node3 v2 newCache newLeft r
                                      GT -> let newRight = treeInsert4 v c r
                                                newCache = c <> cached newRight <> cached l
                                            in Node3 v2 newCache l newRight

cached :: Monoid c => BinaryTree3 v c -> c
cached Leaf3            = mempty
cached (Node3 _ c _ _) = c

-- Note: this is different than in the book, b/c <> was moved to Semigroup

newtype Min = Min Int deriving Show

instance Semigroup Min where
  (Min x) <> (Min y) = Min $ min x y

instance Monoid Min where
  mempty = Min (maxBound :: Int)

modifyTravelGuidePrice' :: Functor f => Integer -> f TravelGuide -> f TravelGuide
modifyTravelGuidePrice' m = fmap (\tg -> tg { price = m * price tg })

-- exercise 4.8, reimplement functors for Maybe and BinaryTree

newtype Maybe' a = Maybe' (Maybe a) deriving Show

instance Functor Maybe' where
  fmap f (Maybe' (Just x)) = Maybe' (Just (f x))
  fmap f (Maybe' Nothing)  = Maybe' Nothing

--  fmap succ (Maybe' (Just 1))

instance Functor BinaryTree where
  fmap f Leaf         = Leaf
  fmap f (Node v l r) = Node (f v) (fmap f l) (fmap f r)

-- test with:
-- fmap (\x -> let (TravelGuidePrice tg) = x in price tg) guideTree

instance Foldable Maybe' where
  foldr f i (Maybe' Nothing)  = i
  foldr f i (Maybe' (Just x)) = f x i

-- foldr (+) 0 (Maybe' (Just 4))

instance Foldable BinaryTree where
  foldr f i Leaf         = i
  foldr f i (Node v l r) = let i'  = f v i
                               i'' = foldr f i' l
                           in foldr f i'' r

-- foldr (\x tot -> let (TravelGuidePrice tg) = x in tot + price tg) 0 guideTree