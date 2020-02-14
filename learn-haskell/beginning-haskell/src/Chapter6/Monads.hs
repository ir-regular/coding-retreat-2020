{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Chapter6.Monads where

import Data.List
import qualified Data.Map as M
import Data.Default

-- "thenDo" combinator

type State s a = s -> (a, s)

thenDo :: State s a -> (a -> State s b) -> State s b
thenDo f g s = let (resultOfF, stateAfterF) = f s
               in g resultOfF stateAfterF

remain :: a -> (s -> (a,s))
remain x s = (x,s)

access :: (s -> a) -> (s -> (a,s))
access f s = (f s, s)

modify :: (s -> s) -> (s -> ((), s))
modify f s = ((), f s)

-- some base types for maths, copied from KMeans

class (Default v, Ord v) => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

instance Vector (Double, Double) where
  distance (a,b) (c,d) = sqrt $ (c - a) * (c - a) + (d - b) * (d - b)
  centroid xs = let (u,v) = foldr (\(a,b) (c,d) -> (a + c, b + d)) (0.0, 0.0) xs
                    n = fromIntegral $ length xs
                in (u / n, v / n)

class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vectorizable (Double,Double) (Double,Double) where
  toVector = id

-- actual components of KMeans

data KMeansState v = KMeansState { centroids :: [v]
                                 , threshold :: Double
                                 , steps :: Int }

newCentroids :: (Vector v, Vectorizable e v) => M.Map v [e] -> [v]
newCentroids = M.elems . fmap (centroid . map toVector)

clusterAssignments :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignments centrs points =
  let initialMap = M.fromList $ zip centrs (repeat [])
  in foldr (\p m -> let chosenCentroid = minimumBy (\x y -> compare (distance x $ toVector p)
                                                                    (distance y $ toVector p))
                                                    centrs
                    in M.adjust (p:) chosenCentroid m)
           initialMap points

-- v1:

--  kMeans' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState v) [v]
--  kMeans' points =
--    (\s -> (centroids s, s))                                           `thenDo` (\prevCentrs ->
--    (\s -> (clusterAssignments prevCentrs points, s))                  `thenDo` (\assignments ->
--    (\s -> (newCentroids assignments, s))                              `thenDo` (\newCentroids ->
--    (\s -> ((), s { centroids = newCentroids}))                        `thenDo` (\_ ->
--    (\s -> ((), s { steps = steps s + 1}))                             `thenDo` (\_ ->
--    (\s -> (threshold s, s))                                           `thenDo` (\t ->
--    (\s -> (sum $ zipWith distance prevCentrs newCentroids, s))        `thenDo` (\error ->
--    if error < t then (\s -> (newCentroids, s)) else kMeans' points)))))))

-- v2:

kMeans' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState v) [v]
kMeans' points =
  access centroids                                         `thenDo` (\prevCentrs  ->
  remain (clusterAssignments prevCentrs points)            `thenDo` (\assignments ->
  remain (newCentroids assignments)                        `thenDo` (\newCentrs   ->
  modify (\s -> s { centroids = newCentrs })               `thenDo` (\_           ->
  modify (\s -> s { steps = steps s + 1})                  `thenDo` (\_           ->
  access threshold                                         `thenDo` (\t           ->
  remain (sum $ zipWith distance prevCentrs newCentrs)     `thenDo` (\err         ->
  if err < t then remain newCentrs else kMeans' points)))))))

initialState :: (Vector v, Vectorizable e v)
             => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState v
initialState init k points threshold = KMeansState (init k points) threshold 0

kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeans init k points threshold = fst $ kMeans' points (initialState init k points threshold)

initializeSimple :: Int -> [e] -> [(Double,Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n-1) v

-- test with:
-- kMeans initializeSimple 2 ([(1,1),(1,2),(4,4),(4,5)]::[(Double,Double)]) 0.001
-- should return:
-- [(1.0,1.5),(4.0,4.5)]
