{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Chapter6.Monads2 where

import Data.List
import qualified Data.Map as M
import Data.Default
import Control.Monad.State

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

-- v3, rewrite with Control.Monad.State

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

kMeans' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState v) [v]
kMeans' points =
  do prevCentrs  <- gets centroids
     let assignments = clusterAssignments prevCentrs points
         newCentrs   = newCentroids assignments
     modify (\s -> s { centroids = newCentrs })
     modify (\s -> s { steps = steps s + 1})
     t <- gets threshold
     let err = sum $ zipWith distance prevCentrs newCentrs
     if err < t
       then return newCentrs
       else kMeans' points

initialState :: (Vector v, Vectorizable e v)
             => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState v
initialState init k points threshold = KMeansState (init k points) threshold 0

kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeans init k points threshold = evalState (kMeans' points) (initialState init k points threshold)

initializeSimple :: Int -> [e] -> [(Double,Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n-1) v

-- test with:
-- kMeans initializeSimple 2 ([(1,1),(1,2),(4,4),(4,5)]::[(Double,Double)]) 0.001
-- should return:
-- [(1.0,1.5),(4.0,4.5)]
