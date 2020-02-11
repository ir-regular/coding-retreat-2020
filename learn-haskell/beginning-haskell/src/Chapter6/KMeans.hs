{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, RecordWildCards #-}

module Chapter6.KMeans where

import Data.List
import qualified Data.Map as M
import Data.Default
import Control.Lens

-- allows for vectors to be tuples of varying dimensions
-- Ord: allow vectors to be map keys
-- Default: provide default values

class (Default v, Ord v) => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

instance Vector (Double, Double) where
  distance (a,b) (c,d) = sqrt $ (c - a) * (c - a) + (d - b) * (d - b)
  centroid xs = let (u,v) = foldr (\(a,b) (c,d) -> (a + c, b + d)) (0.0, 0.0) xs
                    n = fromIntegral $ length xs
                in (u / n, v / n)

-- this seems to serve as a constructor

class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vectorizable (Double,Double) (Double,Double) where
  toVector = id

clusterAssignmentPhase :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
      closerTo p x y = compare (distance x $ toVector p) (distance y $ toVector p)
      closestOf p = minimumBy (closerTo p)
      addToCentroid p = M.adjust (p:) (p `closestOf` centroids)
  in
    foldr addToCentroid initialMap points

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v,v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

shouldStop :: (Vector v) => [(v,v)] -> Double -> Bool
shouldStop centroidDiffs threshold = diffSum < threshold
  where diffSum = foldr (\(old,new) sum -> sum + distance old new) 0.0 centroidDiffs

-- first arg: a way to generate the initial vectors
-- second arg: how many centroids to generate
-- third arg: data points
-- fourth arg: threshold - stop iterating if centroids are this close
-- result: final centroids for each cluster
kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> (Int, [v])
kMeans init k points threshold = kMeans' (init k points) points threshold 1

kMeans' :: (Vector v, Vectorizable e v) => [v] -> [e] -> Double -> Int -> (Int, [v])
kMeans' centroids points threshold steps =
  let clusters = clusterAssignmentPhase centroids points
      centroidDiffs = newCentroidPhase clusters
      newCentroids = map snd centroidDiffs
  in
      if shouldStop centroidDiffs threshold
      then (steps, newCentroids)
      else kMeans' newCentroids points threshold (steps + 1)

-- example function that generates n initial centroids from a list of data points

initializeSimple :: Int -> [e] -> [(Double,Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n-1) v

-- test with:
-- kMeans initializeSimple 2 ([(1,1),(1,2),(4,4),(4,5)]::[(Double,Double)]) 0.001

data KMeansState e v = KMeansState { _centroids :: [v]
                                   , _points :: [e]
                                   , _err :: Double
                                   , _threshold :: Double
                                   , _steps :: Int
                                   }

makeLenses ''KMeansState

-- the derivation of lenses via template haskell must appear before any use of them in other code

initializeState :: (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState e v
initializeState init k points threshold = KMeansState (init k points) points 0.0 threshold 0

kMeansL :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeansL i k points threshold = view centroids $ kMeansL' (initializeState i k points threshold)

kMeansL' :: (Vector v, Vectorizable e v) => KMeansState e v -> KMeansState e v
kMeansL' state =
  let assignments = clusterAssignmentsL state
      state1 = state  & centroids.traversed
                      %~ (\c -> centroid $ toVector <$> M.findWithDefault [] c assignments) -- fmap == <$>
      state2 = state1 & err .~ sum (zipWith distance (state^.centroids) (state1^.centroids))
      state3 = state2 & steps +~ 1
  in
      if state3^.err < state3^.threshold then state3 else kMeansL' state3

-- NOTE: this is really copy-pasta from the above, so I'm not sure whether this
-- fits the goal of the exercise. Was this expected to use state more?

clusterAssignmentsL :: (Vector v, Vectorizable e v) => KMeansState e v -> M.Map v [e]
clusterAssignmentsL state =
  let initialMap = M.fromList $ zip (state^.centroids) (repeat [])
      closerTo p x y = compare (distance x $ toVector p) (distance y $ toVector p)
      closestOf p = minimumBy (closerTo p)
      addToCentroid p = M.adjust (p:) (p `closestOf` (state^.centroids))
  in
    foldr addToCentroid initialMap (state^.points)
