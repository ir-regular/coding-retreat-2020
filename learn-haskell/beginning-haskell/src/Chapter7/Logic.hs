module Chapter7.Logic where

import Control.Monad.List
import Control.Monad.Logic

-- list monad: graph search (cannot deal with cyclic graphs)

paths :: [(Int,Int)] -> Int -> Int -> [[Int]]
paths edges start end =
  let e_paths = do
      (e_start, e_end) <- edges
      guard $ e_start == start
      subpath <- paths edges e_end end
      return $ start : subpath
  in if start == end
        then return [end] `mplus` e_paths
        else e_paths

-- test with:
-- graph1 = [(2013,501),(2013,1004),(501,2558),(1004,2558)]
-- paths graph1 2013 2558


-- The list of paths between year 2013 and 2558 is infinite (you can always
-- loop once more between years 501 and 1004). (See graph2, below)

-- If you use a List monad, the path [2013,2558] will never be found, because
-- the search strategy embodied in the list monad will first need to find all
-- subpaths with go through year 501 (because it’s found before in the list
-- of edges).

-- v1, start using Logic

--  pathsL :: [(Int,Int)] -> Int -> Int -> Logic [Int]
--  pathsL edges start end =
--    let e_paths = do
--            (e_start, e_end) <- choices edges
--            guard $ e_start == start
--            tail <- pathsL edges e_end end
--            return $ e_start:tail
--    in if start == end
--          then return [end] `mplus` e_paths
--          else e_paths

-- v2, desugar to prepare for using >>-

--  pathsL :: [(Int,Int)] -> Int -> Int -> Logic [Int]
--  pathsL edges start end =
--    let e_paths =
--          choices edges >>= \(e_start, e_end) ->
--          guard (e_start == start) >>
--          pathsL edges e_end end >>= \tail ->
--          return (e_start : tail)
--    in if start == end
--          then return [end] `mplus` e_paths
--          else e_paths

-- v3, using >>- and interleave

pathsL :: [(Int,Int)] -> Int -> Int -> Logic [Int]
pathsL edges start end =
  let e_paths =
        choices edges >>- \(e_start, e_end) ->
        guard (e_start == start) >>
        pathsL edges e_end end >>- \tail ->
        return (e_start : tail)
  in if start == end
        then return [end] `interleave` e_paths
        else e_paths

--  taken from the article “Adventures in Three Monads” by Edward Z. Yang
-- turns a list into a set of successes
choices :: [a] -> Logic a
choices = msum . map return

-- test with:
-- graph2 = [(2013,501),(501,2558),(501,1004),(1004,501),(2013,2558)]
-- observeMany 3 $ pathsL graph2 2013 2558
