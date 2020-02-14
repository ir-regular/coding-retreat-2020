module YAMT.Fibonacci where

import Control.Monad.State

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

type FibState = (Int, Int, Int) -- (countdown, current iteration value, previous iteration value)

fibState :: State FibState Int
fibState = do (i, n_1, n_2) <- get
              if i == 0
                then return n_2
                else do put (i - 1, n_1 + n_2, n_1)
                        fibState

runFib :: Int -> Int
runFib n = evalState fibState (n, 1, 0)
