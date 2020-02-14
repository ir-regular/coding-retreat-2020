module YAMT.Factorial where

import Control.Monad.State

factorial 0 = 1
factorial 1 = 1
factorial n = product [2..n]

type FactorialState = (Int, Int) -- iteration, result

factorialState :: Int -> State FactorialState Int
factorialState 0 = return 1
factorialState 1 = return 1
factorialState n = do (i, result) <- get
                      if i == n
                        then return result
                        else do put (i + 1, result * (i + 1))
                                factorialState n

runFactorial :: Int -> Int
runFactorial n = evalState (factorialState n) (2, 2)
