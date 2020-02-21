{-# LANGUAGE FlexibleContexts #-}
module Chapter7.Transformers where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS

-- 7.5

factorialRun :: Integer -> Integer
factorialRun x = execState (execStateT factorial x) 1

factorial :: StateT Integer (State Integer) Integer
factorial = do counter <- get
               if counter == 1
                 then lift get -- return the inner state
                 else do
                   lift . modify $ (* counter) -- modify inner state
                   modify pred -- decrease counter
                   factorial

-- 7.6 honestly I have no idea how to do this

