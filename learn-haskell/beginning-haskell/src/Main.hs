module Main where

import Chapter8.GuessTheNumber
import Control.Monad.Reader
import Data.Default

main :: IO ()
main = runReaderT lottery def
