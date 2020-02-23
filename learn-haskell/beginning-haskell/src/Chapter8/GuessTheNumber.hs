{-# LANGUAGE FlexibleContexts #-}
module Chapter8.GuessTheNumber where

import System.Random
import Control.Monad.Reader
import Control.Monad.State
import Data.Default
import Text.Read
import Data.Maybe

data LotteryConfig = LotteryConfig { numberMin :: Int, numberMax :: Int, guesses :: Int }
data LotteryState = LotteryState { target :: Int, guessesLeft :: Int }

instance Default LotteryConfig where
  def = LotteryConfig { numberMin = 3, numberMax = 17, guesses = 5 }

guessNumber :: (MonadState LotteryState m, MonadReader LotteryConfig m, MonadIO m) => m Int
guessNumber = do min <- asks numberMin
                 max <- asks numberMax
                 rawGuess <- liftIO getLine
                 let guess = fromMaybe (min - 1) (readMaybe rawGuess)
                 if guess < min || guess > max
                 then do
                   liftIO $ putStrLn $ "Invalid guess! Try a number between " ++ show min ++ " and " ++ show max
                   guessNumber
                 else do
                   modify (\s -> s { guessesLeft = pred $ guessesLeft s })
                   return guess

lotteryTurn :: (MonadState LotteryState m, MonadReader LotteryConfig m, MonadIO m) => m ()
lotteryTurn = do number <- guessNumber
                 t <- gets target
                 gl <- gets guessesLeft
                 if number == t
                 then liftIO $ putStrLn "*** You guessed correctly! ***"
                 else if gl > 0
                 then do liftIO $ putStrLn $ "Wrong guess. Try again! Guesses left: " ++ show gl
                         lotteryTurn
                 else liftIO $ putStrLn "Wrong guess. No more guesses left!"

lottery :: (MonadReader LotteryConfig m, MonadIO m) => m ()
lottery = do min <- asks numberMin
             max <- asks numberMax
             liftIO . putStrLn $ "Guess a number between " ++ show min ++ " and " ++ show max
             n <- liftIO $ randomRIO (min, max)
             g <- asks guesses
             let initState = LotteryState { target = n, guessesLeft = g }
             evalStateT lotteryTurn initState
