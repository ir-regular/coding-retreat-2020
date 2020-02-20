module Chapter7.List where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer

addPrefix :: String -> Reader String String
addPrefix s = ask >>= \p -> return $ p ++ s

addPrefixL :: [String] -> Reader String [String]
addPrefixL = mapM addPrefix

logInformation :: [String] -> Writer String ()
logInformation = mapM_ (\s -> tell (s ++ "\n"))

-- Try to write the definition of sequence.
-- do it using do notation and pattern matching.
-- as with any other list function, you should consider the cases of the empty
-- list and the list with some head and tail.
-- remember that x <- v “extracts” the value wrapped in a monad from v :: m a
-- into a binding x :: a.

-- liftM :: Monad m => m (a -> b) -> (m a -> m b)

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (x:xs) = do head <- x
                      tail <- sequence' xs
                      return $ head : tail

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f = sequence' . map f

foldM' :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM' f init []     = return init
foldM' f init (x:xs) = do tot <- foldM' f init xs
                          f tot x
