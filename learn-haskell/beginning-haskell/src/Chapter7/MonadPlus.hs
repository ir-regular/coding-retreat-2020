module Chapter7.MonadPlus where

import Control.Monad.List

-- exercise 7.1

brokenJumps3 :: Int -> [Int]
brokenJumps3 year = do x1 <- [-1, 3, 5]
                       x2 <- [-1, 3, 5]
                       x3 <- [-1, 3, 5]
                       return $ year + x1 + x2 + x3

brokenJumps :: Int -> Int -> [Int]
brokenJumps 0 year = [year]
brokenJumps n year = do x <- [-1, 3, 5]
                        y <- brokenJumps (n - 1) year
                        return $ x + y

-- exercise 7.2

find_ :: (a -> Bool) -> [a] -> Maybe a
find_ pred = msum . fmap isMatch
             where isMatch x = do guard (pred x)
                                  return x
