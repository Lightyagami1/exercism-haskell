module CollatzConjecture (collatz) where

import           Data.List (unfoldr)
{--
c :: Int -> Integer -> Maybe Integer
c xs 0 = Nothing
c xs 1 = Just $ toInteger xs
c xs n = if even n then c (succ xs) (n `div` 2)
                   else c (succ xs) (3*n + 1)

--}

collatz :: Integer -> Maybe Integer
collatz n = Just $ toInteger $ length $ unfoldr c n
  where c n     | n <= 0 = Nothing
                | n == 1 = Just (1, 0)
                | even n = Just (n, n `div` 2)
                | otherwise = Just (n, 3*n+1)


