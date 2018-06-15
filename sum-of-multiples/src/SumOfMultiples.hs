module SumOfMultiples (sumOfMultiples) where

import           Data.List

multiples :: Integer -> Integer -> [Integer]
multiples base limit =  map (*base) [1..(quot limit base)]

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples x y = sum . nub . concat $ map (`multiples` pred y) x
