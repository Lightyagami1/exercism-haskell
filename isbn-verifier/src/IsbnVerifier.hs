module IsbnVerifier (isbn) where

import Data.Char (digitToInt, isDigit)
import Data.Maybe (mapMaybe)

{--
isbn :: String -> Bool
isbn = lenCheck . readIsbn

readIsbn :: String -> [Int]
readIsbn xs = mapMaybe f xs
        where f x | isDigit x = Just (digitToInt x)
              f 'X'           = Just 10
              f _             = Nothing

lenCheck :: [Int] -> Bool
lenCheck x 
  | length x == 10 && isbn10 x == True = True
  | length x == 13 && isbn13 x == True = True
  | otherwise                          = False

isbn10 :: [Int] -> Bool
isbn10 xs = divCheck $ sum (zipWith (*) [10,9..1] xs)

divCheck :: Int -> Bool
divCheck x = rem x 11 == 0

isbn13 :: [Int] -> Bool
isbn13 xs = True

--}

isbn :: String -> Bool
isbn code
  | length pruned /= 10           = False
  | any (not . isDigit) (init pruned) = False
  | (not . isDigit . last) code && (last code) /= 'X' = False
  | otherwise = sum (zipWith (*) numbers [10,9..1]) `rem` 11 == 0
  where
          pruned = filter (/= '-') code
          numbers = map readIsbn pruned

readIsbn :: Char -> Int
readIsbn x 
  | x == 'X' = 10
  | otherwise = digitToInt x
