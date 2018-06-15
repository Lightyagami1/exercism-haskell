module Pangram (isPangram) where
import Data.Char (toUpper, isAlpha)
import Data.List (sort, group)

isPangram :: String -> Bool
--isPangram text = error "You need to implement this function."
isPangram text = length set == 26 
            where set = map head . group . sort $ filter isAlpha (map toUpper text)
