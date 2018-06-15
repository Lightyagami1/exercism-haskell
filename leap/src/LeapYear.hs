module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
{- isLeapYear year 
        | (year `mod` 100 == 0) && (year `mod` 400 == 0) = True
        | (year `mod` 4 == 0) && (year `mod` 100 /= 0) = True
        | otherwise = False
-}
isLeapYear year
        | divs 100 && not( divs 400) = False
        | divs 4 = True
        | otherwise = False
        where divs x = year `mod` x == 0
