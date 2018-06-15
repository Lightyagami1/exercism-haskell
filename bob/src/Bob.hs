module Bob (responseFor) where
import Data.Char (isSpace, isLower, isLetter)
import Data.List (isSuffixOf)

responseFor :: String -> String
{-
responseFor xs 
        | cap xs == True = "Whoa, chill out!"
        | isQues xs == True = "Sure."
        | xs == "Bob" = "Fine"
        | otherwise = "Whatever."
        where cap xs = if xs == map toUpper xs then test2 xs else False

test2 [] = False
test2 (x:xs) = if elem x ['A'..'Z'] == True then True else test2 xs

isQues [] = False
isQues (x:xs) = if x == '?' then True else isQues xs

qmIsNotlast xs = if last ( words xs ) /= ['?'] then True else False

-}

responseFor xs
        | isQuiet = "Fine. Be that way!"
        | shouting = "Whoa, chill out!"
        | question = "Sure."
        | otherwise = "Whatever."
        where
            isQuiet = all isSpace xs
            shouting = not ( any isLower xs ) && any isLetter xs
            question = "?" `isSuffixOf` last ( words xs )
