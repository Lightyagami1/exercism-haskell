module DNA (toRNA) where

rNAcompliment :: Char -> Char
rNAcompliment 'G' = 'C'
rNAcompliment 'C' = 'G'
rNAcompliment 'T' = 'A'
rNAcompliment 'A' = 'U'



toRNA :: String -> Maybe String
toRNA [] = Nothing
toRNA xs
  | any (`notElem` "GCTA") xs = Nothing
  | otherwise = Just $ map rNAcompliment xs
