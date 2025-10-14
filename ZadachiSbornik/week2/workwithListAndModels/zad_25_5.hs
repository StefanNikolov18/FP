--25.5. Да се дефинира функция, която в даден низ замества всички малки
--латински букви със съответните им големи латински букви.

toUpper' :: String -> String
toUpper' [] = []
toUpper' str = toUpperHelper str []
    where
        toUpperHelper :: String -> String -> String
        toUpperHelper [] res = reverse res
        toUpperHelper (x : xs) res
            | isLower x = toUpperHelper xs (toUpperChar x : res)
            | otherwise = toUpperHelper xs (x : res)

toUpperChar :: Char -> Char
toUpperChar x = toEnum (fromEnum x - 32) 

isLower :: Char -> Bool
isLower x = x >= 'a' && x <= 'z'



