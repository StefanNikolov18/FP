--24.2. Да се дефинира функция, която по дадено естествено число n връща
--списък с цифрите му, четени отдясно на ляво.
invertedNumSetInList :: Int -> [Int] -> [Int]
invertedNumSetInList n l
    | n < 10 && not (inList (n `rem` 10) l) = l ++ [n]
    | n < 10 = l
    | n >= 10 && not (inList (n `rem` 10) l) = invertedNumSetInList (n `div` 10) (l ++ [n `rem` 10])
    | otherwise =  invertedNumSetInList (n `div` 10) l
        where
            inList :: Int ->[Int] -> Bool
            inList _ [] = False
            inList x (x' : xs) = x == x' ||  inList x xs