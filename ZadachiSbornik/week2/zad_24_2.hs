--24.2. Да се дефинира функция, която по дадено естествено число n връща
--списък с цифрите му, четени отдясно на ляво.
invertedNumInList :: Int -> [Int]
invertedNumInList n
    | n < 10 = [n]
    | otherwise = (n `rem` 10) : invertedNumInList(n `div` 10)
