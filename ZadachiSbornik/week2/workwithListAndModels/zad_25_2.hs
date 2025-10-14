--25.2. Да се дефинира функция, която по два списъка намира дължината на
--най-дългия им общ префикс.

combinePrefixLength :: [Int] -> [Int] -> Int
combinePrefixLength _ [] = 0
combinePrefixLength [] _ = 0
combinePrefixLength (x : xs) (y : ys)
    | x == y = 1 + combinePrefixLength xs ys
    | otherwise = 0