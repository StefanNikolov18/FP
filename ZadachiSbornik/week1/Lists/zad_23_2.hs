--23.2. Да се реализира функция count x l, която преброява колко пъти се
--среща елемента x в l.


myCount :: Int -> [Int] -> Int
myCount _ [] = 0
myCount x (x' : xs)
    | x == x' = 1 + myCount x xs
    | otherwise = myCount x xs
