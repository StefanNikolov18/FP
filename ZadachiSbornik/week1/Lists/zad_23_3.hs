
--23.3. Да се реализира функция index x l, която намира поредния номер на
--първото срещане на елемента x в l. Например index 7 [1,2,7,3,2] ->
--3.

index :: Int -> [Int] -> Int
index = indexIter 0
    where 
        indexIter :: Int -> Int -> [Int] -> Int
        indexIter _ _ [] = -1
        indexIter i x (x' : xs)
            | x == x' = i
            | otherwise = indexIter (i+1) x xs


    