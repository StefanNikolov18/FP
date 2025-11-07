
{-28.3. За списък от числа L да се намери списък с само с тези числа, които
съвпадат с поредния си номер в L. Например [1, 5, 3, 4, 2] → [1, 3, 4].-}

listInPos :: [Int] -> [Int]
listInPos = listInPosIter 1
    where
        listInPosIter :: Int -> [Int] -> [Int]
        listInPosIter _ [] = []
        listInPosIter index (x : xs)
            | index == x = x : listInPosIter (index + 1) xs 
            | otherwise = listInPosIter (index + 1) xs


eqPair :: Eq a => (a, a) -> Bool
eqPair (x,y) = x == y

takeFirst :: [(a,a)] -> [a]
takeFirst = map fst

--listInPosBetter ::  a =>[a] -> [a]
listInPosBetter :: (Eq a, Num a,Enum a) => [a] -> [a]
listInPosBetter list =  takeFirst $ filter eqPair (zip list [1..])