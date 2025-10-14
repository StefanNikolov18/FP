--25.4. Да се дефинира функция pivot l x, която за списъца от числа l и числото
--x връща наредена двойка (l1, l2), където l1 е списък от елементите на l,
--по-малки от x, а l2 е списък от елементите на l, по-големи или равни на
--x.

type PairLists = ([Int],[Int])

pivot :: [Int] -> Int -> PairLists
pivot [] x = ([],[])
pivot l x = pivotHelper l x ([],[])
    where
        pivotHelper :: [Int] -> Int -> PairLists -> PairLists
        pivotHelper [] x (l1,l2) = (reverse l1, reverse l2)
        pivotHelper (x' : xs) x (l1,l2)
            | x' < x = pivotHelper xs x (x' : l1, l2)
            | otherwise = pivotHelper xs x (l1 , x' : l2)
