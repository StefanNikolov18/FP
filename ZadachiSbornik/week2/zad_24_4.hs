--24.4. Едно положително цяло число е съвършено, ако е равно на сумата от
--своите делители (без самото число). Например, 6 е съвършено, защото
--6 = 1+2+3; числото 1 не е съвършено. Да се дефинира функция, коя-
--то създава списък с всички съвършени числа, ненадминаващи дадено
--положително цяло число в параметър n.

makeDivsList :: Int -> [Int]
makeDivsList 0 = []
makeDivsList n= makeDivsIterList 1 n
    where
        makeDivsIterList :: Int -> Int -> [Int]
        makeDivsIterList i n 
            | i >= n = []
            | n `rem` i == 0 =  i : makeDivsIterList (i+1) n
            | otherwise = makeDivsIterList (i+1) n

sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs

allPerfectlyToN :: Int -> [Int]
allPerfectlyToN 0 = []
allPerfectlyToN 1 = []
allPerfectlyToN n = isPerfectlyIter 2 n
    where
        isPerfectlyIter :: Int->Int->[Int]
        isPerfectlyIter i n
            | i >= n = []
            | isPerfectNumber i = i : isPerfectlyIter (i+1) n
            | otherwise = isPerfectlyIter (i+1) n
                where
                    isPerfectNumber :: Int -> Bool
                    isPerfectNumber k
                        | sum' (makeDivsList k) == k = True
                        | otherwise = False